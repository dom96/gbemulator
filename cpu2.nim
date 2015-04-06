import mem2, unsigned, os, strutils, opcodes, parseutils, utils
type
  CPU = ref object
    ## Registers
    pc, sp: uint16
    a, b, c, d, e, h, l, f: uint8
    mem: Memory

  OperandKind = enum
    Immediate, Address, Register, RegisterCombo, IntLit

  Operand = ref object
    name: string
    case kind: OperandKind
    of Register:
      location: pointer
      negated: bool ## Only used for conditional operands
      flagMask: uint8
    of RegisterCombo:
      first, second: pointer
    of Immediate:
      signed: bool
      pcIncremented: bool ## To prevent PC from being incremented multiple times
    of Address:
      op: Operand ## Operand holding the address
      decrement: bool ## (HL-)
      increment: bool ## (HL+)
    of IntLit:
      num: range[0 .. 7]
    word: bool ## Determines whether location points to a uint8 (false) or uint16 (true).


  Operands = tuple[dest, src: Operand]

proc newCPU(mem: Memory): CPU =
  new result
  result.mem = mem
  result.pc = 0

proc get16(cpu: CPU, op: Operand): uint16
proc get8(cpu: CPU, op: Operand): uint8 =
  ## If ``op`` is Immediate this will increase PC!!!
  assert(not op.word)
  case op.kind
  of Register:
    return cast[ptr uint8](op.location)[]
  of RegisterCombo:
    assert false, "Cannot return uint16 in get8 for RegisterCombo."
  of Address:
    var address: uint16
    if not op.op.word:
      case op.op.name
      of "C", "A8":
        address = 0xFF00'u16 + cpu.get8(op.op).uint16
      else:
        assert false, "Unknown byte-sized address register " & op.op.name
    else:
      address = cpu.get16(op.op)
    return cpu.mem.read8(address)
  of IntLit:
    assert false
  of Immediate:
    result = cpu.mem.read8(cpu.pc - 1)

proc get16(cpu: CPU, op: Operand): uint16 =
  ## If ``op`` is Immediate this will increase PC!!!
  assert(op.word)
  case op.kind
  of Register:
    return cast[ptr uint16](op.location)[]
  of RegisterCombo:
    return (cast[ptr uint8](op.first)[].uint16 shl 8) or
            cast[ptr uint8](op.second)[].uint16
  of Address:
    assert false
  of IntLit:
    assert false
  of Immediate:
    result = cpu.mem.read16(cpu.pc - 2)

proc set(cpu: CPU, op: Operand, value: uint16) =
  assert op.word
  case op.kind
  of Register:
    let old = cast[ptr uint16](op.location)[].toHex()
    echo("Register $1: 0x$2 -> 0x$3" %
          [op.name, old, value.toHex()])
    cast[ptr uint16](op.location)[] = value
  of RegisterCombo:
    let old = cast[ptr uint8](op.first)[].toHex() &
              cast[ptr uint8](op.second)[].toHex()
    echo("Register $1: 0x$2 -> 0x$3" %
          [op.name, old, value.toHex()])
    cast[ptr uint8](op.first)[] = (value shr 8).uint8
    cast[ptr uint8](op.second)[] = cast[uint8](value)
  of Address:
    assert false
  of IntLit:
    assert false
  of Immediate:
    assert false, "Cannot set immediate."

proc set(cpu: CPU, op: Operand, value: uint8) =
  assert(not op.word)
  case op.kind
  of Register:
    let old = cast[ptr uint8](op.location)[].toHex()
    echo("Register $1: 0x$2 -> 0x$3" %
          [op.name, old, value.toHex()])
    cast[ptr uint8](op.location)[] = value
  of RegisterCombo:
    assert false, "Need 16 bits for two registers"
  of Address:
    var address: uint16
    if not op.op.word:
      case op.op.name
      of "C", "A8":
        address = 0xFF00'u16 + cpu.get8(op.op).uint16
      else:
        assert false, "Unknown byte-sized address register " & op.op.name
    else:
      address = cpu.get16(op.op)
    let old = cpu.mem.read8(address)
    cpu.mem.write8(address, value)

    # Check whether we should decrement/increment
    if op.decrement:
      cpu.set(op.op, cpu.get16(op.op)-1)
    elif op.increment:
      cpu.set(op.op, cpu.get16(op.op)+1)
  of IntLit:
    assert false
  of Immediate:
    assert false, "Cannot set immediate."

proc isTrue(cpu: CPU, op: Operand): bool =
  case op.kind
  of Register:
    assert op.flagMask != 0
    result = cpu.f.isFlagSet(op.flagMask)
    if op.negated: result = not result
  else:
    assert false

proc add(cpu: CPU, op: Operand, y: uint16): uint16 =
  ## This should only be used in JR.

  assert op.kind == Immediate

  if op.word:
    let data = cpu.get16(op)
    if op.signed:
      return uint16(cast[int16](data) + y.int64)
    else:
      return data + y
  else:
    let data = cpu.get8(op)
    if op.signed:
      return uint16(cast[int8](data) + y.int64)
    else:
      return data.uint16 + y

proc createRegOperand(name: string, reg: ptr uint8,
                      neg = false, flagMask: uint8 = 0): Operand =
  Operand(kind: Register, name: name, location: reg, word: false,
          negated: neg, flagMask: flagMask)

proc createRegOperand(name: string, reg: ptr uint16): Operand =
  Operand(kind: Register, name: name, location: reg, word: true)

proc createRegComboOperand(name: string, first, second: ptr uint8): Operand =
  Operand(kind: RegisterCombo, name: name, first: first,
          second: second, word: true)

proc registerF(cpu: CPU): Operand =
  createRegOperand("F", addr cpu.f)

proc verifyFlags(cpu: CPU, opc: Opcode, prevFlags: uint8) =
  let flags = cpu.get8(registerF(cpu))
  if opc.z == '1': assert((flags and 0b10000000) == 0b10000000)
  elif opc.z == '0': assert((flags and 0b10000000) == 0)
  elif opc.z == '-': assert(isFlagSet(flags, BitZ) == isFlagSet(prevFlags, BitZ))

  if opc.n == '1': assert((flags and 0b01000000) == 0b01000000)
  elif opc.n == '0': assert((flags and 0b01000000) == 0)
  elif opc.n == '-': assert(isFlagSet(flags, BitN) == isFlagSet(prevFlags, BitN))

  if opc.h == '1': assert((flags and 0b00100000) == 0b00100000)
  elif opc.h == '0': assert((flags and 0b00100000) == 0)
  elif opc.h == '-': assert(isFlagSet(flags, BitH) == isFlagSet(prevFlags, BitH))

  if opc.c == '1': assert((flags and 0b00010000) == 0b00010000)
  elif opc.c == '0': assert((flags and 0b00010000) == 0)
  elif opc.c == '-': assert(isFlagSet(flags, BitC) == isFlagSet(prevFlags, BitC))

proc parseOperand(cpu: CPU, encoded: string): Operand =
  case encoded
  of "A":
    result = createRegOperand(encoded, addr cpu.a)
  of "B":
    result = createRegOperand(encoded, addr cpu.b)
  of "C":
    result = createRegOperand(encoded, addr cpu.c)
  of "D":
    result = createRegOperand(encoded, addr cpu.d)
  of "E":
    result = createRegOperand(encoded, addr cpu.e)
  of "H":
    result = createRegOperand(encoded, addr cpu.h)
  of "L":
    result = createRegOperand(encoded, addr cpu.l)
  of "F":
    result = createRegOperand(encoded, addr cpu.f)
  of "HL":
    result = createRegComboOperand(encoded, addr cpu.h, addr cpu.l)
  of "SP":
    result = createRegOperand(encoded, addr cpu.sp)
  of "DE":
    result = createRegComboOperand(encoded, addr cpu.d, addr cpu.e)
  of "BC":
    result = createRegComboOperand(encoded, addr cpu.b, addr cpu.c)
  of "AF":
    result = createRegComboOperand(encoded, addr cpu.a, addr cpu.f)
  of "NC":
    result = createRegOperand(encoded, addr cpu.f, true, BitC)
  of "NZ":
    result = createRegOperand(encoded, addr cpu.f, true, BitZ)
  of "Z":
    result = createRegOperand(encoded, addr cpu.f, false, BitZ)
  of "D16":
    result = Operand(name: encoded, kind: Immediate, word: true)
    cpu.pc.inc 2
  of "D8":
    result = Operand(name: encoded, kind: Immediate, word: false)
    cpu.pc.inc 1
  of "R8":
    result = Operand(name: encoded, kind: Immediate, signed: true, word: false)
    cpu.pc.inc 1
  of "A8":
    result = Operand(name: encoded, kind: Immediate, signed: false, word: false)
    cpu.pc.inc 1
  of "A16":
    result = Operand(name: encoded, kind: Immediate, signed: false, word: true)
    cpu.pc.inc 2
  of "0", "1", "2", "3", "4", "5", "6", "7":
    result = Operand(name: encoded, kind: IntLit, num: encoded.parseInt())
  else:
    assert false, "Unknown operand: " & encoded.toUpper()

proc parseOperand(cpu: CPU, opc: Opcode, i: var int): Operand =
  var encoded = ""
  i.inc parseUntil(opc.mnemonic, encoded, {',', '\0'}, i)

  if encoded[0] == '(':
    var onlyRegister = ""
    discard parseUntil(encoded, onlyRegister, {'+', '-', ')'}, 1)
    let op = parseOperand(cpu, onlyRegister.toUpper())
    let decrement = encoded[^2] == '-'
    let increment = encoded[^2] == '+'
    result = Operand(name: encoded, kind: Address, op: op, increment: increment,
                     decrement: decrement)

  if opc.mnemonic[i] == ')': i.inc
  if opc.mnemonic[i] == ',': i.inc

  if result.isNil:
    result = parseOperand(cpu, encoded.toUpper())

proc parseOperands(cpu: CPU, opc: Opcode, i: var int): Operands =
  result.dest = parseOperand(cpu, opc, i)
  result.src = parseOperand(cpu, opc, i)

proc execLoad(cpu: CPU, opc: Opcode, i: var int) =
  let (dest, src) = parseOperands(cpu, opc, i)

  if src.word:
    cpu.set(dest, cpu.get16(src))
  else:
    cpu.set(dest, cpu.get8(src))

proc execXOR(cpu: CPU, opc: Opcode, i: var int) =
  let src = parseOperand(cpu, opc, i)

  # Destination for XOR is always register A.
  let dest = createRegOperand("A", addr cpu.a)

  assert(not src.word)
  let value = cpu.get8(dest) xor cpu.get8(src)
  cpu.set(dest, value)

  let flags = registerF(cpu)
  # Only Z flag is affected.
  let zero: uint8 =
    if value == 0: 0b10000000
    else: 0
  cpu.set(flags, zero)

proc execJr(cpu: CPU, opc: Opcode, i: var int) =
  let opOne = parseOperand(cpu, opc, i)

  if opc.mnemonic[i] != '\0':
    let (cond, displacement) = (opOne, parseOperand(cpu, opc, i))
    if cpu.isTrue(cond):
      let newPc = cpu.add(displacement, cpu.pc)
      echo("PC: 0x$1 -> 0x$2" % [cpu.pc.toHex(), newPc.toHex()])
      cpu.pc = newPc
  else:
    let newPc = cpu.add(opOne, cpu.pc)
    echo("PC: 0x$1 -> 0x$2" % [cpu.pc.toHex(), newPc.toHex()])
    cpu.pc = newPc

proc execInc(cpu: CPU, opc: Opcode, i: var int) =
  let reg = parseOperand(cpu, opc, i)
  if not reg.word:
    let value = cpu.get8(reg)
    let newValue = value+1
    cpu.set(reg, newValue)

    # Check for half carry.
    let halfCarry = (((value and 0xF) + (1'u8 and 0xF)) and 0x10) == 0x10
    let zero = newValue == 0
    let flags = registerF(cpu)
    let flagsValue = cpu.get8(flags)
    cpu.set(flags, flagsValue.changeFlags(z = >>zero, n=FUnset, h = >>halfCarry))
  else:
    # Flags are not affected for 16bit INC.
    let value = cpu.get16(reg)
    cpu.set(reg, value+1)

proc execDec(cpu: CPU, opc: Opcode, i: var int) =
  let reg = parseOperand(cpu, opc, i)
  if not reg.word:
    let value = cpu.get8(reg)
    let newValue = value-1
    cpu.set(reg, newValue)

    # Check for half carry.
    let halfCarry = (((value and 0xF) - (1'u8 and 0xF)) and 0x10) == 0x10
    let zero = newValue == 0
    let flags = registerF(cpu)
    let flagsValue = cpu.get8(flags)
    cpu.set(flags, flagsValue.changeFlags(z = >>zero, n=FSet, h = >>halfCarry))
  else:
    # Flags are not affected for 16bit INC.
    let value = cpu.get16(reg)
    cpu.set(reg, value-1)

proc execCall(cpu: CPU, opc: Opcode, i: var int) =
  let location = parseOperand(cpu, opc, i)

  # Push PC onto Stack.
  cpu.sp.dec
  cpu.mem.write8(cpu.sp, uint8(cpu.pc shr 8))
  cpu.sp.dec
  cpu.mem.write8(cpu.sp, uint8((cpu.pc shl 8) shr 8))

  let newPc = cpu.get16(location)
  echo("PC: 0x$1 -> 0x$2" % [cpu.pc.toHex(), newPc.toHex()])
  cpu.pc = newPc

proc execPush(cpu: CPU, opc: Opcode, i: var int) =
  let valueOp = parseOperand(cpu, opc, i)
  let value = cpu.get16(valueOp)

  cpu.sp.dec
  cpu.mem.write8(cpu.sp, uint8(value shr 8))
  cpu.sp.dec
  cpu.mem.write8(cpu.sp, uint8((value shl 8) shr 8))

proc execPop(cpu: CPU, opc: Opcode, i: var int) =
  let reg = parseOperand(cpu, opc, i)

  let low = cpu.mem.read8(cpu.sp)
  cpu.sp.inc
  let high = cpu.mem.read8(cpu.sp)
  cpu.sp.inc

  cpu.set(reg, (uint16(high) shl 8) or uint16(low))

proc execRet(cpu: CPU, opc: Opcode, i: var int) =
  let low = cpu.mem.read8(cpu.sp)
  cpu.sp.inc
  let high = cpu.mem.read8(cpu.sp)
  cpu.sp.inc

  let newPc = (uint16(high) shl 8) or uint16(low)
  echo("PC: 0x$1 -> 0x$2" % [cpu.pc.toHex(), newPc.toHex()])
  cpu.pc =  newPc

proc execSubCp(cpu: CPU, opc: Opcode, i: var int) =
  let reg = parseOperand(cpu, opc, i)

  let accumulator = createRegOperand("A", addr cpu.a)
  let aValue = cpu.get8(accumulator)

  let value = cpu.get8(reg)
  let newValue = aValue - value

  if opc.mnemonic == "SUB":
    cpu.set(reg, newValue)

  # Check for half carry.
  let halfCarry = (((aValue and 0xF) - (value and 0xF)) and 0x10) == 0x10
  let carry = aValue < value
  let zero = newValue == 0
  let flags = registerF(cpu)
  let flagsValue = cpu.get8(flags)
  cpu.set(flags,
      flagsValue.changeFlags(z = >>zero, n=FSet, h = >>halfCarry, c = >>carry))

# ---- Prefix CB opcodes follow.

proc execBit(cpu: CPU, opc: Opcode, i: var int) =
  let (bitNum, src) = parseOperands(cpu, opc, i)
  assert bitNum.kind == IntLit

  # TODO: This may be wrong.
  let srcVal = cpu.get8(src)
  let zero = (srcVal and (1.uint8 shl bitNum.num)) == 0
  let flags = registerF(cpu)
  let flagsValue = cpu.get8(flags)
  cpu.set(flags, flagsValue.changeFlags(>>zero, FUnset, FSet))

proc execRL(cpu: CPU, opc: Opcode, i: var int) =
  let reg =
    if opc.mnemonic == "RLA": createRegOperand("A", addr cpu.a)
    else: parseOperand(cpu, opc, i)

  var value = cpu.get8(reg)
  let bit7Set = (value and 0x80) == 0x80
  value = value shl 1

  let flags = cpu.get8(registerF(cpu))
  if flags.isFlagSet(BitC):
    value = value or 1
  else:
    value = value and (not 1'u8)

  cpu.set(reg, value)

  var z = >>(value == 0)
  if opc.mnemonic == "RLA": z = FUnchanged

  cpu.set(registerF(cpu),
      flags.changeFlags(z = z, n = FUnset,
                        h = FUnset, c = >>bit7Set))

proc parseMnemonic(cpu: CPU, opc: Opcode)
proc execPrefix(cpu: CPU, opc: Opcode) =
  let prevFlags = cpu.f
  let prefixOpc = prefixOpcs[cpu.mem.read8(cpu.pc)]
  parseMnemonic(cpu, prefixOpc)
  verifyFlags(cpu, prefixOpc, prevFlags)

proc parseMnemonic(cpu: CPU, opc: Opcode) =
  var i = 0
  var opcType = ""
  i.inc parseUntil(opc.mnemonic, opcType, Whitespace + {'\0'}, i)

  if opc.mnemonic[i] in Whitespace:
    i.inc # Skip whitespace

  cpu.pc.inc
  case opcType.toUpper()
  of "LD", "LDH":
    execLoad(cpu, opc, i)
  of "XOR":
    execXOR(cpu, opc, i)
  of "PREFIX":
    execPrefix(cpu, opc)
  of "JR":
    execJr(cpu, opc, i)
  of "INC":
    execInc(cpu, opc, i)
  of "DEC":
    execDec(cpu, opc, i)
  of "CALL":
    execCall(cpu, opc, i)
  of "PUSH":
    execPush(cpu, opc, i)
  of "POP":
    execPop(cpu, opc, i)
  of "RET":
    execRet(cpu, opc, i)
  of "CP":
    execSubCp(cpu, opc, i)
  of "BIT": # Prefix CB's start here
    execBit(cpu, opc, i)
  of "RL", "RLA":
    execRL(cpu, opc, i)
  else:
    assert false, "Unknown opcode type: " & opcType

proc next(cpu: CPU) =
  ## Executes the next opcode.
  let prevFlags = cpu.f
  let opcode = opcs[cpu.mem.read8(cpu.pc)]
  parseMnemonic(cpu, opcode)

  # PREFIX CB does its own verification.
  if opcode.mnemonic != "PREFIX CB":
    verifyFlags(cpu, opcode, prevFlags)

proc echoCurrentOpcode(cpu: CPU) =
  let opcode = cpu.mem.read8(cpu.pc)
  let meaning = opcs[opcode]

  echo "0x$1: $2 (0x$3)" % [cpu.pc.toHex(),
      meaning.mnemonic, opcode.toHex()]


when isMainModule:
  var mem = newMemory()
  mem.loadFile(getCurrentDir() / "bios.gb")
  var cpu = newCPU(mem)

  var breakpoints: seq[uint16] = @[]

  while true:
    stdout.write("> ")
    let split = stdin.readLine().split(" ")
    let cmd =
      if split.len > 0: split[0]
      else: ""

    case cmd.toLower()
    of "n", "next":
      cpu.next()
    of "b", "break":
      breakpoints.add(split[1].parseHexInt().uint16)
      echo "Breakpoint at 0x", breakpoints[^1].toHex()
    of "c", "continue":
      block cpuLoop:
        while true:
          for brk in breakpoints:
            if cpu.pc == brk:
              echo("Reached 0x", brk.toHex())
              break cpuLoop
          cpu.next()
    else:
      echoCurrentOpcode(cpu)
