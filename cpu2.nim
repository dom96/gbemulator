import mem2, unsigned, os, strutils, opcodes, parseutils, utils, gpu2, tables
import times
type
  CPU = ref object
    ## Registers
    pc, sp: uint16
    a, b, c, d, e, h, l, f: uint8
    mem: Memory
    ime: bool # Interrupt Master Enable Flag
    clock: int

  OperandKind = enum
    Immediate, Address, Register, RegisterCombo, IntLit

  RegisterName = enum
    RegA, RegB, RegC, RegD, RegE, RegF, RegH, RegL, RegPC, RegSP

  Operand = object
    name: string
    case kind: OperandKind
    of Register:
      location: RegisterName
      negated: bool ## Only used for conditional operands
      flagMask: uint8
    of RegisterCombo:
      first, second: RegisterName
    of Immediate:
      signed: bool
      pc: int
    of Address:
      opName: string ## Operand which stores the address.
      decrement: bool ## (HL-)
      increment: bool ## (HL+)
    of IntLit:
      num: range[0 .. 7]
    word: bool ## Determines whether location points to a uint8 (false) or uint16 (true).

proc createRegOperand(name: string, reg: RegisterName, word: bool,
                      neg = false, flagMask: uint8 = 0): Operand =
  Operand(kind: Register, name: name, location: reg, word: word,
          negated: neg, flagMask: flagMask)

proc createRegComboOperand(name: string, first, second: RegisterName): Operand =
  Operand(kind: RegisterCombo, name: name, first: first,
          second: second, word: true)

var operandTable = toTable[string, Operand]({
  "A": createRegOperand("A", RegA, false),
  "B": createRegOperand("B", RegB, false),
  "C": createRegOperand("C", RegC, false),
  "D": createRegOperand("D", RegD, false),
  "E": createRegOperand("E", RegE, false),
  "H": createRegOperand("H", RegH, false),
  "L": createRegOperand("L", RegL, false),
  "F": createRegOperand("F", RegF, false),
  "HL": createRegComboOperand("HL", RegH, RegL),
  "SP": createRegOperand("SP", RegSP, true),
  "DE": createRegComboOperand("DE", RegD, RegE),
  "BC": createRegComboOperand("BC", RegB, RegC),
  "AF": createRegComboOperand("AF", RegA, RegF),
  "NC": createRegOperand("NC", RegF, false, true, BitC),
  "NZ": createRegOperand("NZ", RegF, false, true, BitZ),
  "Z": createRegOperand("Z", RegF, false, false, BitZ),
  "D16": Operand(name: "D16", kind: Immediate, word: true, pc: 2),
  "D8": Operand(name: "D8", kind: Immediate, word: false, pc: 1),
  "R8": Operand(name: "R8", kind: Immediate, signed: true, word: false, pc: 1),
  "A8": Operand(name: "A8", kind: Immediate, signed: false, word: false, pc: 1),
  "A16": Operand(name: "A16", kind: Immediate, signed: false, word: true, pc: 2),
  "(HL+)": Operand(name: "(HL+)", kind: Address, opName: "HL", increment: true),
  "(HL-)": Operand(name: "(HL-)", kind: Address, opName: "HL", decrement: true),
  "(HL)": Operand(name: "(HL)", kind: Address, opName: "HL"),
  "(C)": Operand(name: "(C)", kind: Address, opName: "C"),
  "(A8)": Operand(name: "(A8)", kind: Address, opName: "A8"),
  "(DE)": Operand(name: "(DE)", kind: Address, opName: "DE"),
  "(A16)": Operand(name: "(A16)", kind: Address, opName: "A16"),
})

for i in 0 .. 7:
  operandTable[$i] = Operand(name: $i, kind: IntLit, num: i)

proc parseOperand(cpu: CPU, operand: string): Operand =
  ## Returns the requested operand and increases PC proportionally.
  assert operand != ""
  assert(operandTable.hasKey(operand), "Unknown operand: " & operand)
  result = operandTable[operand]
  if result.kind == Immediate:
    if result.word:
      cpu.pc.inc 2
    else:
      cpu.pc.inc

proc newCPU(mem: Memory): CPU =
  new result
  result.mem = mem
  result.pc = 0
  result.ime = true

proc getRegisterAddr8(cpu: CPU, reg: RegisterName): ptr uint8 =
  # TODO Make this return a var?
  case reg
  of RegA: result = addr cpu.a
  of RegB: result = addr cpu.b
  of RegC: result = addr cpu.c
  of RegD: result = addr cpu.d
  of RegE: result = addr cpu.e
  of RegF: result = addr cpu.f
  of RegH: result = addr cpu.h
  of RegL: result = addr cpu.l
  else:
    assert false, "Invalid register access. Want 8-bit, access to 16-bit"

proc getRegisterAddr16(cpu: CPU, reg: RegisterName): ptr uint16 =
  case reg
  of RegPC: result = addr cpu.pc
  of RegSP: result = addr cpu.sp
  else:
    assert false, "Invalid register access. Want 16-bit, access to 8-bit"

proc get16(cpu: CPU, op: Operand): uint16
proc get8(cpu: CPU, op: Operand): uint8 =
  assert(not op.word)
  case op.kind
  of Register:
    return getRegisterAddr8(cpu, op.location)[]
  of RegisterCombo:
    assert false, "Cannot return uint16 in get8 for RegisterCombo."
  of Address:
    var address: uint16
    let addrOp = parseOperand(cpu, op.opName)
    if not addrOp.word:
      case addrOp.name
      of "C", "A8":
        address = 0xFF00'u16 + cpu.get8(addrOp).uint16
      else:
        assert false, "Unknown byte-sized address register " & addrOp.name
    else:
      address = cpu.get16(addrOp)
    result = cpu.mem.read8(address)

    # There won't ever be any increments/decrements for 8-bit registers
    assert(not op.increment)
    assert(not op.decrement)
  of IntLit:
    assert false
  of Immediate:
    result = cpu.mem.read8(cpu.pc - 1)

proc get16(cpu: CPU, op: Operand): uint16 =
  assert(op.word)
  case op.kind
  of Register:
    return getRegisterAddr16(cpu, op.location)[]
  of RegisterCombo:
    return (getRegisterAddr8(cpu, op.first)[].uint16 shl 8) or
            getRegisterAddr8(cpu, op.second)[].uint16
  of Address:
    assert false
  of IntLit:
    assert false
  of Immediate:
    result = cpu.mem.read16(cpu.pc - 2)

proc set(cpu: CPU, op: Operand, value: uint16) =
  assert op.word, "Operand was: " & op.name
  case op.kind
  of Register:
    let old = getRegisterAddr16(cpu, op.location)[].toHex()
    echo("Register $1: 0x$2 -> 0x$3" %
          [op.name, old, value.toHex()])
    getRegisterAddr16(cpu, op.location)[] = value
  of RegisterCombo:
    let old = getRegisterAddr8(cpu, op.first)[].toHex() &
              getRegisterAddr8(cpu, op.second)[].toHex()
    echo("Register $1: 0x$2 -> 0x$3" %
          [op.name, old, value.toHex()])
    getRegisterAddr8(cpu, op.first)[] = (value shr 8).uint8
    getRegisterAddr8(cpu, op.second)[] = cast[uint8](value)
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
    let old = getRegisterAddr8(cpu, op.location)[].toHex()
    echo("Register $1: 0x$2 -> 0x$3" %
          [op.name, old, value.toHex()])
    getRegisterAddr8(cpu, op.location)[] = value
  of RegisterCombo:
    assert false, "Need 16 bits for two registers"
  of Address:
    var address: uint16
    let addrOp = parseOperand(cpu, op.opName)
    if not addrOp.word:
      case addrOp.name
      of "C", "A8":
        address = 0xFF00'u16 + cpu.get8(addrOp).uint16
      else:
        assert false, "Unknown byte-sized address register " & addrOp.name
    else:
      address = cpu.get16(addrOp)
    let old = cpu.mem.read8(address)
    cpu.mem.write8(address, value)

    # Check whether we should decrement/increment
    if op.decrement:
      cpu.set(addrOp, cpu.get16(addrOp)-1)
    elif op.increment:
      cpu.set(addrOp, cpu.get16(addrOp)+1)
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

proc registerF(cpu: CPU): Operand =
  operandTable["F"]

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

  # Also verify clock.
  assert(cpu.clock != 0)
  assert(cpu.clock == opc.cycles or cpu.clock == opc.idleCycles)

proc parseOperands(cpu: CPU, opc: Opcode): tuple[dest, src: Operand] =
  result.dest = parseOperand(cpu, opc.operandOne)
  result.src = parseOperand(cpu, opc.operandTwo)

proc execLoad(cpu: CPU, opc: Opcode) =
  let (dest, src) = parseOperands(cpu, opc)

  if src.word:
    cpu.set(dest, cpu.get16(src))
  else:
    cpu.set(dest, cpu.get8(src))

proc execXOR(cpu: CPU, opc: Opcode) =
  let src = parseOperand(cpu, opc.operandOne)

  # Destination for XOR is always register A.
  let dest = cpu.parseOperand("A")

  assert(not src.word)
  let value = cpu.get8(dest) xor cpu.get8(src)
  cpu.set(dest, value)

  let flags = registerF(cpu)
  # Only Z flag is affected.
  let zero: uint8 =
    if value == 0: 0b10000000
    else: 0
  cpu.set(flags, zero)

proc execJr(cpu: CPU, opc: Opcode) =
  let opOne = parseOperand(cpu, opc.operandOne)

  if opc.operandTwo != "":
    let (cond, displacement) = (opOne, parseOperand(cpu, opc.operandTwo))
    if cpu.isTrue(cond):
      let newPc = cpu.add(displacement, cpu.pc)
      echo("PC: 0x$1 -> 0x$2" % [cpu.pc.toHex(), newPc.toHex()])
      cpu.pc = newPc
      cpu.clock.inc opc.cycles
    else:
      cpu.clock.inc opc.idleCycles
  else:
    let newPc = cpu.add(opOne, cpu.pc)
    echo("PC: 0x$1 -> 0x$2" % [cpu.pc.toHex(), newPc.toHex()])
    cpu.pc = newPc

proc execInc(cpu: CPU, opc: Opcode) =
  let reg = parseOperand(cpu, opc.operandOne)
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

proc execDec(cpu: CPU, opc: Opcode) =
  let reg = parseOperand(cpu, opc.operandOne)
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

proc execCallLoc(cpu: CPU, location: uint16) =
  # Push PC onto Stack.
  cpu.sp.dec
  cpu.mem.write8(cpu.sp, uint8(cpu.pc shr 8))
  cpu.sp.dec
  cpu.mem.write8(cpu.sp, uint8((cpu.pc shl 8) shr 8))

  let newPc = location
  echo("PC: 0x$1 -> 0x$2" % [cpu.pc.toHex(), newPc.toHex()])
  cpu.pc = newPc

proc execCall(cpu: CPU, opc: Opcode) =
  let location = parseOperand(cpu, opc.operandOne)

  execCallLoc(cpu, cpu.get16(location))

proc execPush(cpu: CPU, opc: Opcode) =
  let valueOp = parseOperand(cpu, opc.operandOne)
  let value = cpu.get16(valueOp)

  cpu.sp.dec
  cpu.mem.write8(cpu.sp, uint8(value shr 8))
  cpu.sp.dec
  cpu.mem.write8(cpu.sp, uint8((value shl 8) shr 8))

proc execPop(cpu: CPU, opc: Opcode) =
  let reg = parseOperand(cpu, opc.operandOne)

  let low = cpu.mem.read8(cpu.sp)
  cpu.sp.inc
  let high = cpu.mem.read8(cpu.sp)
  cpu.sp.inc

  cpu.set(reg, (uint16(high) shl 8) or uint16(low))

proc execRet(cpu: CPU, opc: Opcode) =
  let low = cpu.mem.read8(cpu.sp)
  cpu.sp.inc
  let high = cpu.mem.read8(cpu.sp)
  cpu.sp.inc

  let newPc = (uint16(high) shl 8) or uint16(low)
  echo("PC: 0x$1 -> 0x$2" % [cpu.pc.toHex(), newPc.toHex()])
  cpu.pc =  newPc

proc execSubCp(cpu: CPU, opc: Opcode) =
  let reg = parseOperand(cpu, opc.operandOne)

  let accumulator = cpu.parseOperand("A")
  let aValue = cpu.get8(accumulator)

  let value = cpu.get8(reg)
  let newValue = aValue - value

  # TODO: Check opcode addr range?
  if opc.mnemonic == "SUB":
    cpu.set(accumulator, newValue)

  # Check for half carry.
  let halfCarry = (((aValue and 0xF) - (value and 0xF)) and 0x10) == 0x10
  let carry = aValue < value
  let zero = newValue == 0
  let flags = registerF(cpu)
  let flagsValue = cpu.get8(flags)
  cpu.set(flags,
      flagsValue.changeFlags(z = >>zero, n=FSet, h = >>halfCarry, c = >>carry))

# ---- Prefix CB opcodes follow.

proc execBit(cpu: CPU, opc: Opcode) =
  let (bitNum, src) = parseOperands(cpu, opc)
  assert bitNum.kind == IntLit

  # TODO: This may be wrong.
  let srcVal = cpu.get8(src)
  let zero = (srcVal and (1.uint8 shl bitNum.num)) == 0
  let flags = registerF(cpu)
  let flagsValue = cpu.get8(flags)
  cpu.set(flags, flagsValue.changeFlags(>>zero, FUnset, FSet))

proc execRL(cpu: CPU, opc: Opcode) =
  let reg =
    if opc.mnemonic == "RLA": cpu.parseOperand("A")
    else: parseOperand(cpu, opc.operandOne)

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
  cpu.pc.inc
  case opc.mnemonic
  of "NOP":
    nil
  of "LD", "LDH":
    execLoad(cpu, opc)
  of "XOR":
    execXOR(cpu, opc)
  of "PREFIX":
    execPrefix(cpu, opc)
  of "JR":
    execJr(cpu, opc)
  of "INC":
    execInc(cpu, opc)
  of "DEC":
    execDec(cpu, opc)
  of "CALL":
    execCall(cpu, opc)
  of "PUSH":
    execPush(cpu, opc)
  of "POP":
    execPop(cpu, opc)
  of "RET":
    execRet(cpu, opc)
  of "CP", "SUB":
    execSubCp(cpu, opc)
  of "BIT": # Prefix CB's start here
    execBit(cpu, opc)
  of "RL", "RLA":
    execRL(cpu, opc)
  else:
    assert false, "Unknown opcode type: " & opc.mnemonic

  if cpu.clock == 0 and opc.cycles == opc.idleCycles:
    cpu.clock.inc opc.cycles

proc handleInterrupts(cpu: CPU) =
  if not cpu.ime: return
  let interruptFlag = cpu.mem.read8(0xFF0F)
  let interruptEnabled = cpu.mem.read8(0xFFFF)
  var location: uint16 = 0
  if ((interruptFlag and 1) == 1) and
     ((interruptEnabled and 1) == 1):
    # V-Blank interrupt.
    location = 0x0040'u16
    cpu.mem.write8(0xFF0F, interruptFlag and 0b11110)
  elif (interruptFlag and (1 shl 1)) == 1 shl 1:
    # LCD STAT
    location = 0x0048'u16
    cpu.mem.write8(0xFF0F, interruptFlag and 0b11101)
  elif (interruptFlag and (1 shl 2)) == 1 shl 2:
    # Timer
    location = 0x0050'u16
    cpu.mem.write8(0xFF0F, interruptFlag and 0b11011)
  elif (interruptFlag and (1 shl 3)) == 1 shl 3:
    # Serial
    location = 0x0058'u16
    cpu.mem.write8(0xFF0F, interruptFlag and 0b10111)
  elif (interruptFlag and (1 shl 4)) == 1 shl 4:
    # Joypad
    location = 0x0060'u16
    cpu.mem.write8(0xFF0F, interruptFlag and 0b01111)

  if location != 0:
    cpu.ime = false
    execCallLoc(cpu, location)
    cpu.clock.inc(5 * 4)

proc next(cpu: CPU) =
  ## Executes the next opcode.
  cpu.clock = 0
  let prevFlags = cpu.f
  let opcode = opcs[cpu.mem.read8(cpu.pc)]
  parseMnemonic(cpu, opcode)

  # PREFIX CB does its own verification.
  if opcode.opcode != 0xCB:
    verifyFlags(cpu, opcode, prevFlags)

  handleInterrupts(cpu)

proc echoCurrentOpcode(cpu: CPU) =
  let opcode = cpu.mem.read8(cpu.pc)
  let meaning = opcs[opcode]

  echo "0x$1: $2 (0x$3)" % [cpu.pc.toHex(),
      meaning.mnemonic & " " & meaning.operandOne & "," & meaning.operandTwo,
      opcode.toHex()]

proc verifyChecksum(cpu: CPU) =
  const nintendoGraphic: array[0x104'u16 .. 0x133'u16, uint8] =
    [
      0xCE'u8, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B,
      0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
      0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,
      0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
      0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC,
      0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E
    ]
  for i in 0x104'u16 .. 0x133'u16:
    assert cpu.mem.read8(i) == nintendoGraphic[i], "Invalid Nintendo Graphic"

proc dump(cpu: CPU) =
  ## For debugging info.
  var data = ""
  for i in 0x8000 .. 0x81A0:
    data.add(cast[char](cpu.mem.read8(i.uint16)))

  writeFile(getCurrentDir() / "dump8000.mem", data)

  data = ""
  for i in 0x9900 .. 0x9930:
    data.add(cast[char](cpu.mem.read8(i.uint16)))

  writeFile(getCurrentDir() / "dump9900.mem", data)

when isMainModule:
  var mem = newMemory()
  mem.loadFile(getCurrentDir() / "tetris.gb", getCurrentDir() / "bios.gb")
  var cpu = newCPU(mem)
  var gpu = newGPU(mem)

  verifyChecksum(cpu)

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
      gpu.next(cpu.clock)
    of "b", "break":
      breakpoints.add(split[1].parseHexInt().uint16)
      echo "Breakpoint at 0x", breakpoints[^1].toHex()
    of "d", "dump":
      dump(cpu)
    of "c", "continue":
      var counter = 0
      var timeCounter = epochTime()

      block cpuLoop:
        while true:
          for brk in breakpoints:
            if cpu.pc == brk:
              echo("Reached 0x", brk.toHex())
              break cpuLoop
          cpu.next()
          gpu.next(cpu.clock)
          counter.inc cpu.clock
          if epochTime() - timeCounter >= 1.0:
            writeFile(getCurrentDir() / "ips.txt", $counter)
            counter = 0
            timeCounter = epochTime()
    else:
      echoCurrentOpcode(cpu)
