import mem2, unsigned, os, strutils, opcodes, parseutils, utils, gpu2, tables
import times, macros
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
    RegA = "a", RegB = "b", RegC = "c", RegD = "d", RegE = "e", RegF = "f",
    RegH = "h", RegL = "l", RegPC = "pc", RegSP = "sp"

  Operand = ref object
    name: string
    case kind: OperandKind
    of Register:
      location: RegisterName
      negated: bool ## Only used for conditional operands
      flagMask: int
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
                      neg = false, flagMask: int = 0): Operand =
  Operand(kind: Register, name: name, location: reg, word: word,
          negated: neg, flagMask: flagMask)

proc createRegComboOperand(name: string, first, second: RegisterName): Operand =
  Operand(kind: RegisterCombo, name: name, first: first,
          second: second, word: true)

proc parseOperand(cpu: CPU, operand: string): Operand =
  ## Returns the requested operand and increases PC proportionally.
  nil # TODO: DETELEME

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
    echod("Register $1: 0x$2 -> 0x$3" %
          [op.name, old, value.toHex()])
    getRegisterAddr16(cpu, op.location)[] = value
  of RegisterCombo:
    let old = getRegisterAddr8(cpu, op.first)[].toHex() &
              getRegisterAddr8(cpu, op.second)[].toHex()
    echod("Register $1: 0x$2 -> 0x$3" %
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
    echod("Register $1: 0x$2 -> 0x$3" %
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
    result = cpu.f.isFlagSet(op.flagMask.uint8)
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
  assert false

proc verifyFlags(cpu: CPU, opc: Opcode, prevFlags: uint8) =
  let flags = cpu.f
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
  #let dest = createRegOperand("B", RegB, false)
  #let src = Operand(name: "A8", kind: Immediate, signed: false, word: false, pc: 1)
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
      echod("PC: 0x$1 -> 0x$2" % [cpu.pc.toHex(), newPc.toHex()])
      cpu.pc = newPc
      cpu.clock.inc opc.cycles
    else:
      cpu.clock.inc opc.idleCycles
  else:
    let newPc = cpu.add(opOne, cpu.pc)
    echod("PC: 0x$1 -> 0x$2" % [cpu.pc.toHex(), newPc.toHex()])
    cpu.pc = newPc

proc execJp(cpu: CPU, opc: Opcode) =
  let opOne = parseOperand(cpu, opc.operandOne)

  let newPc = cpu.get16(opOne)
  echod("PC: 0x$1 -> 0x$2" % [cpu.pc.toHex(), newPc.toHex()])
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
  echod("PC: 0x$1 -> 0x$2" % [cpu.pc.toHex(), newPc.toHex()])
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
  echod("PC: 0x$1 -> 0x$2" % [cpu.pc.toHex(), newPc.toHex()])
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
  of "JP":
    execJp(cpu, opc)
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

proc parseOperandCT(encoded: string): Operand =
  case encoded
  of "A":
    result = createRegOperand(encoded, RegA, false)
  of "B":
    result = createRegOperand(encoded, RegB, false)
  of "C":
    result = createRegOperand(encoded, RegC, false)
  of "D":
    result = createRegOperand(encoded, RegD, false)
  of "E":
    result = createRegOperand(encoded, RegE, false)
  of "H":
    result = createRegOperand(encoded, RegH, false)
  of "L":
    result = createRegOperand(encoded, RegL, false)
  of "F":
    result = createRegOperand(encoded, RegF, false)
  of "HL":
    result = createRegComboOperand(encoded, RegH, RegL)
  of "SP":
    result = createRegOperand(encoded, RegSP, true)
  of "PC":
    result = createRegOperand(encoded, RegPC, true)
  of "DE":
    result = createRegComboOperand(encoded, RegD, RegE)
  of "BC":
    result = createRegComboOperand(encoded, RegB, RegC)
  of "AF":
    result = createRegComboOperand(encoded, RegA, RegF)
  of "NC":
    result = createRegOperand(encoded, RegF, false, true, BitC.int)
  of "NZ":
    result = createRegOperand(encoded, RegF, false, true, BitZ.int)
  of "Z":
    result = createRegOperand(encoded, RegF, false, false, BitZ.int)
  of "D16":
    result = Operand(name: encoded, kind: Immediate, word: true)
  of "D8":
    result = Operand(name: encoded, kind: Immediate, word: false)
  of "R8":
    result = Operand(name: encoded, kind: Immediate, signed: true, word: false)
  of "A8":
    result = Operand(name: encoded, kind: Immediate, signed: false, word: false)
  of "A16":
    result = Operand(name: encoded, kind: Immediate, signed: false, word: true)
  of "(HL+)":
    result = Operand(name: "(HL+)", kind: Address, opName: "HL", increment: true, word: true)
  of "(HL-)":
    result = Operand(name: "(HL-)", kind: Address, opName: "HL", decrement: true, word: true)
  of "(HL)":
    result = Operand(name: "(HL)", kind: Address, opName: "HL", word: true)
  of "(C)":
    result = Operand(name: "(C)", kind: Address, opName: "C")
  of "(A8)":
    result = Operand(name: "(A8)", kind: Address, opName: "A8")
  of "(DE)":
    result = Operand(name: "(DE)", kind: Address, opName: "DE", word: true)
  of "(A16)":
    result = Operand(name: "(A16)", kind: Address, opName: "A16", word: true)
  of "(BC)":
    result = Operand(name: "(BC)", kind: Address, opName: "BC", word: true)
  of "0", "1", "2", "3", "4", "5", "6", "7":
    result = Operand(name: encoded, kind: IntLit, num: encoded.parseInt())
  else:
    assert false, "Unknown operand: " & encoded.toUpper()

proc assertNodeSize(result: var NimNode, n: NimNode, size: int) {.compileTime.} =
  # -> assert sizeof(`n`) == 2
  result.add quote do:
    assert sizeof(`n`) == `size`

proc createGet16(operand: Operand): NimNode {.compileTime.}
proc createGet8(operand: Operand): NimNode {.compileTime.}
proc calculateAddress(operand: Operand):
    tuple[address: NimNode, addrOp: Operand] {.compileTime.} =
  var address: NimNode
  let addrOp = parseOperandCT(operand.opName)
  if not addrOp.word:
    case addrOp.name
    of "C", "A8":
      var offset = parseExpr("0xFF00'u16")
      var addrVal = newCall("uint16", createGet8(addrOp))
      address = infix(offset, "+", addrVal)
    else:
      assert false, "Unknown byte-sized address register " & addrOp.name
  else:
    address = createGet16(addrOp)

  return (address, addrOp)

proc createSet16(operand: Operand, src: NimNode): NimNode {.compileTime.}
proc incDecAddress(operand, addrOp: Operand,
    result: var NimNode) {.compileTime.} =
  # Check whether we should decrement/increment
  if operand.decrement:
    # -> cpu.r = cpu.r - 1
    result.add createSet16(addrOp,
        infix(createGet16(addrOp), "-", newIntLitNode(1)))
  elif operand.increment:
    # -> cpu.r = cpu.r + 1
    result.add createSet16(addrOp,
        infix(createGet16(addrOp), "+", newIntLitNode(1)))

proc createSet16(operand: Operand, src: NimNode): NimNode =
  result = newStmtList()

  assertNodeSize(result, src, 2)

  case operand.kind
  of Register:
    assert(operand.word, operand.name)
    let reg = newDotExpr(newIdentNode("cpu"), newIdentNode(operand.name.toLower()))
    assertNodeSize(result, reg, 2)
    # -> cpu.r = `src`
    result.add newAssignment(
        reg,
        src
      )
  of RegisterCombo:
    let first = newDotExpr(newIdentNode("cpu"),
        newIdentNode($operand.first))
    assertNodeSize(result, first, 1)
    let second = newDotExpr(newIdentNode("cpu"),
        newIdentNode($operand.second))
    assertNodeSize(result, second, 1)
    # -> cpu.r1 = (`src` shr 8).uint8
    result.add newAssignment(
        first,
        newCall(newIdentNode("uint8"), infix(src, "shr", newIntLitNode(8)))
      )
    # -> cpu.r2 = (`src` shr 8).uint8
    result.add newAssignment(
        second,
        newNimNode(nnkCast).add(newIdentNode("uint8"), src)
      )
  of Address:
    let (address, addrOp) = calculateAddress(operand)

    # -> cpu.mem.write16(`address`, `src`)
    result.add quote do:
      cpu.mem.write16(`address`, `src`)

    incDecAddress(operand, addrOp, result)
  else:
    assert false, $operand.kind

proc createSet8(operand: Operand, src: NimNode): NimNode {.compileTime.} =
  result = newStmtList()

  # XXX: Setting an 8-bit value to a 16-bit dest sounds ok.
  assertNodeSize(result, src, 1)

  case operand.kind
  of Register:
    assert(not operand.word, operand.name)
    let reg = newDotExpr(newIdentNode("cpu"), newIdentNode(operand.name.toLower()))
    assertNodeSize(result, reg, 1)
    # -> cpu.r = `src`
    result.add newAssignment(
        reg,
        src
      )
  of RegisterCombo:
    assert false, "RegisterCombo takes 16-bit values."
  of Address:
    let (address, addrOp) = calculateAddress(operand)

    # -> cpu.mem.write8(`address`, `src`)
    result.add quote do:
      cpu.mem.write8(`address`, `src`)

    if operand.word:
      incDecAddress(operand, addrOp, result)
    else:
      # No decrement/increment for 8-bit addresses.
      assert(not operand.decrement)
      assert(not operand.increment)
  else:
    assert false, $operand.kind

proc createGet16(operand: Operand): NimNode =
  # TODO: Using a nnkStmtListExpr here causes compiler crash.

  assert(operand.word, operand.name)

  var tempIdent = newIdentNode("temp")

  case operand.kind
  of Immediate:
    # let temp = cpu.mem.read16(cpu.pc)
    result = parseExpr("cpu.mem.read16(cpu.pc)")
  of Register:
    var regIdent = newIdentNode($operand.location)
    result = quote do:
      cpu.`regIdent`
  of RegisterCombo:
    let first = newIdentNode($operand.first)
    let second = newIdentNode($operand.second)
    result = quote do:
      (cpu.`first`.uint16 shl 8) or cpu.`second`.uint16
  of Address:
    let (address, addrOp) = calculateAddress(operand)

    var procBody = newStmtList()
    var resultIdent = genSym(nskVar, "result")
    procBody.add quote do:
      var `resultIdent` = cpu.mem.read16(`address`)

    incDecAddress(operand, addrOp, procBody)

    procBody.add newNimNode(nnkReturnStmt).add resultIdent

    result = newStmtList()
    var name = genSym(nskProc, "derefAddr")
    result.add newProc(name, [newIdentNode"uint16"], procBody)
    result.add newCall(name)
  else:
    assert false, $operand.kind

proc createGet8(operand: Operand): NimNode =
  if operand.kind != Address:
    assert(not operand.word)

  case operand.kind
  of Immediate:
    # let temp = cpu.mem.read8(cpu.pc)
    result = quote do:
      cpu.mem.read8(cpu.pc)
  of Register:
    var regIdent = newIdentNode($operand.location)
    result = quote do:
      cpu.`regIdent`
  of RegisterCombo:
    assert false, "Cannot create 16-bit RegisterCombo, 8-bit expected."
  of Address:
    let (address, addrOp) = calculateAddress(operand)

    var procBody = newStmtList()
    var resultIdent = genSym(nskVar, "result")
    procBody.add quote do:
      var `resultIdent` = cpu.mem.read8(`address`)

    incDecAddress(operand, addrOp, procBody)

    procBody.add newNimNode(nnkReturnStmt).add resultIdent

    result = newStmtList()
    var name = genSym(nskProc, "derefAddr")
    result.add newProc(name, [newIdentNode"uint8"], procBody)
    result.add newCall(name)
  else:
    assert false, $operand.kind

proc createCond(operand: Operand): NimNode {.compileTime.} =
  case operand.kind
  of Register:
    var mask = 0
    if operand.flagMask == 0:
      # Sort out the 'C' ambiguity.
      assert operand.name == "C", operand.name
      mask = BitC.int
    else:
      mask = operand.flagMask
    if operand.negated:
      result = quote do:
        not cpu.f.isFlagSet(`mask`)
    else:
      result = quote do:
        cpu.f.isFlagSet(`mask`)
  else:
    assert false

proc createAdd(x, y: Operand): NimNode {.compileTime.} =
  ## This should only really be used in JR.
  assert x.kind == Immediate

  if x.word:
    let data = createGet16(x)
    let pc = createGet16(y)
    if x.signed:
      result = quote do:
        uint16(cast[int16](`data`) + `pc`.int64)
    else:
      result = quote do: `data` + `pc`
  else:
    let data = createGet8(x)
    let pc = createGet16(y)
    if x.signed:
      result = quote do:
        uint16(cast[int8](`data`) + `pc`.int64)
    else:
      result = quote do: `data`.uint16 + `pc`

proc createChangeFlags(z = FUnchanged, n = FUnchanged,
                      h = FUnchanged, c = FUnchanged): NimNode {.compileTime.} =
  result = quote do:
    cpu.f = cpu.f.changeFlags(z, n, h, c)

template genGeneric(name, body, opcodeList: expr,
                    contents: stmt): stmt {.immediate.} =
  for opc {.inject.} in opcodeList:
    if opc.mnemonic == name:# and opc.opcode in {0xC3, 0x06}:
      var ofBranch = newNimNode(nnkOfBranch)
      ofBranch.add newIntLitNode(opc.opcode)

      var body = newStmtList()

      var operandOne {.inject.}: Operand = nil
      var operandTwo {.inject.}: Operand = nil
      if opc.operandOne != "":
         operandOne = parseOperandCT(opc.operandOne)
      if opc.operandTwo != "":
         operandTwo = parseOperandCT(opc.operandTwo)

      contents

      if opc.operandTwo != "":
        if operandTwo.kind == Immediate:
          if operandTwo.word:
            body.add parseExpr("cpu.pc.inc 2")
          else:
            body.add parseExpr("cpu.pc.inc")

      if opc.cycles == opc.idleCycles:
        body.add parseExpr("cpu.clock.inc " & $opc.cycles)

      ofBranch.add body
      result.add ofBranch

proc genLoad(result: NimNode) {.compileTime.} =
  genGeneric("LD", body, opcs):
    var word = false
    if operandOne.kind == Address:
      word = operandTwo.word
    elif operandTwo.kind == Address:
      word = operandOne.word
    else:
      word = operandOne.word and operandTwo.word

    if word:
      body.add createSet16(operandOne, createGet16(operandTwo))
    else:
      body.add createSet8(operandOne, createGet8(operandTwo))

proc genXor(result: NimNode) {.compileTime.} =
  genGeneric("XOR", body, opcs):
    assert((not operandOne.word) or operandOne.kind == Address)

    var src = createGet8(operandOne)
    body.add quote do:
      cpu.a = cpu.a xor `src`

      cpu.f = cpu.f.changeFlags(z = >>(cpu.a == 0))

proc genJr(result: NimNode) {.compileTime.} =
  genGeneric("JR", body, opcs):
    if opc.operandTwo != "":
      let cond = createCond(operandOne)
      let addition = createAdd(operandTwo, parseOperandCT("PC"))
      let cycles = newIntLitNode(opc.cycles)
      let idleCycles = newIntLitNode(opc.idleCycles)
      body.add quote do:
        if `cond`:
          cpu.pc = `addition`
          cpu.clock.inc `cycles`
        else:
          cpu.clock.inc `idleCycles`
    else:
      let addition = createAdd(operandOne, parseOperandCT("PC"))
      body.add quote do:
        cpu.pc = `addition`

proc genJp(result: NimNode) {.compileTime.} =
  genGeneric("JP", body, opcs):
    if operandTwo == nil:
      let dest = parseOperandCT("PC")
      body.add createSet16(dest, createGet16(operandOne))
    else:
      let cond = createCond(operandOne)
      let newPc = createGet16(operandTwo)
      let cycles = newIntLitNode(opc.cycles)
      let idleCycles = newIntLitNode(opc.idleCycles)
      body.add quote do:
        if `cond`:
          cpu.pc = `newPc`
          cpu.clock.inc `cycles`
        else:
          cpu.clock.inc `idleCycles`
          nil

proc genBit(result: NimNode) {.compileTime.} =
  genGeneric("BIT", body, prefixOpcs):
    assert operandOne.kind == IntLit

    let bitNum = operandOne.num
    let src = createGet8(operandTwo)
    body.add quote do:
      # TODO: This may be wrong.
      let zero = (`src` and (1.uint8 shl `bitNum`)) == 0
      cpu.f = cpu.f.changeFlags(>>zero, FUnset, FSet)

proc genCb(result: NimNode) {.compileTime.} =
  genGeneric("PREFIXCB", body, opcs):
    var cbAddrIdent = newIdentNode("cbAddr")
    var caseStmt = newNimNode(nnkCaseStmt)
    caseStmt.add cbAddrIdent
    genBit(caseStmt)

    caseStmt.add(newNimNode(nnkElse).add(
      quote do: assert false, `cbAddrIdent`.toHex()))

    body.add quote do:
      let prevFlags = cpu.f
      let `cbAddrIdent` = cpu.mem.read8(cpu.pc)
      `caseStmt`
      verifyFlags(cpu, prefixOpcs[`cbAddrIdent`], prevFlags)

macro genOpcodeLogic(): stmt =
  ## We want to generate a fast case statement dealing with all opcodes defined
  ## in the opcodes module.

  result = newNimNode(nnkCaseStmt)
  result.add(newIdentNode("opcodeAddr"))

  genLoad(result)
  genXor(result)
  genJp(result)
  genJr(result)
  genCb(result)


  result.add(newNimNode(nnkElse).add(parseExpr("assert false, opcodeAddr.toHex()")))

  echo(result.toStrLit())

proc saveRegisters(cpu: CPU): tuple[pc, sp: uint16, a, b, c, d,
    e, f, h, l: uint8] =
  result = (cpu.pc, cpu.sp, cpu.a, cpu.b, cpu.c, cpu.d, cpu.e, cpu.f,
      cpu.h, cpu.l)

proc echoRegDiff(oldReg, newReg: tuple[pc, sp: uint16, a, b, c, d,
    e, f, h, l: uint8]) =
  if oldReg.pc != newReg.pc:
    echodReg("PC", oldReg.pc, newReg.pc)
  if oldReg.sp != newReg.sp:
    echodReg("SP", oldReg.sp, newReg.sp)
  if oldReg.a != newReg.a:
    echodReg("A", oldReg.a, newReg.a)
  if oldReg.b != newReg.b:
    echodReg("B", oldReg.b, newReg.b)
  if oldReg.c != newReg.c:
    echodReg("C", oldReg.c, newReg.c)
  if oldReg.d != newReg.d:
    echodReg("D", oldReg.d, newReg.d)
  if oldReg.e != newReg.e:
    echodReg("E", oldReg.e, newReg.e)
  if oldReg.f != newReg.f:
    echodReg("F", oldReg.f, newReg.f)
  if oldReg.h != newReg.h:
    echodReg("H", oldReg.h, newReg.h)
  if oldReg.l != newReg.l:
    echodReg("L", oldReg.l, newReg.l)

proc next(cpu: CPU) =
  ## Executes the next opcode.
  cpu.clock = 0
  let prevFlags = cpu.f
  let opcodeAddr = cpu.mem.read8(cpu.pc)
  let opcode = opcs[opcodeAddr]
  let oldReg = saveRegisters(cpu)
  cpu.pc.inc
  genOpcodeLogic()

  echoRegDiff(oldReg, saveRegisters(cpu))

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

proc main() =
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

when isMainModule:
  main()
