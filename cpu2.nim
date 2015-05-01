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

proc newCPU(mem: Memory): CPU =
  new result
  result.mem = mem
  result.pc = 0
  result.ime = true

proc verifyFlags(cpu: CPU, opc: Opcode, prevFlags: uint8) =
  template assertInfo(cond: expr) =
    assert cond, opc.mnemonic & " (0x" & opc.opcode.toHex(4) & ")"
  let flags = cpu.f
  if opc.z == '1': assertInfo((flags and 0b10000000) == 0b10000000)
  elif opc.z == '0': assertInfo((flags and 0b10000000) == 0)
  elif opc.z == '-': assertInfo(isFlagSet(flags, BitZ) == isFlagSet(prevFlags, BitZ))

  if opc.n == '1': assertInfo((flags and 0b01000000) == 0b01000000)
  elif opc.n == '0': assertInfo((flags and 0b01000000) == 0)
  elif opc.n == '-': assertInfo(isFlagSet(flags, BitN) == isFlagSet(prevFlags, BitN))

  if opc.h == '1': assertInfo((flags and 0b00100000) == 0b00100000)
  elif opc.h == '0': assertInfo((flags and 0b00100000) == 0)
  elif opc.h == '-': assertInfo(isFlagSet(flags, BitH) == isFlagSet(prevFlags, BitH))

  if opc.c == '1': assertInfo((flags and 0b00010000) == 0b00010000)
  elif opc.c == '0': assertInfo((flags and 0b00010000) == 0)
  elif opc.c == '-': assertInfo(isFlagSet(flags, BitC) == isFlagSet(prevFlags, BitC))

  # Also verify clock.
  assertInfo(cpu.clock != 0)
  assertInfo(cpu.clock == opc.cycles or cpu.clock == opc.idleCycles)

proc execCallLoc(cpu: CPU, location: uint16) =
  # Push PC onto Stack.
  cpu.sp.dec
  cpu.mem.write8(cpu.sp, uint8(cpu.pc shr 8))
  cpu.sp.dec
  cpu.mem.write8(cpu.sp, uint8((cpu.pc shl 8) shr 8))

  cpu.pc = location

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

proc createPcInc(operand: Operand): NimNode {.compileTime.} =
  case operand.kind
  of Immediate:
    if operand.word:
      result = quote do: cpu.pc.inc 2
    else:
      result = quote do: cpu.pc.inc
  of Address:
    let addrOp = parseOperandCT(operand.opName)
    result = createPcInc(addrOp)
  else:
    result = newEmptyNode()

template genGeneric(name, body, opcodeList: expr,
                    contents: stmt): stmt {.immediate.} =
  for opc {.inject.} in opcodeList:
    if opc.mnemonic in name:# and opc.opcode in {0xC4}:
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

      if not operandOne.isNil():
        body.add createPcInc(operandOne)
      if not operandTwo.isNil():
        body.add createPcInc(operandTwo)

      if opc.cycles == opc.idleCycles:
        body.add parseExpr("cpu.clock.inc " & $opc.cycles)

      ofBranch.add body
      result.add ofBranch

proc genNop(result: NimNode) {.compileTime.} =
  genGeneric(["NOP"], body, opcs):
    discard # Do nothing

proc genLoad(result: NimNode) {.compileTime.} =
  genGeneric(["LD", "LDH"], body, opcs):
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
  genGeneric(["XOR"], body, opcs):
    assert((not operandOne.word) or operandOne.kind == Address)

    var src = createGet8(operandOne)
    body.add quote do:
      cpu.a = cpu.a xor `src`

      cpu.f = cpu.f.changeFlags(z = >>(cpu.a == 0), n = FUnset,
          c = FUnset, h = FUnset)

proc genJr(result: NimNode) {.compileTime.} =
  genGeneric(["JR"], body, opcs):
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

    # Apparently "The assembler automatically adjusts for the twice
    # incremented PC."

proc genJp(result: NimNode) {.compileTime.} =
  genGeneric(["JP"], body, opcs):
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

    operandOne = nil
    operandTwo = nil

proc genInc(result: NimNode) {.compileTime.} =
  genGeneric(["INC"], body, opcs):
    if not operandOne.word:
      let reg = createGet8(operandOne)
      let newValueIdent = newIdentNode("newValue")
      body.add quote do:
        let `newValueIdent` = `reg`+1
        # Check for half carry.
        let halfCarry = (((`reg` and 0xF) +
          (1'u8 and 0xF)) and 0x10) == 0x10
        let zero = `newValueIdent` == 0
        cpu.f = cpu.f.changeFlags(z = >>zero, n=FUnset, h = >>halfCarry)
      body.add createSet8(operandOne, newValueIdent)
    else:
      # Flags are not affected for 16bit INC.
      let reg = createGet16(operandOne)
      let newValueIdent = newIdentNode("newValue")
      body.add quote do:
        let `newValueIdent` = `reg`+1

      body.add createSet16(operandOne, newValueIdent)

proc genDec(result: NimNode) {.compileTime.} =
  genGeneric(["DEC"], body, opcs):
    if not operandOne.word:
      let reg = createGet8(operandOne)
      let newValueIdent = newIdentNode("newValue")
      body.add quote do:
        let `newValueIdent` = `reg`-1
        # Check for half carry.
        let halfCarry = (((`reg` and 0xF) -
           (1'u8 and 0xF)) and 0x10) == 0x10
        let zero = `newValueIdent` == 0
        cpu.f = cpu.f.changeFlags(z = >>zero, n=FSet, h = >>halfCarry)

      body.add createSet8(operandOne, newValueIdent)
    else:
      # Flags are not affected for 16bit DEC.
      let reg = createGet16(operandOne)
      let newValueIdent = newIdentNode("newValue")
      body.add quote do:
        let `newValueIdent` = `reg`-1

      body.add createSet16(operandOne, newValueIdent)

proc genCall(result: NimNode) {.compileTime.} =
  genGeneric(["CALL"], body, opcs):
    let locationIdent = newIdentNode("location")
    if operandTwo.isNil:
      let location = createGet16(operandOne)
      body.add quote do:
        let `locationIdent` = `location`
      body.add createPcInc(operandOne)

      body.add quote do: execCallLoc(cpu, `locationIdent`)
    else:
      let cond = createCond(operandOne)
      let location = createGet16(operandTwo)
      let cycles = newIntLitNode(opc.cycles)
      let idleCycles = newIntLitNode(opc.idleCycles)
      body.add quote do:
        let `locationIdent` = `location`
      body.add createPcInc(operandOne)
      body.add createPcInc(operandTwo)
      body.add quote do:
        if `cond`:
          execCallLoc(cpu, `locationIdent`)
          cpu.clock.inc `cycles`
        else:
          cpu.clock.inc `idleCycles`

    operandOne = nil # Prevent genGeneric from increasing PC.
    operandTwo = nil

proc genPush(result: NimNode) {.compileTime.} =
  genGeneric(["PUSH"], body, opcs):
    let value = createGet16(operandOne)
    body.add quote do:
      cpu.sp.dec
      cpu.mem.write8(cpu.sp, uint8(`value` shr 8))
      cpu.sp.dec
      cpu.mem.write8(cpu.sp, uint8((`value` shl 8) shr 8))

proc genPopGeneric(body: NimNode): tuple[high, low, value: NimNode] {.compileTime.} =
  let high = newIdentNode"high"
  let low = newIdentNode"low"
  let value = newIdentNode"value"
  body.add quote do:
    let `low` = cpu.mem.read8(cpu.sp)
    cpu.sp.inc
    let `high` = cpu.mem.read8(cpu.sp)
    cpu.sp.inc

    let `value` = (uint16(`high`) shl 8) or uint16(`low`)

  return (high, low, value)

proc genPop(result: NimNode) {.compileTime.} =
  genGeneric(["POP"], body, opcs):
    let (high, low, value) = genPopGeneric(body)

    body.add createSet16(operandOne, value)

proc genRet(result: NimNode) {.compileTime.} =
  genGeneric(["RET"], body, opcs):
    if operandOne.isNil:
      let (high, low, value) = genPopGeneric(body)

      body.add quote do:
        cpu.pc =  `value`
    else:
      let cond = createCond(operandOne)
      let cycles = newIntLitNode(opc.cycles)
      let idleCycles = newIntLitNode(opc.idleCycles)
      var stackPop = newStmtList()
      let (high, low, value) = genPopGeneric(stackPop)

      body.add quote do:
        if `cond`:
          `stackPop`
          cpu.pc = `value`
          cpu.clock.inc `cycles`
        else:
          cpu.clock.inc `idleCycles`

proc genSubCp(result: NimNode) {.compileTime.} =
  genGeneric(["SUB", "CP"], body, opcs):
    let reg = createGet8(operandOne)
    let value = newIdentNode"value"
    body.add quote do:
      let `value` = cpu.a - `reg`

    if opc.mnemonic == "SUB":
      body.add quote do:
        cpu.a = `value`

    body.add quote do:
      # Check for half carry.
      # TODO: Half carry is calculated incorrectly. ld a, 0; sub a, 88; should
      # set the carry, but it does not. 0xc90b in 06-ld r,r.gb
      let halfCarry = (((cpu.a and 0xF) - (`reg` and 0xF)) and 0x10) == 0x10
      let carry = cpu.a < `reg`
      let zero = `value` == 0
      cpu.f = cpu.f.changeFlags(z = >>zero, n=FSet, h = >>halfCarry, c = >>carry)

proc genAdd(result: NimNode) {.compileTime.} =
  genGeneric(["ADD"], body, opcs):
    if not operandOne.word:
      let regA = createGet8(operandOne)
      let regB = createGet8(operandTwo)
      let newValueIdent = newIdentNode("newValue")
      body.add quote do:
        let `newValueIdent` = `regA`+`regB`
        let regAValue = `regA`
        let regBValue = `regB`
        let halfCarry = (((regAValue and 0xF) + (regBValue and 0xF)) and 0x10) == 0x10
        let zero = `newValueIdent` == 0
        let carry = (regAValue.int + regBValue.int) > high(uint8).int
        cpu.f = cpu.f.changeFlags(z = >>zero, n=FUnset,
            h = >>halfCarry, c = >>carry)
      body.add createSet8(operandOne, newValueIdent)
    else:
      let regA = createGet16(operandOne)
      let regB =
        if operandTwo.word: createGet16(operandTwo)
        else: createGet8(operandTwo)
      let newValueIdent = newIdentNode("newValue")
      body.add quote do:
        let `newValueIdent` = `regA`+`regB`
        let halfCarry = (((`regA` and 0xFF) + (`regB` and 0xFF)) and 0x100) == 0x100
        let carry = (`regA`.int + `regB`.int) > high(uint16).int
        cpu.f = cpu.f.changeFlags(n=FUnset, h = >>halfCarry, c = >>carry)
      body.add createSet16(operandOne, newValueIdent)

proc genAdc(result: NimNode) {.compileTime.} =
  genGeneric(["ADC"], body, opcs):
    assert(not operandOne.word)
    let regA = createGet8(operandOne)
    let regB = createGet8(operandTwo)
    let newValueIdent = newIdentNode("newValue")
    let carryIdent = newIdentNode("carry")
    body.add quote do:
      let `carryIdent` =
        if cpu.f.isFlagSet(BitC): 1'u8
        else: 0'u8
      let `newValueIdent` = `regA` + `regB` + `carryIdent`
      let regAValue = `regA`
      let regBValue = `regB`
      let halfCarry = (((regAValue and 0xF) + (regBValue and 0xF) +
          (`carryIdent` and 0xF)) and 0x10) == 0x10
      let zero = `newValueIdent` == 0
      let carry = (regAValue.int + regBValue.int + `carryIdent`.int) > high(uint8).int
      cpu.f = cpu.f.changeFlags(z = >>zero, n=FUnset,
          h = >>halfCarry, c = >>carry)
    body.add createSet8(operandOne, newValueIdent)

proc genCcf(result: NimNode) {.compileTime.} =
  genGeneric(["CCF"], body, opcs):
    body.add quote do:
      cpu.f = cpu.f.changeFlags(h = FUnset, n = FUnset,
          c = >>(not isFlagSet(cpu.f, BitC)))

proc genAnd(result: NimNode) {.compileTime.} =
  genGeneric(["AND"], body, opcs):
    var reg = operandOne.createGet8()
    body.add quote do:
      cpu.a = cpu.a and `reg`
      cpu.f = cpu.f.changeFlags(z = >>(cpu.a == 0), h = FSet, n = FUnset,
          c = FUnset)

proc genOr(result: NimNode) {.compileTime.} =
  genGeneric(["OR"], body, opcs):
    var reg = operandOne.createGet8()
    body.add quote do:
      cpu.a = cpu.a or `reg`
      cpu.f = cpu.f.changeFlags(z = >>(cpu.a == 0), h = FUnset, n = FUnset,
          c = FUnset)

# --- Prefix CB opcodes start here ---

proc genBit(result: NimNode) {.compileTime.} =
  genGeneric(["BIT"], body, prefixOpcs):
    assert operandOne.kind == IntLit

    let bitNum = operandOne.num
    let src = createGet8(operandTwo)
    body.add quote do:
      # TODO: This may be wrong.
      let zero = (`src` and (1.uint8 shl `bitNum`)) == 0
      cpu.f = cpu.f.changeFlags(>>zero, FUnset, FSet)

proc genRlRla(result: NimNode, rla = false) {.compileTime.} =
  genGeneric(["RL", "RLA"], body, if rla: opcs else: prefixOpcs):
    let reg =
      if opc.mnemonic == "RLA": parseOperandCT("A")
      else: operandOne
    let regRead = reg.createGet8()

    let value = newIdentNode"value"
    let bit7set = newIdentNode"bit7set"

    body.add quote do:
      var `value` = `regRead`
      let `bit7Set` = (`value` and 0x80) == 0x80
      `value` = `value` shl 1

      if cpu.f.isFlagSet(BitC):
        `value` = `value` or 1
      else:
        `value` = `value` and (not 1'u8)

    body.add createSet8(reg, value)

    let z = newIdentNode"z"
    if opc.mnemonic == "RLA":
      body.add quote do:
        var `z` = FUnchanged
    else:
      body.add quote do:
        var `z` = >>(`value` == 0)
    body.add quote do:
      cpu.f =
          cpu.f.changeFlags(z = `z`, n = FUnset,
                            h = FUnset, c = >>`bit7Set`)

proc genRrRra(result: NimNode, rra = false) {.compileTime.} =
  genGeneric(["RR", "RRA"], body, if rra: opcs else: prefixOpcs):
    let reg =
      if opc.mnemonic == "RRA": parseOperandCT("A")
      else: operandOne
    let regRead = reg.createGet8()

    let value = newIdentNode"value"
    let bit0set = newIdentNode"bit0set"

    body.add quote do:
      var `value` = `regRead`
      let `bit0Set` = (`value` and 1) == 1
      `value` = `value` shr 1

      if cpu.f.isFlagSet(BitC):
        `value` = `value` or 0x80

    body.add createSet8(reg, value)

    let z = newIdentNode"z"
    if opc.mnemonic == "RRA":
      body.add quote do:
        var `z` = FUnchanged
    else:
      body.add quote do:
        var `z` = >>(`value` == 0)
    body.add quote do:
      cpu.f =
          cpu.f.changeFlags(z = `z`, n = FUnset,
                            h = FUnset, c = >>`bit0Set`)

proc genSrl(result: NimNode) {.compileTime.} =
  genGeneric(["SRL"], body, prefixOpcs):
    let regRead = operandOne.createGet8()

    let value = newIdentNode"value"
    let bit0Set = newIdentNode"bit0Set"

    body.add quote do:
      var `value` = `regRead`
      let `bit0Set` = (`value` and 1) == 1
      `value` = `value` shr 1

      # 7th bit is reset
      `value` = `value` and (not 0x80'u8)

    body.add createSet8(operandOne, value)

    body.add quote do:
      cpu.f = cpu.f.changeFlags(z = >>(`value` == 0), n = FUnset,
                                h = FUnset, c = >>`bit0Set`)

proc genRlcRlca(result: NimNode, rlca = false) {.compileTime.} =
  genGeneric(["RLC", "RLCA"], body, if rlca: opcs else: prefixOpcs):
    let reg =
      if opc.mnemonic == "RLCA": parseOperandCT("A")
      else: operandOne
    let regRead = reg.createGet8()

    let value = newIdentNode"value"
    let bit7set = newIdentNode"bit7set"

    body.add quote do:
      var `value` = `regRead`
      let `bit7Set` = (`value` and 0x80) == 0x80
      `value` = `value` shl 1

      if `bit7Set`:
        `value` = `value` or 1

    body.add createSet8(reg, value)

    body.add quote do:
      cpu.f =
          cpu.f.changeFlags(n = FUnset,
                            h = FUnset, c = >>`bit7Set`)

proc genCb(result: NimNode) {.compileTime.} =
  genGeneric(["PREFIXCB"], body, opcs):
    var cbAddrIdent = newIdentNode("cbAddr")
    var caseStmt = newNimNode(nnkCaseStmt)
    caseStmt.add cbAddrIdent
    genBit(caseStmt)
    genRlRla(caseStmt)
    genRrRra(caseStmt)
    genRlcRlca(caseStmt)
    genSrl(caseStmt)

    caseStmt.add(newNimNode(nnkElse).add(
      quote do:
        assert false, "CB: " & `cbAddrIdent`.toHex() & ' ' & oldReg.pc.toHex()))

    body.add quote do:
      let prevFlags = cpu.f
      let `cbAddrIdent` = cpu.mem.read8(cpu.pc)
      cpu.pc.inc
      `caseStmt`
      verifyFlags(cpu, prefixOpcs[`cbAddrIdent`], prevFlags)

macro genOpcodeLogic(): stmt =
  ## We want to generate a fast case statement dealing with all opcodes defined
  ## in the opcodes module.

  result = newNimNode(nnkCaseStmt)
  result.add(newIdentNode("opcodeAddr"))

  genNop(result)
  genLoad(result)
  genXor(result)
  genJp(result)
  genJr(result)
  genInc(result)
  genDec(result)
  genCall(result)
  genPush(result)
  genPop(result)
  genRet(result)
  genSubCp(result)
  genRlRla(result, true)
  genRrRra(result, true)
  genRlcRlca(result, true)
  genAdd(result)
  genAdc(result)
  genCcf(result)
  genAnd(result)
  genOr(result)
  genCb(result)
  result.add(
    newNimNode(nnkOfBranch).add(newIntLitNode(0xF3),
      quote do:
        # DI
        cpu.ime = false
        cpu.clock.inc 4))
  result.add(
    newNimNode(nnkOfBranch).add(newIntLitNode(0xFC),
      quote do:
        # EI
        cpu.ime = true
        cpu.clock.inc 4))
  result.add(
    newNimNode(nnkOfBranch).add(newIntLitNode(0x76),
      quote do:
        # HALT
        # Keep looping this instruction to wait for interrupts.
        cpu.pc.dec
        cpu.clock.inc 4))
  result.add(
    newNimNode(nnkOfBranch).add(newIntLitNode(0x10),
      quote do:
        # STOP
        raise newException(DebugError, "STOP")))


  result.add(newNimNode(nnkElse).add(
    quote do:
      assert false, "OPCS: " & opcodeAddr.toHex() & ' ' & oldReg.pc.toHex()))

  #echo(result.toStrLit())

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
  for i in 0x8300 .. 0x83E0:
    data.add(cast[char](cpu.mem.read8(i.uint16)))

  writeFile(getCurrentDir() / "dump8000.mem", data)

  data = ""
  for i in 0x9800 .. 0x9870:
    data.add(cast[char](cpu.mem.read8(i.uint16)))

  writeFile(getCurrentDir() / "dump9900.mem", data)

proc echoTrace(cpu: CPU) =
  var sp = cpu.sp
  echo("Traceback (0x", sp.toHex(), ")")
  while sp.int+2 < high(uint16).int:
    let low = cpu.mem.read8(sp)
    sp.inc
    let high = cpu.mem.read8(sp)
    sp.inc
    let value = (uint16(high) shl 8) or uint16(low)
    if value == 0: break
    let opc = opcs[cpu.mem.read8(value)]
    echo("  SP: 0x$#. 0x$#: $# $#, $# (0x$#)" % [sp.toHex(), value.toHex(),
        opc.mnemonic, opc.operandOne, opc.operandTwo, opc.opcode.toHex(2)])

proc echoMem(cpu: CPU) =
  for i in 0'u8 .. 5'u8:
    echo("0x$#: 0x$#" % [(cpu.pc+i).toHex(), cpu.mem.read8(cpu.pc+i).toHex()])

proc main() =
  var mem = newMemory()
  mem.loadFile(getCurrentDir() / "tetris.gb", getCurrentDir() / "bios.gb")
  var cpu = newCPU(mem)
  cpu.sp = 0xFFFE
  #cpu.pc = 0x100
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
      discard gpu.next(cpu.clock)
    of "b", "break":
      breakpoints.add(split[1].parseHexInt().uint16)
      echo "Breakpoint at 0x", breakpoints[^1].toHex()
    of "bt":
      echoTrace(cpu)
    of "mem":
      echoMem(cpu)
    of "d", "debug":
      enableEchod = not enableEchod
    of "dump":
      dump(cpu)
    of "w", "watch":
      cpu.mem.watchMem.add(split[1].parseHexInt().uint16)
      echo "Memory watch at 0x", cpu.mem.watchMem[^1].toHex()
    of "c", "continue":
      var counter = 0
      var timeCounter = epochTime()

      block cpuLoop:
        while true:
          for brk in breakpoints:
            if cpu.pc == brk:
              echo("Reached 0x", brk.toHex())
              break cpuLoop
          try:
            cpu.next()
          except:
            echo(getStackTrace())
            echo(getCurrentExceptionMsg())
            break cpuLoop
          if gpu.next(cpu.clock): break cpuLoop
          counter.inc cpu.clock
          if epochTime() - timeCounter >= 1.0:
            writeFile(getCurrentDir() / "ips.txt", $counter)
            counter = 0
            timeCounter = epochTime()
    else:
      echoCurrentOpcode(cpu)

when isMainModule:
  main()
