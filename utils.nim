import unsigned, strutils, macros
type
  FlagState* = enum
    FUnchanged, FSet, FUnset

const enableEchod* = true

const
  BitZ*: uint8 = 0b10000000
  BitN*: uint8 = 0b01000000
  BitH*: uint8 = 0b00100000
  BitC*: uint8 = 0b00010000

proc changeFlag(f: var uint8, state: FlagState, mask: uint8)  =
  case state
  of FSet:
    f = f or mask
  of FUnset:
    f = f and (not mask)
  else: assert false

proc changeFlags*(f: uint8, z = FUnchanged, n = FUnchanged,
                  h = FUnchanged, c = FUnchanged): uint8 =
  result = f
  if z != FUnchanged: changeFlag(result, z, BitZ)
  if n != FUnchanged: changeFlag(result, n, BitN)
  if h != FUnchanged: changeFlag(result, h, BitH)
  if c != FUnchanged: changeFlag(result, c, BitC)

proc `>>`*(b: bool): FlagState =
  if b: return FSet
  else: return FUnset

proc isFlagSet*(f: uint8, mask: uint8): bool = (f and mask) != 0

proc toHex*(x: uint8): string =
  x.BiggestInt.toHex(2)

proc toHex*(x: uint16): string =
  x.BiggestInt.toHex(4)

proc echod*(msg: varargs[string, `$`]) =
  if enableEchod:
    for m in msg:
      stdout.write(m)
    echo()

proc echodReg*(regName: string, oldVal, newVal: uint8 | uint16) =
  echod("Register $1: 0x$2 -> 0x$3" %
        [regName, oldVal.toHex(), newVal.toHex()])

proc echodRegCombo*(regName: string, oldVal1, oldVal2: uint8,
                    newVal: uint16) =
  echod("Register $1: 0x$2$3 -> 0x$4" %
        [regName, oldVal1.toHex(), oldVal2.toHex(),
         newVal.toHex()])

