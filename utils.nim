import unsigned, strutils
type
  FlagState* = enum
    FUnchanged, FSet, FUnset

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
