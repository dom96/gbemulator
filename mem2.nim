import unsigned, strutils, utils
type
  Memory* = ref object
    rom: string
    mem: array[0xFFFF, uint8]

proc newMemory*: Memory =
  new result

proc loadFile*(m: Memory, file: string) =
  m.rom = readFile(file)

  for i in 0 .. min(m.rom.len-1, 0xFFFF):
    m.mem[i] = m.rom[i].uint8

proc read8*(m: Memory, address: uint16): uint8 =
  return m.mem[address]

proc read16*(m: Memory, address: uint16): uint16 =
  return (m.mem[address+1].uint16 shl 8) or m.mem[address]

proc write8*(m: Memory, address: uint16, data: uint8) =
  let old = m.mem[address]
  m.mem[address] = data
  echo("Memory 0x$1: 0x$2 -> 0x$3" %
     [address.toHex(), old.toHex(), data.toHex()])

proc write16*(m: Memory, address: uint16, data: uint16) =
  m.mem[address] = (data shr 8).uint8
  m.mem[address+1] = ((data shl 8) shr 8).uint8
