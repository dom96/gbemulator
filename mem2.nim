import unsigned, strutils, utils
type
  Memory* = ref object
    rom: string
    mem: array[0xFFFF+1, uint8]
    watchMem*: seq[uint16] ## For debugging

proc newMemory*: Memory =
  new result
  result.watchMem = @[]

proc loadFile*(m: Memory, file, bios: string) =
  m.rom = readFile(file)
  var bios = readFile(bios)

  for i in 0 .. min(m.rom.len-1, 0xFFFF):
    m.mem[i] = m.rom[i].uint8

  for i in 0 .. bios.len-1:
    m.mem[i] = bios[i].uint8

proc read8*(m: Memory, address: uint16): uint8 =
  return m.mem[address]

proc read16*(m: Memory, address: uint16): uint16 =
  return (m.mem[address+1].uint16 shl 8) or m.mem[address]

proc write8*(m: Memory, address: uint16, data: uint8) =
  let old = m.mem[address]
  m.mem[address] = data
  echod("Memory 0x$1: 0x$2 -> 0x$3" %
       [address.toHex(), old.toHex(), data.toHex()])
  if address in m.watchMem:
    raise newException(DebugError, "WatchMem")

proc write16*(m: Memory, address: uint16, data: uint16) =
  m.mem[address] = (data shr 8).uint8
  m.mem[address+1] = ((data shl 8) shr 8).uint8

proc requestInterrupt*(m: Memory, bit: range[0 .. 4]) =
  let interruptFlag = m.read8(0xFF0F)
  m.write8(0xFF0F, interruptFlag or (1'u8 shl bit))
