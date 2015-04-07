import graphics, colors, mem2, sdl, utils, unsigned

type
  GPU = ref object
    surface: graphics.PSurface
    mem: Memory

    mode: GpuMode
    clock: int

  GpuMode = enum
    HBlank = 0, VBlank = 1, OamRead = 2, VramRead = 3

  LCDC = object
    displayEnabled: bool
    winTileMap: bool # False for 0x9800-0x9BFF, True for 0x9C00-0x9FFF.
    winDisplayEnabled: bool
    winBgTileData: bool # False for 0x8800-0x97FF, True for 0x8000-0x8FFF.
    bgTileMap: bool # False for 0x9800-0x9BFF, True for 0x9C00-0x9FFF.
    spriteSize: bool # (Obj Size). False for 8x8, True for 8*16
    spriteDisplayEnabled: bool
    bgDisplayEnabled: bool

proc newGPU*(mem: Memory): GPU =
  new result
  result.surface = newScreenSurface(640, 480)
  initDefaultFont(r"C:\Windows\Fonts\DejaVuSans.ttf", 12)

  result.mem = mem

proc drawMem*(gpu: GPU) =
  for i in countup(0, 0x100, 2):
    let address = 0x8000'u16 + i.uint16
    let value = gpu.mem.read16(address)
    let pos = i
    #assert(value == 0, address.toHex())
    gpu.surface.drawText((400, 5 + (10*pos)), "0x" & address.toHex())
    gpu.surface.drawText((460, 5 + (10*pos)), "0x" & value.toHex())

proc getLCDC(gpu: GPU): LCDC =
  let ff40 = gpu.mem.read8(0xFF40)
  result.displayEnabled = (ff40 and (1 shl 7)) != 0
  result.winTileMap = (ff40 and (1 shl 6)) != 0
  result.winDisplayEnabled = (ff40 and (1 shl 5)) != 0
  result.winBgTileData = (ff40 and (1 shl 4)) != 0
  result.bgTileMap = (ff40 and (1 shl 3)) != 0
  result.spriteSize = (ff40 and (1 shl 2)) != 0
  result.spriteDisplayEnabled = (ff40 and (1 shl 1)) != 0
  result.bgDisplayEnabled = (ff40 and 1) != 0

proc getPaletteBg(gpu: GPU): array[4, Color] =
  let ff47 = gpu.mem.read8(0xFF47)
  for i in 0 .. 3:
    let shade = (ff47 shr (i.uint8 * 2)) and 0b11
    case shade
    of 0: result[i] = colWhite
    of 1: result[i] = rgb(192, 192, 192)
    of 2: result[i] = rgb(96, 96, 96)
    of 3: result[i] = colBlack

proc next*(gpu: GPU, clock: int) =
  gpu.surface.fillSurface(colWhite)

  gpu.clock.inc clock

  case gpu.mode
  of OamRead:
    if gpu.clock >= 80:
      gpu.mode = VRAMRead
      gpu.clock = 0
  of VramRead:
    if gpu.clock >= 172:
      gpu.mode = HBlank
      gpu.clock = 0

      # Render scanline
      #gpu.renderLine()
  of HBlank:
    if gpu.clock >= 204:
      gpu.clock = 0

      let ly = gpu.mem.read8(0xFF44)

      #echo("HBlank line: ", gpu.line)
      if ly == 144:
        # We reached the bottom edge of the screen (screen is 144 pixels in height.)
        gpu.mode = VBlank
        gpu.mem.requestInterrupt(0)
      else:
        gpu.mode = OamRead

      gpu.mem.write8(0xFF44, ly+1)
  of VBlank:
    if gpu.clock >= 456:
      gpu.clock = 0

      let ly = gpu.mem.read8(0xFF44)

      if ly == 153:
        gpu.mode = OAMRead
        gpu.mem.write8(0xFF44, 0)
      else:
        gpu.mem.write8(0xFF44, ly+1)

  var event: TEvent
  if pollEvent(addr(event)) == 1:
    case event.kind:
    of sdl.QUITEV:
      quit(QuitSuccess)
    of sdl.KEYDOWN:
      var evk = sdl.evKeyboard(addr event)
      if evk.keysym.sym == sdl.K_SPACE:
        gpu.surface.drawText((400, 5), "Space")
      else:
        echo(evk.keysym.sym)
    else:
      nil

    drawMem(gpu)

    sdl.updateRect(gpu.surface.s, 0, 0, 640, 480)
