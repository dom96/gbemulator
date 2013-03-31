# Part of the Nimrod Gameboy emulator.
# Copyright (C) Dominik Picheta.

import colors, strutils, graphics, sdl, times, os

type
  TGPUMode = enum
    HBlank = 0, VBlank = 1, OAMRead = 2, VRAMRead = 3, 

  TBGTile = enum
    TileSet0, TileSet1
  TBGMap = enum
    TileMap0, TileMap1

  TPaletteKind = enum
    PaletteBG, PaletteObj, PaletteObj1

  PGPU* = ref object
    surface*: graphics.PSurface
    vram*: array[0 .. 8191, int32]
    mode*: TGPUMode
    clock: int32
    line*: int
    palette: array[TPaletteKind, array[0..3, colors.TColor]] # 4 colors. 0-1 (Lightest), 7-8 (Darkest)
    scrollY*, scrollX*: int32
    bgtilemap: TBGMap
    bgtileset: TBGTile
    LCDOn: bool
    OBJSize: bool # True(1): 8*16, False(0): 8*8
    OBJOn: bool
    BGOn: bool
    debug: bool

proc newGPU*(): PGPU =
  new(result)
  result.surface = newScreenSurface(257, 256)
  result.surface.fillSurface(colWhite)
  
  # Set default palette
  result.palette[PaletteBG] = [colBlue, rgb(192, 192, 192), rgb(96, 96, 96), colBlack]

proc setLCDC*(gpu: PGPU, b: int32) =
  gpu.BGon = (b and 1) == 1
  gpu.OBJon = (b and (1 shl 1)) == 1
  gpu.OBJsize = (b and (1 shl 2)) == 1
  gpu.bgtilemap = if (b and (1 shl 3)) == 1: TileMap1 else: TileMap0
  # Bit 4: BG & Window tile data select?
  # TODO: Bit 4 could be incorrectly handled?
  gpu.bgtileset = if (b and (1 shl 4)) == 1: TileSet1 else: TileSet0
  
  # TODO: Bit 5: Window display (yes/no)
  echo("LCDC: Bit 5: ", (b and (1 shl 5)).toHex(2))
  # TODO: Bit 6: Window Tile Map Display Select?
  echo("LCDC: Bit 6: ", (b and (1 shl 6)).toHex(2))
  gpu.LCDon = (b and (1 shl 7)) == 1

proc getTileAddr(gpu: PGPU, index: int32): int =
  ## Returns the address of the **beginning** of the tile at ``index``.
  case gpu.bgtileset
  of TileSet0:
    return 0x0000+(index*16) # Each tile is 16 bytes (128 bits)
  of TileSet1:
    return 0x1000+(index*16)

proc getTileRow(gpu: PGPU, index, line: int32): array[0..7, int32] =
  let tileAddr = getTileAddr(gpu, index)
  #if gpu.debug and index != 0:
  #echo("  getTileRow: ", tileAddr.toHex(4), " ", index, " ", line)
  var i = line*2
  let lowRow = gpu.vram[tileAddr+i]
  
  #if lowRow != 0: echo(lowRow, " ", lowRow.toBin(32), " ", tileAddr.toHex(4))
  let highRow = gpu.vram[tileAddr+i+1]
  
  #if highRow != 0: echo(highRow, highRow.toBin(32))
  for px in 0 .. 7:
    let lowBit = if (lowRow and (1 shl (7-px))) == 0: 0 else: 1 
    let highBit = if (highRow and (1 shl (7-px)))  == 0: 0 else: 2
    #echo("  px: ", px, "; ", lowBit, "; ", highBit, ". ",toBin(lowRow and (1 shl (7-px)), 10))
    result[px] = (lowBit or highBit).int32
    #if lowRow != 0 or highRow != 0: echo(lowBit, " ", highBit)
    assert(result[px] >= 0 and result[px] < 4)

proc getTileRow(gpu: PGPU, index: int32): array[0..7, int32] =
  let tileAddr = getTileAddr(gpu, index)
  let y = (gpu.line + gpu.scrollY) and 7
  #echo("  getTileRow: ", tileAddr.toHex(4), " ", index, " ", gpu.line, " ", gpu.scrollY)
  var i = y
  for px in 0 .. 7:
    result[px] = (gpu.vram[y+tileAddr+i] shl 8) or gpu.vram[y+tileAddr+i+1]
    i.inc 2

proc echoTile(tile: array[0..7, int32]) =
  stdout.write("  [")
  for i in 0 .. 7:
    stdout.write($tile[i] & ", ")
  echo(" ]")

proc renderLine*(gpu: PGPU) =
  #echo("Render Line: ", gpu.line, " scrollX: ", gpu.scrollX, " scrollY: ", gpu.scrollY)
  var mapOffset = if gpu.bgTileMap == TileMap0: 0x1800 else: 0x1C00
  assert gpu.bgTileMap == TileMap0
  # Get the line of tiles to use.
  mapOffset.inc(((gpu.line + gpu.scrollY) and 0xFF) shr 3)
  #echo("  MapOffset: ", mapOffset.toHex(4))  
  
  let lineOffset = (gpu.scrollX shr 3) # Which tile to start with in the map line
  
  # Get tile index from background map
  var tileIndex = gpu.vram[mapOffset + lineOffset]
  #echo("  mapOffset: ", mapOffset.toHex(4))
  #echo("Tile index: ", tileIndex, " @ ", (0x8000 + mapOffset).toHex(4))
  
  #echo("bgtileset: ", gpu.bgtileset)

  
  let tile = getTileRow(gpu, tileIndex)
  let x = gpu.scrollX and 7
  let surfaceOffset = gpu.line * 160 * 4

  #echo("  TileIndex: ", tileIndex, ". X: ", x, ". Y: ", y) 
  #echoTile(tile)

proc renderTile(gpu: PGPU, a, x, y: int) =
  ## ``a`` is the address of the tile number in the tile map.
  let tileIndex = gpu.vram[a]

  let tileAddr = getTileAddr(gpu, tileIndex)
  if gpu.debug:
    if tileIndex != 0:
      echo("a: 0x", a.toHex(4), "; index: ", tileIndex, "; tileAddr: 0x", tileAddr.toHex(4))
      echo("  x: ", x, "; y: ", y)
  for tileY in 0 .. 7:
    var tileRow = getTileRow(gpu, tileIndex, tileY.int32)
    for tileX in 0 .. 7:
      #echo(tileRow[tileX])
      gpu.surface[x + tileX, y + tileY] = gpu.palette[PaletteBG][tileRow[tileX]]


  #gpu.surface.fillRect((x, y, 8, 8), colgreen)
  #gpu.surface.drawRect((x, y, 8, 8), colMagenta)

proc renderDebugFullMap(gpu: PGPU) =
  let mapOffset = if gpu.bgTileMap == TileMap0: 0x1800 else: 0x1C00
  var x = 0
  var y = 0
  for i in 0 .. (32*32)-1:
    #echo("renderTile: ", x, " ", y, " ", i)
    
    renderTile(gpu, mapOffset + i, x * 8, y * 8)
    if x == 31:
      x = 0
      y.inc
    else:
      x.inc
  if gpu.debug:
    sleep(5000)

proc next*(gpu: PGPU, time: int) =
  gpu.clock.inc(time)
  #echo("GPU Mode: ", gpu.mode)
  case gpu.mode
  of OAMRead:
    if gpu.clock >= 80:
      # Enter VRAMRead
      gpu.mode = VRAMRead
      gpu.clock = 0
  of VRAMRead:
    if gpu.clock >= 172:
      # Enter HBlank
      gpu.mode = HBlank
      gpu.clock = 0
      
      # Render scanline
      gpu.renderLine()
  of HBlank:
    if gpu.clock >= 204:
      gpu.clock = 0
      gpu.line.inc
      #echo("HBlank line: ", gpu.line)
      if gpu.line == 144:
        # We reached the bottom edge of the screen (screen is 144 pixels in height.)
        # Enter VBlank
        gpu.mode = VBlank
        
        # TODO: Render surface on screen.
      else:
        gpu.mode = OAMRead
  of VBlank:
    if gpu.clock >= 456:
      gpu.clock = 0
      
      # The line should be increased between 144 and 153 during v-blank.
      # BIOS checks this for example.
      
      if gpu.line == 153:
        #echo("Vblank done. Line = ", gpu.line)
        renderDebugFullMap(gpu)
        updateRect(gpu.surface.s, 0, 0, 256, 256)
        
        gpu.mode = OAMRead
        gpu.line = 0
      else:
        gpu.line.inc
  
  #gpu.surface.
  # SDL
  var event: TEvent
  if pollEvent(addr(event)) == 1:
    case event.kind:
    of SDL.QUITEV:
      quit(QuitSuccess)
    of SDL.KEYDOWN:
      var evk = sdl.EvKeyboard(addr event)
      if evk.keysym.sym == SDL.K_SPACE:
        gpu.debug = not gpu.debug
        
        # Dummy vram info
        gpu.vram[0x0010] = 0x7C
        gpu.vram[0x0011] = 0x7C
        gpu.vram[0x0012] = 0x00
        gpu.vram[0x0013] = 0xC6
        gpu.vram[0x0014] = 0xC6
        gpu.vram[0x0015] = 0x00
        gpu.vram[0x0016] = 0x00
        gpu.vram[0x0017] = 0xFE
        gpu.vram[0x0018] = 0xC6
        gpu.vram[0x0019] = 0xC6
        gpu.vram[0x001A] = 0x00
        gpu.vram[0x001B] = 0xC6
        gpu.vram[0x001C] = 0xC6
        gpu.vram[0x001D] = 0x00
        gpu.vram[0x001E] = 0x00
        gpu.vram[0x001F] = 0x00
       
        
      else:
        echo(evk.keysym.sym)
    else:
      nil
  
  