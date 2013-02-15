# Part of the Nimrod Gameboy emulator.
# Copyright (C) Dominik Picheta.

import colors

type
  TGPUMode = enum
    HBlank = 0, VBlank = 1, OAMRead = 2, VRAMRead = 3, 

  PGPU* = ref object
    vram*: array[0 .. 8191, int32]
    mode*: TGPUMode
    clock: int32
    line: int
    palette: array[0..3, TColor] # 4 colors. 0-1 (Lightest), 7-8 (Darkest)
    scrollY, scrollX: int32

proc newGPU*(): PGPU =
  new(result)
  
  # Set default palette
  result.palette = [colWhite, rgb(192, 192, 192), rgb(96, 96, 96), colBlack]

proc renderLine*(gpu: PGPU) =
  echo("Render Line: ", gpu.line, " scrollX: ", gpu.scrollX, " scrollY: ", gpu.scrollY)

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
      if gpu.line == 143:
        # We reached the bottom edge of the screen (screen is 144 pixels in height.)
        # Enter VBlank
        gpu.mode = VBlank
        
        # TODO: Render surface on screen.
      else:
        gpu.mode = OAMRead
  of VBlank:
    if gpu.clock >= 456:
      gpu.clock = 0
      
      # TODO: according to ref, line should be increased and, should wait
      # for line to be greater than 153? then restart the line and mode?
      echo("Vblank done. Line = ", gpu.line)
      gpu.mode = OAMRead
      gpu.line = 0
      