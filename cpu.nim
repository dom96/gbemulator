# Part of the Nimrod Gameboy emulator.
# Copyright (C) Dominik Picheta.
import mem, gpu
import strutils
type
  # m (machine cycles), t (time cycles).
  # Ref: http://www.zilog.com/docs/z80/um0080.pdf
  TClock = tuple[m, t: int]

  PRegister = ref object
    pc, sp: int32 # 16-bit
    a, b, c, d, e, H, L, f: int32 # 8-bit
    clock: TClock

  PCPU = ref object
    clock: TClock
    r: PRegister
    mem: PMem

  TFlagState = enum
    FUnchanged, FSet, FUnset

const
  BitZ = 1 shl 7
  BitN = 1 shl 6
  BitH = 1 shl 5
  BitC = 1 shl 4

## Flags
## -----
## 0x80 (1 shl 7) - (Zero|Z) Last result was zero.
## 0x40 (1 shl 6) - (Operation|N) Set if last operation was a subtraction
## 0x20 (1 shl 5) - (Half carry|H)
## 0x10 (1 shl 4) - (Carry|C)
## 0x08 (1 shl 3) - (Sign flag|S)               NOT USED IN GB's Z80
## 0x04 (1 shl 2) - (Parity/Overflow Flag|P/V)  NOT USED IN GB's Z80

proc newCPU(mem: PMem): PCPU =
  new(result)
  new(result.r)
  result.mem = mem

# Split a 16-bit into 8-bit: let (a, b) = (x shr 8 and 0xff, x and 0xff)

template changeFlag(f: var int32, state: TFlagState, bit: int32)  =
  case state
  of FSet:
    f = f or bit
  of FUnset:
    f = f and (not bit)
  else: assert false

template changeFlags(cpu: PCPU, Z = FUnchanged, N = FUnchanged,
                     H = FUnchanged, C = FUnchanged) =
  if Z != FUnchanged: changeFlag(cpu.r.f, Z, BitZ)
  if N != FUnchanged: changeFlag(cpu.r.f, N, BitN)
  if H != FUnchanged: changeFlag(cpu.r.f, H, BitH)
  if C != FUnchanged: changeFlag(cpu.r.f, C, BitC)

template isFSet(cpu: PCPU, bit: int32): bool = (cpu.r.f and bit) != 0

proc `>>`(b: bool): TFlagState =
  if b: return FSet
  else: return FUnset

template LDrn(cpu: PCPU, register: expr) {.immediate.} =
  cpu.r.register = cpu.mem.readByte(cpu.r.pc)
  inc(cpu.r.pc)
  cpu.r.clock.m = 2

proc exec(cpu: PCPU) =
  ## Executes the next instruction
  let opcode = cpu.mem.readByte(cpu.r.pc)
  #echo("OPCODE: 0x", toHex(opcode, 2))
  cpu.r.pc.inc()
  case opcode
  of 0x0C:
    # INC c
    # Increment C
    cpu.r.c = (cpu.r.c + 1) and 0xFF
    cpu.r.clock.m = 1
    cpu.changeFlags(Z = >>(cpu.r.c == 0), H = >>((cpu.r.c and 0xF) == 0),
                    N = FUnset)
  of 0x0E:
    # LD C, n
    # Load 8-bit immediate into C.
    LDrn(cpu, c)
  of 0x3E:
    # LD A, n
    # Load 8-bit immediate into A.
    LDrn(cpu, a)
    
  of 0x20:
    # JR NZ, n
    # Relative jump by signed immediate if last result was not zero
    var x = cpu.mem.readByte(cpu.r.pc)
    if x > 127: x = -(((not x) + 1) and 255)
    cpu.r.pc.inc
    cpu.r.clock.m = 2
    if not isFSet(cpu, BitZ): cpu.r.pc.inc(x); cpu.r.clock.m.inc 
  of 0x21:
    # LD HL, nn
    # Load 16-bit immediate into (registers) H and L
    cpu.r.L = cpu.mem.readByte(cpu.r.pc)
    cpu.r.H = cpu.mem.readByte(cpu.r.pc+1)
    cpu.r.pc.inc(2)
    cpu.r.clock.m = 3
  
  of 0x31:
    # LD SP, nn
    # Load 16-bit immediate into (register) SP
    cpu.r.sp = cpu.mem.readWord(cpu.r.pc)
    cpu.r.pc.inc(2)
    cpu.r.clock.m = 3
  of 0x32:
    # LDD (HL), A
    # Save A to address pointed by HL, and decrement HL
    cpu.mem.writeByte((cpu.r.h shl 8) or cpu.r.l, cpu.r.a)
    let x = ((cpu.r.h shl 8) or cpu.r.l) - 1
    let (hi, low) = (x shr 8 and 0xFF, x and 0xFF)
    cpu.r.L = low; cpu.r.H = hi
    cpu.r.clock.m = 2
  
  of 0xAF:
    # XOR A
    # Logical XOR against (register) A
    cpu.r.a = (cpu.r.a xor cpu.r.a) and 255 # If result is bigger than 255, will be set to 0
    cpu.changeFlags(Z = >>(cpu.r.a == 0), H = FUnset, C = FUnset)
    cpu.r.clock.m = 1
  
  of 0xCB:
    # Extended Ops
    let extop = cpu.mem.readByte(cpu.r.pc)
    cpu.r.pc.inc
    case extop
    of 0x7C:
      # BIT 7, H
      # Test whether bit 7 of H is zero
      cpu.changeFlags(Z = >>((cpu.r.h and (1 shl 7)) == 0), H = FSet, N = FUnset)
      cpu.r.clock.m = 2
    else:
      echo "Unknown extended op: 0x", extop.toHex(2)
      assert false
  
  of 0xE2:
    # LDH (0xFF00 + C), A
    # Save A at address pointed to by 0xFF00+C
    cpu.mem.writeByte(0xFF00 + cpu.r.c, cpu.r.a)
    cpu.r.clock.m = 2
  else:
    echo "Unknown opcode: 0x", opcode.toHex(2)
    assert false

proc next*(cpu: PCPU) =
  cpu.exec()
  cpu.mem.gpu.next(cpu.r.clock.m)


when isMainModule:
  var cpu = newCpu(mem.load("/home/dom/code/nimrod/gbemulator/Pokemon_Red.gb"))
  while True:
    cpu.next()