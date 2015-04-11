# gbemulator

Inspired by the Rust NES emulator I have decided to write a Gameboy emulator in
my favourite programming language.

I have never written an emulator before and because writing one requires a lot
experimentation, the code may not be the cleanest for a while as I am constantly
using many debugging statements to figure out where I'm going wrong.

## Status

<del>Currently the emulator only gets as far as displaying the Nintendo logo.</del>

We get to **0x0086** in the BIOS!

0x0064 in the BIOS waits for the vertical-blank period, since we have no GPU
hooked up yet it waits for that forever.

The speed: current average 4,084,472 cycles per second. (in release mode)

----

Now we get to **0x002E** in the BIOS!

### Performance numbers

These are the Clock cycles per second using bench.gb.

```
1,727,968 - Abstracted - ~-50%
13,671,816 - Abstraced with -d:release - 225%
11,659,384 - Abstracted (but operands replaced)
219,323,976 - Abstracted (but operands replaced) with -d:release - 5375%
12,779,408 - As fast as possible
17,344,544 - As fast as possible with -d:release
344,247,552 - As fast as possible with -d:release, no gpu - 8500%

344,000,000-4,000,000 = 340,000,000
340,000,000/4,000,000 = 85*100 = 8500% increase in speed vs. GB
```

## Development

### Debugging

The bgb emulator was very handy for debugging.

## References

http://www.devrs.com/gb/files/gbspec.txt

http://www.zilog.com/docs/z80/um0080.pdf

https://github.com/Two9A/jsGB/blob/master/js

https://github.com/grantgalitz/GameBoy-Online/blob/master/js/GameBoyCore.js

http://imrannazar.com/Gameboy-Z80-Opcode-Map

http://www.zophar.net/fileuploads/2/10816buusf/z80opcod.txt

http://gekkio.fi/blog/2015-01-13-mooneye-gb-a-gameboy-emulator-written-in-rust.html

https://realboyemulator.wordpress.com/2013/01/03/a-look-at-the-game-boy-bootstrap-let-the-fun-begin/

http://datacrystal.romhacking.net/wiki/Pok%C3%A9mon_Yellow

http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf

http://www.codeslinger.co.uk/pages/projects/gameboy/lcd.html

https://code.google.com/p/megaboy/wiki/Interrupts

http://imrannazar.com/GameBoy-Emulation-in-JavaScript:-GPU-Timings

http://blog.kevtris.org/blogfiles/Nitty%20Gritty%20Gameboy%20VRAM%20Timing.txt

https://slashbinbash.wordpress.com/2013/02/07/gameboy-tile-mapping-between-image-and-memory/

GB Test ROMs - http://blargg.8bitalley.com/parodius/gb-tests/
