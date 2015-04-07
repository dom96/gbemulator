# gbemulator

Inspired by the Rust NES emulator I have decided to write a Gameboy emulator in
my favourite programming language.

I have never written an emulator before and because writing one requires a lot
experimentation, the code may not be the cleanest for a while as I am constantly
using many debugging statements to figure out where I'm going wrong.

## Status

<del>Currently the emulator only gets as far as displaying the Nintendo logo.</del>

We get to **0x0064** in the BIOS!

0x0064 in the BIOS waits for the vertical-blank period, since we have no GPU
hooked up yet it waits for that forever.

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
