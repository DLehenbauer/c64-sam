# Notes
This file documents the process used to disassemble and document the SAM source code from the C64 version.

## Disassembling
* In VICE, load SAM and RECITER into low memory
* Execute code at $97e2-97e8 to map LOMEM at $A000-$BFFF
* Create dump file (need $7d00-$bfff)
* Trace entry points with [WFDis](https://www.white-flame.com/wfdis/)
    * $9500 installs the SAM wedge
    * $950D executes the wedge, which reaches the rest of the code
* Mechanically convert to [CA65](https://cc65.github.io/doc/ca65.html) syntax
    * A lot of this was semi-automated with Vim macros and regexes:
        * Add `:` after labels: `'<,'>-1s/^\([LS][0-9a-f]\{4\}\)/\1:^M     /g`
        * Convert hex into '.byte' statements:
            * Insert `, $` between bytes: `'<,'>-1s/\(\w\) \(\w\)\1, $\2/g`
            * Add `.byte ` prefix: `'<,'>-1s/^\(\s*\)\1.byte $/g`
    * Node scripts were used to decode some tables (see /reference/tools/*)
* Authored custom 'c64.cfg' to place SAM at $7D00

## Running SAM
Author stub 'startup.s' to install wedge (`jsr $9500`).

### Important Addresses
Documented on page 35 of the [C64 manual](https://ia800404.us.archive.org/2/items/Commodore64SoftwareAutomaticMouthManual/Commodore64SoftwareAutomaticMouthManual.pdf).
HEX | Function | Example | Notes
-|-|-|-
$9500 | (Re)install S.A.M. Wedge | SYS 38144 |
$97E0 | Throat | POKE 38880, *n* | (Default 128)
$97E1 | Mouth | POKE 38881, *n* | (Default 128)
$9A00 | S.A.M. from BASIC | SYS 39424 | 
$9A03 | S.A.M. from ML | JSR $9A03 | 
$9A06 | RECITER from BASIC | SYS 39430 | 
$9A09 | RECITER from ML | JSR $9A09 | 
$9A0E | SPEED | POKE 39438, *n* | (Default 72)
$9A0F | PITCH | POKE 39439, *n* | (Default 64)
$9A10 | LIGHTS | POKE 39440, *n* | (Default 0 = off)
$9A11 | INTERRUPTS | POKE 39441, *n* | (Default 0 = disabled)
$9A15 | TEXT | (ASCII string terminated by $9b) | 

## Tidying
### Replace Hardcoded Addresses
Identified by relocating SAM and using a memory breakpoint to detect access to the previous memory region.
* $9271, $928b: Split pointer table to alphabetic rules at $7d00-$8cda
* $92a5: LTS rules for non-alphanumeric characters
* $Lb3c0: Sample table
* $a800-$af00: Rendering tables (**Note:** Must be page aligned)

### Replace padding/uninitialized vars with '.res'
Identified through code inspection aided by memory breakpoints.

## Bugs Fixed
During the process of documenting SAM, I found and fixed a few minor/benign bugs:
* '?' did not cause a rising inflection when using RECITER due to '(?)=.'
* A benign bug where 'e_rules' pointed to $8039 instead of $803f.
* 'jsr' to $a439 (which is mid-instruction).  Probably benign as I think this code is unreachable.

## Documenting SAM
The reverse-engineered comments borrow liberally from pre-existing efforts:

* [C64 disassembly](http://hitmen.c02.at/html/tools_sam.html) by Groepaz
* ['C' port](https://github.com/s-macke/SAM) by Sebastian Macke
    * [Refactor](https://github.com/vidarh/SAM) by Vidar Hokstad
    * [Refactor](https://github.com/bit-hack/SAM) by Aidan Dodds
* ['C#' port](https://gitdab.com/Simon/SamSharp) by Simon

## Reference
* SAM
    * Manual ([C64 PDF](https://ia800404.us.archive.org/2/items/Commodore64SoftwareAutomaticMouthManual/Commodore64SoftwareAutomaticMouthManual.pdf), [C64 TXT](https://the-cbm-files.tripod.com/speak/samdoc.txt), [Atari MD](https://github.com/discordier/sam/blob/master/docs/manual.md))
    * [Article](https://habr-com.translate.goog/ru/post/500764/?_x_tr_sl=auto&_x_tr_tl=en) by Artyom Skrobov (from [Russian](https://habr.com/en/post/500764/))
    * [SAM2600](https://github.com/rossumur/SAM2600#a-brief-history-of-speech-synthesis) by Peter Barrett
* C64
    * [PETSCII](https://www-c64--wiki-de.translate.goog/wiki/PETSCII-Tabelle?_x_tr_sl=de&_x_tr_tl=en&_x_tr_hl=en-US&_x_tr_pto=wapp) (from [German](https://www.c64-wiki.de/wiki/PETSCII-Tabelle))
    * [6502 Branching](http://www.6502.org/tutorials/compare_beyond.html)
    * [C64 KERNAL functions](https://sta.c64.org/cbm64krnfunc.html)
    * [CHRGET](https://www-c64--wiki-de.translate.goog/wiki/CHRGET?_x_tr_sl=de&_x_tr_tl=en&_x_tr_hl=en-US&_x_tr_pto=wapp) (from [German](https://www.c64-wiki.de/wiki/CHRGET?msclkid=6b32d4daaf6611eca15046311c7d18f6))
