# BootLPT/86

A project that lets you boot any PC, based on an 8086 and up, from a standard parallel port!

## Background

Once I was in progress of reviving a vintage *"diskless workstation"* based on an 8086 CPU.
This was an IBM PC-compatible computer built into a single keyboard that, apart from a hopelessly outdated ARCNET network card built into it, contained absolutely no provisions of connecting a disk drive; not even a floppy controller. It didn't have any kind of a system bus connector accessible either. 
So it really was designed to work from a network.

But it did have a serial and a parallel port - which was enough for me to decide not to ditch that thing!

## Inspiration

The project stems its inspiration from the *[XTIDE Universal BIOS project](http://www.xtideuniversalbios.org/)*, whose newest revisions allow you to boot a computer from a serial port. And also from the *[ROMOS project](http://rayer.g6.cz/romos/romose.htm)*, that demonstrates embedding FreeDOS into your computer's BIOS chip, or to a separate bootable PROM chip.

However, my rig was limited to 9600 bps with its standard serial port hardware. That translated to just over a kilobyte per second data transfer. And while I could drop in a separate ROM chip to boot, it didn't accept anything over 8K in size, which is hopelessly small to hold any reasonable x86 OS kernel on it.

The result was **BootLPT/86** - an 8086-compatible bootable code, just *over a kilobyte* in size, allowing you to load and boot a disk image through the classic printer port, being over 18 times faster than the classic COM port!

# What you'll need

 - A computer with an LPT port to boot.
 - A second computer with a parallel interface, that will provide the bootable image.
   Can be running **DOS** or **Windows**, from 9x all the way up to Windows 10.
   Future support for an **Arduino** to be used as a "server" is planned.
   **USB to LPT** adapters are also an option.
 - A crossed LPT cable, also known as a "LapLink cable".
 - General Electronics and Engineering Knowledge (a.k.a. you're a GEEK).
  You'll have to think of a way on how to make your computer equipped with **BootLPT/86**.
  This means either adding the required code to your **BIOS**, or installing a special boot (E/EE)**PROM**.
  You are also going to experiment with where to load the bootable disk.
  Under real mode, of course. That is, under the first megabyte of your RAM. :)

# More information

On how to compile, install and use - check the INSTALL.md file!

## License

The BootLPT/86 project is released under the [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html).
