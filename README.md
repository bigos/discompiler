discompiler
===========

Experiment - a bit of disassembler and discompiler

http://en.wikibooks.org/wiki/X86_Disassembly


Structure of PE file
====================

http://www.reversinglabs.com/advisory/pecoff.html

http://www.csn.ul.ie/~caolan/publink/winresdump/winresdump/doc/pefile2.html

http://msdn.microsoft.com/en-us/library/ms809762.aspx

http://en.wikibooks.org/wiki/X86_Disassembly/Windows_Executable_Files

http://code.google.com/p/corkami/wiki/PE

http://www.skyfree.org/linux/references/coff.pdf

http://www.cultdeadcow.com/tools/pewrap.html

http://msdn.microsoft.com/en-us/windows/hardware/gg463119.aspx

PE Loading Process
===============

http://download.microsoft.com/download/1/4/0/14045A9E-C978-47D1-954B-92B9FD877995/97807356648739_SampleChapters.pdf  page 373

There are discrepanciess between PE file format and actual handling by the loader.

http://www.kaspersky.co.uk/images/alexander_liskin_-_pe_specification_vs_pe_loader.pdf

http://media.blackhat.com/bh-us-11/Vuksan/BH_US_11_VuksanPericin_PECOFF_Slides.pdf slides 17, 39


Structure of ELF file
=====================

http://man7.org/linux/man-pages/man5/elf.5.html



using Lisp Critic
=====================

    DISCOMPILER> (ql:quickload :lisp-critic)
    DISCOMPILER> (lisp-critic-user::critique (+ 1 2 3))

running tests
=============

    CL-USER> (ql:quickload :discompiler)
    CL-USER> (in-package :discompiler)
    DISCOMPILER> (run! :pe-coff)
