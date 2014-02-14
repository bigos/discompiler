discompiler
===========

Experiment - a bit of disassembler and discompiler

http://en.wikibooks.org/wiki/X86_Disassembly


Structure of PE file
====================

http://www.csn.ul.ie/~caolan/publink/winresdump/winresdump/doc/pefile2.html

http://msdn.microsoft.com/en-us/library/ms809762.aspx

http://en.wikibooks.org/wiki/X86_Disassembly/Windows_Executable_Files

http://code.google.com/p/corkami/wiki/PE

http://www.skyfree.org/linux/references/coff.pdf

http://www.cultdeadcow.com/tools/pewrap.html

http://msdn.microsoft.com/en-us/windows/hardware/gg463119.aspx

Structure of ELF file
=====================

http://man7.org/linux/man-pages/man5/elf.5.html


using Licp Critic
=====================

    DISCOMPILER> (ql:quickload :lisp-critic)
    DISCOMPILER> (lisp-critic-user::critique (+ 1 2 3))
