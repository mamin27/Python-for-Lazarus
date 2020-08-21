#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking /home/comet/project/lazarus/Python-for-Lazarus/demo_lazarus/Demo09/libdemodll.so
OFS=$IFS
IFS="
"
/usr/bin/ld.bfd -b elf64-x86-64 -m elf_x86_64  -init FPC_SHARED_LIB_START -fini FPC_LIB_EXIT -soname libdemodll.so -shared -L. -o /home/comet/project/lazarus/Python-for-Lazarus/demo_lazarus/Demo09/libdemodll.so /home/comet/project/lazarus/Python-for-Lazarus/demo_lazarus/Demo09/link.res
if [ $? != 0 ]; then DoExitLink /home/comet/project/lazarus/Python-for-Lazarus/demo_lazarus/Demo09/libdemodll.so; fi
IFS=$OFS
