#!/bin/bash
set -e -o pipefail
echo Building boot files...
./configure --pb -m=$MACH --disable-x11 --kernelobj --disable-curses
make ${MACH}.bootquick -j$(nproc)
echo Building Chez Scheme...
./configure -m=$MACH --disable-x11 --kernelobj --disable-curses
make -j$(nproc)
case $MACH in
  *a6nt)
    curl -Ls https://github.com/burgerrg/win-iconv/releases/download/v0.0.9/iconv-x64.dll > "$TARGET_MACHINE"/bin/"$TARGET_MACHINE"/iconv.dll
    ;;
  *i3nt)
    curl -Ls https://github.com/burgerrg/win-iconv/releases/download/v0.0.9/iconv-x86.dll > "$TARGET_MACHINE"/bin/"$TARGET_MACHINE"/iconv.dll
    ;;
esac
