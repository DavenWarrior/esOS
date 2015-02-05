mkdir "RootDirectory"

Nasm\nasm C:\cygwin64\home\david\asmTest\asmTest.asm -f bin -o C:\cygwin64\home\david\asmTest\asmTest.bin
Tools\FileSplitter C:\cygwin64\home\david\asmTest\asmTest.bin C:\cygwin64\home\david\asmTest\Stage1.bin 512 C:\cygwin64\home\david\asmTest\Stage2.bin *

copy C:\cygwin64\home\david\asmTest\Stage2.bin RootDirectory\Stage2.bin
Tools\ImageCreator -rootdirectory RootDirectory -image C:\cygwin64\home\david\asmTest\bootsector.bin -bootsector C:\cygwin64\home\david\asmTest\Stage1.bin -sectorcount 2880 -clustersizeinbytes 512
Tools\RawCopy -source "C:\cygwin64\home\david\asmTest\bootsector.bin" -destination "C:\Users\david\VirtualBox VMs\myOS2\NewVirtualDisk1.vhd"

pause