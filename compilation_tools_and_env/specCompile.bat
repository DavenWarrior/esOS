mkdir "RootDirectory"

Tools\Nasm\nasm -Ox "C:\cygwin64\home\david\asmTest\asmTest.asm" -f bin -o "C:\cygwin64\home\david\asmTest\asmTest.bin"
Tools\FileSplitter "C:\cygwin64\home\david\asmTest\asmTest.bin" "C:\cygwin64\home\david\asmTest\Stage1.bin" 512 "Stage2.bin" *

Tools\ImageCreator -rootdirectory "RootDirectory" -image "C:\cygwin64\home\david\asmTest\asmTest.bin" -bootsector "C:\cygwin64\home\david\asmTest\Stage1.bin" -sectorcount 2880 -clustersizeinbytes 512

pause