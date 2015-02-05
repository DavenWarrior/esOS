mkdir "RootDirectory"

Tools\Nasm\nasm -Ox "BootSector.asm" -f bin -o "BootSector.bin"
Tools\FileSplitter "BootSector.bin" "Stage1.bin" 512 "Stage2.bin" *

copy "Stage2.bin" "RootDirectory\Stage2.bin"
Tools\ImageCreator -rootdirectory "RootDirectory" -image "DiskImage.bin" -bootsector "Stage1.bin" -sectorcount 2880 -clustersizeinbytes 512

pause