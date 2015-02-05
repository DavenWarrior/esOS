[BITS 16]
[ORG 0x7C00]


;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;
;;  Stage 1
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
jmp 0x0:Stage1Start

Stage1Start:
cli                 ;Turn off interrupts

;Setup the stack
mov AX,0x9000
mov SS,AX			;Stack segment begins at 0x90000
mov SP,0xFB00		;Stack pointer points to 0x9FB00

xor AX,AX
mov DS,AX			;Set the DS segment to 0x0

mov [BootDevice],DL	;Save the boot device number


;Retrieve and save the disk geometry
call GetDiskGeometry


LoadStage2:

;Get offset to first cluster
mov AX,0x2000       ;We will load all sectors and clusters
mov ES,AX           ;to the address 0x20000
xor BX,BX           ;Offset to load File System Definition: 0x0

;Load the File System Definition into memory
mov AX,1            ;File system definition start
mov SI,3            ;File system definition is 3 sectors large
mov DL,[BootDevice]
call LoadSectorsCHS

push word [ES:0x0 + 8]      ;Push cluster size (in bytes)
push word [ES:0x0 + 12]     ;push cluster size (in sectors)
push word [ES:0x0 + 32]     ;Push the offset (in sectors) to the first cluster
mov BP,SP           ;So we can access data on the stack

;[SS:BP] = First Offset
;[SS:BP+2] = Cluster Size (in sectors)
;[SS:BP+4] = Cluster Size (in bytes)

xor CX,CX           ;Cluster number
.FindFileOuter:

;Multiply cluster number by sectors per cluster
mov AX,CX           ;Move cluster number into AX
xor DX,DX           ;Clear DX for mul
mul word [SS:BP+2]  ;EDX:EAX * [mem] => EDX = high dword, EAX = low dword
add AX,[SS:BP]      ;Add the first cluster offset (in sectors) to cluster number

;Load current cluster into memory (cluster number)
xor BX,BX           ;ES:BX = memory location to load sectors into
mov SI,[SS:BP+2]
mov DL,[BootDevice]
call LoadSectorsCHS

xor BX,BX           ;Holds current directory entry offset (in bytes)
.FindFileInner:
mov AL,[ES:0x0 + BX]    ;Move attributes byte into AL
test AL,0x1         ;Does directory entry exist?
jz .SkipEntry       ;If no then skip this entry

;Compare the current directory entry's filename to the stage 2 filename
mov SI,Stage2Filename   ;DS:SI
mov DI,0x0 + 32         ;ES:DI
add DI,BX           ;Add directory entry offset
call CompareStrings

cmp AX,0            ;Are they different?
je .SkipEntry       ;If yes, than skip this entry

;We found stage 2
push word [ES:0x0 + BX + 1]     ;Push the first cluster
push word [ES:0x0 + BX + 5]     ;Push the file size (in bytes)

;Load stage 2 then jump to stage 2
call LoadFile

jmp Stage2Start

.SkipEntry:
add BX,64			;Offset to next directory entry
cmp BX,[SS:BP+4]	;Is offset past cluster size (in bytes)
jl .FindFileInner	;If no, than search next directory entry

;If yes, than load in next cluster chain
xchg AX,CX          ;xchg takes up less space then mov
call GetNextClusterInChain
xchg AX,CX

;If we are at the last cluster in the chain,
;than the file doesn't exist; hang the system
cmp CX,0xFFFE
jne .FindFileOuter

cli
hlt


;;;;;;;;;;;;;;;;;
;;  GetNextClusterInChain
;;
;;  Parameters:
;;  AX CatNumber
;;
;;  Returns:
;;  AX contains next cat number
;;;;;;;;;;;;;;;;;
GetNextClusterInChain:
enter 0,0
push BX
push CX
push DX
push SI

;Get the CAT sector
mov CX,2
xor DX,DX
mul CX              ;Multiply the CAT number by 2 (size of ushort)

mov CX,512
xor DX,DX
div CX              ;Divide by 512 to get the sector the CAT number resides in
push DX             ;Save the remainder as the offset into the sector

add AX,4            ;First four sectors are permanent in the file system

;Load the CAT sector
xor BX,BX           ;Load sector into ES:0x0
mov DL,[BootDevice]
mov SI,1            ;Read one sector
call LoadSectorsCHS

;Get the next CAT number
pop BX              ;Recover the remainder (offset into the sector)
mov AX,[ES:0x0 + BX]    ;Get the next CAT number

pop SI
pop DX
pop CX
pop BX
leave
ret


;;;;;;;;;;;;;;;;;
;;  LoadFile
;;
;;  Parameters:
;;  word FileSize
;;  word FirstCluster
;;  word OffsetCluster
;;  word ClusterSize (in bytes)
;;  word ClusterSize (in sectors)
;;
;;  Returns:
;;  None
;;;;;;;;;;;;;;;;;
LoadFile:
enter 0,0
pusha

xor CX,CX           ;CX contains offset into memory

.LoadLoop:
mov AX,[SS:BP+6]    ;The cluster to read
xor DX,DX
mul word [SS:BP+10]
add AX,[SS:BP+8]    ;Add offset to first cluster (in sectors)

push ES             ;Save current ES
xor DX,DX           ;Set ES to zero so we can load a sector of
mov ES,DX           ;the file directly after stage 1

mov BX,0x7E00       ;Base address of where to load stage 2
add BX,CX           ;Add the offset into memory to the base address
mov SI,[SS:BP+10]
mov DL,[BootDevice]
call LoadSectorsCHS

pop ES              ;Restore original ES

mov AX,[SS:BP+6]
call GetNextClusterInChain
mov [SS:BP+6],AX

add CX,[SS:BP+12]
cmp CX,[SS:BP+4]
jl .LoadLoop

popa
leave
ret 10


;;;;;;;;;;;;;;;;;
;;  CompareStrings
;;
;;  Parameters:
;;  DS:SI = First String
;;  ES:DI = Second String
;;
;;  Returns:
;;  AX = 1 if strings are the same
;;  AX = 0 if strings are different
;;;;;;;;;;;;;;;;;
CompareStrings:
enter 0,0
push BX
push CX

xor BX,BX
.Loop:
mov AL,[DS:SI+BX]
mov AH,[ES:DI+BX]

cmp AL,0
je .IsNull
cmp AH,0
je .IsNull
jmp .NotNull        ;If neither is null, terminate right now

.IsNull:
mov CL,1
jmp .SkipNull

.NotNull:
mov CL,0

.SkipNull:

cmp AL,AH
jne .Different

cmp CL,1
je .Same

inc BX
jmp .Loop


.Different:
mov AX,0
jmp .Exit

.Same:
mov AX,1

.Exit:

pop CX
pop BX
leave
ret


;;;;;;;;;;;;;;;;;
;;  GetDiskGeometry
;;
;;  Parameters:
;;  DL = Device
;;
;;  Returns:
;;  Fills disk geometry data structure
;;;;;;;;;;;;;;;;;
GetDiskGeometry:
enter 0,0
pusha
push ES

xor AX,AX           ;Protect us from some BIOS bugs
mov ES,AX
xor DI,DI

mov AH,08h
int 13h

;Get sector count
mov BL,CL
and BL,00111111b
mov [DiskGeometry.SectorCount],BL

;Get cylinder count
mov BL,CH
mov BH,CL
and BH,11000000b
shr BH,6
mov [DiskGeometry.CylinderCount],BX

;Get head count
mov [DiskGeometry.HeadCount],DH

;Get drive count
mov [DiskGeometry.DriveCount],DL

pop ES
popa
leave
ret


;;;;;;;;;;;;;;;;;
;;  LBAToCHS
;;
;;  Parameters:
;;  AX = LBA Address
;;
;;  Returns:
;;  CX and DH are setup for CHS read sector call
;;;;;;;;;;;;;;;;;
LBAToCHS:
enter 0,0
push AX
push BX
push DX

xor DX,DX           ;Dividend is DX:AX register pair
xor BH,BH           ;Divisor is in BX, clear BH
mov BL,[DiskGeometry.SectorCount]
div BX              ;DX:AX / BX => quotient: AX, remainder: DX
mov CL,DL           ;Move Sector (DL) into CL
inc CL              ;Sector is one based, add one

cmp AX,0            ;If the quotient is zero then we only
je .OnlySectors     ;have sectors, we can stop here

xor DX,DX           ;Clear out DX again
mov BL,[DiskGeometry.HeadCount]
inc BL              ;Increment max head
div BX              ;DX:AX / BX => quotient: AX, remainder: DX
jmp .Exit

.OnlySectors:
xor AX,AX           ;We only have sectors, zero out cylinder
xor DX,DX           ;We only have sectors, zero out head

.Exit:

;Setup registers for a CHS load sectors call
;Currently CL contains sector, DX contains head, and AX contains cylinder
and CL,00111111b    ;Sector only uses low six bits
mov DH,DL           ;DL contains head, move it into DH
mov CH,AL           ;Move low eight bits of cylinder into CH
shl AH,6            ;Move high two bits of cylinder into bits 6 and 7 of AH
or CL,AH            ;Move high two bits of cylinder into bits 6 and 7 of CL

pop AX              ;Preserve only DL
mov DL,AL           ;DH contains head so keep DH

pop BX
pop AX
leave
ret


;;;;;;;;;;;;;;;;;
;;  LoadSectorsCHS
;;
;;  Parameters:
;;  AX = Start LBA
;;  SI = Sector Count
;;  ES:BX = Memory Address To Load Sectors Into
;;  DL = Device
;;
;;  Returns:
;;  Sectors are loaded into specified memory location
;;;;;;;;;;;;;;;;;
LoadSectorsCHS:
enter 0,0
pusha

.Loop:
call LBAToCHS
push AX

xor AH,AH           ;Clear AH for the reset command
int 13h             ;Reset disk drive
xor DI,DI           ;Try count

;Attempt to read a sector from the disk
.Try:
mov AH,02h
mov AL,1
int 13h             ;Read sector

jnc .SkipTry        ;If we read the sector, don't try again
inc DI              ;Increment try count
cmp DI,5            ;Try reading the sector five times
jl .Try             ;Try again if we are are <= 5 tries

;Failed to load the sector 5 times, halt the system
cli
hlt

.SkipTry:

pop AX

add BX,512
inc AX
dec SI
jnz LoadSectorsCHS.Loop

popa
leave
ret


;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;
;;  Stage 1 Data
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
Stage2Filename          db 'Stage2.bin',0


times 510 - ($ - $$) db 0
dw 0xAA55


;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;
;;  Stage 2
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
Stage2Start:


;Force video mode 03h
;This will also clear the screen
mov AH,00h
mov AL,03h
int 10h


;Set the cursor position to (0, 0)
mov AH,02h          ;Set cursor command
mov BH,00h          ;Page number
mov DH,0            ;Row
mov DL,0            ;Column
int 10h             ;Set cursor position


;Create some space for the protected mode message
;which comes later
call PrintNewLine
call PrintNewLine


;Print the boot drive disk geometry
xor CH,CH
push MessageGeometrySectors
call PrintString
mov CL,[DiskGeometry.SectorCount]
push CX
call PrintNumber
call PrintNewLine

push MessageGeometryHeads
call PrintString
mov CL,[DiskGeometry.HeadCount]
inc CL
push CX
call PrintNumber
call PrintNewLine

push MessageGeometryCylinders
call PrintString
mov CX,[DiskGeometry.CylinderCount]
inc CX
push CX
call PrintNumber
call PrintNewLine
call PrintNewLine


;Enable the A20 line
;Check to see if the A20 line is already enabled
EnableA20:
call A20Enabled
cmp AX,0
jne EnableA20.Enabled   ;Enabled

.Disabled:
;Print a message saying it is disabled,
;but still attempt to enable it
push MessageA20Disabled
Call PrintString

;Use the BIOS to enable A20
call A20EnableBiosInterrupt
call A20Enabled
cmp AX,0
jne EnableA20.Enabled   ;Enabled

;Try the fast A20 method
call A20EnableFast
call A20Enabled
cmp AX,0
jne EnableA20.Enabled   ;Enabled

;Final attempt is to use the keyboard controller
call A20EnableKeyboard
call A20Enabled
cmp AX,0
jne EnableA20.Enabled   ;Enabled

;We were unable to enable the A20 line
push MessageA20Failed
call PrintString

;We must hang the system
cli
hlt

;A20 is enabled, we can continue
.Enabled:
push MessageA20Enabled
call PrintString
call PrintNewLine


;Get Memory Size
call GetMemorySize
mov [0x600],EAX     ;Memory size is stored in EAX after the call

;Print the amount of memory recognized
push MessageTotalSystemMemory
call PrintString
push EAX
call PrintNumber32
call PrintNewLine


;Get Memory Map
xor AX,AX
mov ES,AX           ;Make sure ES is zero
mov DI,0x604        ;Location to store the memory map
call GetMemoryMap


;Print the memory map entry count
push MessageMemoryMap
call PrintString

push word [0x604]
call PrintNumber
call PrintNewLine
call PrintNewLine


;Get all of the vesa information now
call GetVesaInformation


jmp Stage3Start


;;;;;;;;;;;;;;;;;
;;  A20Enabled
;;
;;  Arguments:
;;  None
;;
;;  Returns:
;;  AX = 0 if A20 is disabled
;;  AX = 1 if A20 is enabled
;;;;;;;;;;;;;;;;
A20Enabled:
enter 0,0
push DS
push ES

xor AX,AX
mov DS,AX
mov AX,0xFFFF
mov ES,AX

;Check wrap around address
cmp word [ES:0x7E0E],0xAA55
jne .Enabled

mov word [DS:0x7DFE],0x9944
cmp word [ES:0x7E0E],0x9944
jne .Enabled

;Return AX=0 if A20 is disabled
.Disabled:
mov AX,0
jmp .Exit

;Return AX=1 if A10 is enabled
.Enabled:
mov AX,1

.Exit:
;Set the boot signature back to 0xAA55
;in case we have to test the A20 line again
mov word [DS:0x7DFE],0xAA55

pop ES
pop DS
leave
ret


;;;;;;;;;;;;;;;;;
;;  A20EnableBiosInterrupt
;;
;;  Arguments:
;;  None
;;
;;  Returns:
;;  None
;;;;;;;;;;;;;;;;
A20EnableBiosInterrupt:
enter 0,0
push AX

mov AX,2401h
int 15h

pop AX
leave
ret


;;;;;;;;;;;;;;;;;
;;  A20EnableFast
;;
;;  Arguments:
;;  None
;;
;;  Returns:
;;  None
;;;;;;;;;;;;;;;;
A20EnableFast:
enter 0,0
push AX

in AL,0x92
or AL,2
out 0x92,AL

pop AX
leave
ret


;;;;;;;;;;;;;;;;;
;;  A20EnableKeyboard
;;
;;  Arguments:
;;  None
;;
;;  Returns:
;;  None
;;;;;;;;;;;;;;;;
A20EnableKeyboard:
enter 0,0
push AX

call .Wait1
mov AL,0xAD
out 0x64,AL

call .Wait1
mov AL,0xD0
out 0x64,AL

call .Wait2
in AL,0x60
push eax

call .Wait1
mov AL,0xD1
out 0x64,AL

call .Wait1
pop eax
or AL,2
out 0x60,AL

call .Wait1
mov AL,0xAE
out 0x64,AL

call .Wait1
ret

.Wait1:
in AL,0x64
test AL,2
jnz .Wait1
ret

.Wait2:
in AL,0x64
test AL,1
jz .Wait2
ret

pop AX
leave
ret


;;;;;;;;;;;;;;;;;
;;  GetMemorySize
;;
;;  Arguments:
;;  None
;;
;;  Returns:
;;  EAX = Size of memory in KiB
;;;;;;;;;;;;;;;;
GetMemorySize:
enter 0,0
push EBX
push ECX
push EDX

xor EAX,EAX
xor EBX,EBX
xor ECX,ECX
xor EDX,EDX

mov AX,0xE801
int 15h

.NoError:
jcxz .Continue

mov AX,CX           ;Data stored in CX and DX
mov BX,DX           ;Move it into AX and BX
jmp .Continue

.Continue:

;Do the math to get the total amount of memory in KiB
;EAX contains memory from 1MiB to 16MiB in Kib
;EBX contains memory above 16MiB in 64KiB blocks
xchg EAX,EBX
mov ECX,64          ;Multiply the 64KiB blocks by 64 to get KiB
mul ECX             ;to get the total amount of KiB
add EAX,EBX         ;Add the 1MiB - 16MiB with the > 16MiB
add EAX,1024        ;Add 1024 KiB to count the first MiB of memory

pop EDX
pop ECX
pop EBX
leave
ret


;;;;;;;;;;;;;;;;;
;;  GetMemoryMap
;;
;;  Arguments:
;;  None
;;
;;  Returns:
;;  Sets memory map
;;;;;;;;;;;;;;;;
GetMemoryMap:
enter 0,0
pushad

push DI             ;Store the original memory address pointer
add DI,4            ;Leave some room for the entry count
xor SI,SI           ;SI stores the entry count

mov dword [ES:SI+20],1  ;ACPI 3.0 complient
mov EAX,0xE820      ;Memory map function
xor EBX,EBX         ;Start at begining of map
mov ECX,24          ;We want 24 or 20 bytes at a time
mov EDX,'PAMS'      ;Required
int 15h

.Loop:
inc SI              ;Increment entry count
add DI,24           ;Increment memory address pointer

jc .Exit            ;May indicate end of list
cmp EBX,0           ;May also indicate end of list
je .Exit

mov dword [ES:SI+20],1  ;ACPI 3.0 complient
mov EAX,0xE820      ;Memory map function
mov ECX,24          ;We want 24 or 20 bytes again
mov EDX,'PAMS'      ;Required
int 15h
jmp .Loop           ;Go to the begining of the loop
                    ;The begining of the loop tests if we are done

.Exit:

pop DI              ;Get the starting memory address pointer
mov [ES:DI],SI      ;Store entry count in there

popad
leave
ret


;;;;;;;;;;;;;;;;;
;;  GetVesaInformation
;;
;;  Arguments:
;;  ES:DI = Pointer to Memory Buffer
;;
;;  Returns:
;;  Stores VESA information in memory
;;;;;;;;;;;;;;;;
GetVesaInformation:
enter 0,0
pusha
push ES
push FS

mov AX,0x1000
mov ES,AX
xor DI,DI

mov dword [ES:DI],'VBE2'    ;We want VBE 2.0 information
mov AH,4Fh          ;Vesa video mode interrupt
mov AL,00h          ;Get VBE information function
int 10h

cmp AH,0            ;Vesa [2.0] isn't supported
jne .NotSupported

mov AX,[ES:DI+14+2] ;Contains segment for video mode array
mov FS,AX           ;Put segment in FS
mov SI,[ES:DI+14]   ;Offset for video mode array

xor CX,CX           ;CX holds video mode count
mov DI,0x300        ;Copy the video mode array to physical 0x10300

.VideoModeLoop:
mov AX,[FS:SI]      ;Put the video mode number into AX
add SI,2            ;Increment offset for next video mode

cmp AX,0xFFFF       ;0xFFFF means we reached the end of the array
je .ExitVideoModeLoop

mov [ES:DI],AX      ;Save the video mode to our array
add DI,2            ;Increment the pointer to our array
inc CX              ;Increment the video mode count
jmp .VideoModeLoop  ;Fetch the next video mode

.ExitVideoModeLoop:
mov [ES:0x200],CX          ;Save the video mode count
mov word [ES:0x200+2],0    ;Video mode count is dword,
                           ;set high two bytes to zero

mov BX,CX          ;Move video mode count into BX
mov SI,0x300       ;Pointer to our video mode array
mov DI,0x500       ;Where we will store the video mode info

.VideoModeInfoLoop:
mov AH,4Fh          ;Vesa video mode interrupt
mov AL,01h          ;Get video mode info
mov CX,[ES:SI]      ;The video mode we want info about
int 10h

add SI,2            ;Increment video mode pointer
add DI,256          ;Increment video mode info pointer

dec BX              ;Exit the loop when we are done
jnz .VideoModeInfoLoop

push MessageVesaSupported
call PrintString
jmp .Exit

.NotSupported:
push MessageVesaNotSupported
call PrintString

.Exit:

pop FS
pop ES
popa
leave
ret


;;;;;;;;;;;;;;;;;
;;  PrintString
;;
;;  Arguments:
;;  word PointerToString
;;
;;  Returns:
;;  None
;;;;;;;;;;;;;;;;;
PrintString:
enter 0,0
push AX
push BX
push SI

mov SI,[BP+4]
.Loop:
cmp byte [SI],0
je .Exit

mov AH,0Eh
mov AL,[SI]
int 10h
inc SI

jmp .Loop
    
.Exit:

pop SI
pop BX
pop AX
leave
ret 2


;;;;;;;;;;;;;;;;;
;;  PrintCharacter
;;
;;  Arguments:
;;  word Character (low byte)
;;
;;  Returns:
;;  None
;;;;;;;;;;;;;;;;;
PrintCharacter:
enter 0,0
pusha

mov AH,0Eh
mov AL,[BP+4]
int 10h

popa
leave
ret 2


;;;;;;;;;;;;;;;;;
;;  PrintNewLine
;;
;;  Arguments:
;;  None
;;
;;  Returns:
;;  None
;;;;;;;;;;;;;;;;;
PrintNewLine:
enter 0,0
pusha

;Print \r
mov AH,0Eh
mov AL,13
int 10h

;Print \n
mov AH,0Eh
mov AL,10
int 10h

popa
leave
ret


;;;;;;;;;;;;;;;;;
;;  PrintNumber
;;
;;  Arguments:
;;  word Number
;;
;;  Returns:
;;  None
;;;;;;;;;;;;;;;;;
PrintNumber:
enter 0,0
pusha

mov AX,[BP+4]
mov BX,10
mov SI,.NumberString
xor CX,CX

.Loop:
xor DX,DX
div BX

add DL,'0'
mov [SI],DL
inc SI
inc CX

cmp AX,0
ja .Loop

dec SI
.LoopPrint:
mov AH,0Eh
mov AL,[SI]
int 10h

dec SI
dec CX
jnz .LoopPrint

popa
leave
ret 2

.NumberString db '000000'


;;;;;;;;;;;;;;;;;
;;  PrintNumber32
;;
;;  Arguments:
;;  dword Number
;;
;;  Returns:
;;  None
;;;;;;;;;;;;;;;;;
PrintNumber32:
enter 0,0
pushad

mov ECX,[BP+4]
mov EBX,10
mov SI,.NumberString

.Loop:
xor EDX,EDX
mov EAX,ECX
div EBX
mov ECX,EAX

add DL,'0'
mov [SI],DL
inc SI

cmp ECX,0
jne .Loop

dec SI
.LoopPrint:
mov AH,0Eh
mov AL,[SI]
int 10h
dec SI
cmp SI,.NumberString
jae .LoopPrint

popad
leave
ret 4

.NumberString db '0000000000'



;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;
;;  Stage 3
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
Stage3Start:

;Save real mode segment registers
push GS

;Setup the three GDT entries by setting the GDT register
lgdt [GdtDesc]

;Enter protected mode
mov EAX,CR0
or EAX,1
mov CR0,EAX

;Set segments to the data segment descriptor
mov AX,0x10
mov GS,AX

;Enter unreal mode
mov EAX,CR0
and EAX,0xFFFFFFFE
mov CR0,EAX

;Load real mode segment registers back in.
;This will make the base the GS segment 0x0
;while the limit will remain at 0xFFFFFFFF
pop GS


call PrintNewLine
push MessageUnrealMode
call PrintString


LoadKernel:

;Get offset to first cluster
mov AX,0x2000       ;We will load all sectors and clusters
mov ES,AX           ;to the address 0x20000
xor BX,BX           ;Offset to load File System Definition: 0x0

;Load the File System Definition into memory
mov AX,1            ;File system definition start
mov SI,3            ;File system definition is 3 sectors large
mov DL,[BootDevice]
call LoadSectorsCHS

push word [ES:0x0 + 8]      ;Push cluster size (in bytes)
push word [ES:0x0 + 12]     ;push cluster size (in sectors)
push word [ES:0x0 + 32]     ;Push the offset (in sectors) to the first cluster
mov BP,SP           ;So we can access data on the stack

;[SS:BP] = First Offset
;[SS:BP+2] = Cluster Size (in sectors)
;[SS:BP+4] = Cluster Size (in bytes)

xor CX,CX           ;Cluster number
.FindFileOuter:

;Multiply cluster number by sectors per cluster
mov AX,CX           ;Move cluster number into AX
xor DX,DX           ;Clear DX for mul
mul word [SS:BP+2]  ;EDX:EAX * [mem] => EDX = high dword, EAX = low dword
add AX,[SS:BP]      ;Add the first cluster offset (in sectors) to cluster number

;Load current cluster into memory (cluster number)
xor BX,BX           ;ES:BX = memory location to load sectors into
mov SI,[SS:BP+2]
mov DL,[BootDevice]
call LoadSectorsCHS

xor BX,BX           ;Holds current directory entry offset (in bytes)
.FindFileInner:
mov AL,[ES:0x0 + BX]    ;Move attributes byte into AL
test AL,0x1         ;Does directory entry exist?
jz .SkipEntry       ;If no then skip this entry

;Compare the current directory entry's filename to the stage 2 filename
mov SI,KernelFilename   ;DS:SI
mov DI,0x0 + 32         ;ES:DI
add DI,BX           ;Add directory entry offset
call CompareStrings

cmp AX,0            ;Are they different?
je .SkipEntry       ;If yes then skip this entry

;We found stage 2
push word [ES:0x0 + BX + 1]     ;Push the first cluster
push word [ES:0x0 + BX + 5]     ;Push the file size (in bytes)

;Load stage 2 then jump to stage 2
call LoadFileUnreal

jmp EnterProtectedMode

.SkipEntry:
add BX,64
cmp BX,[SS:BP+4]
jl .FindFileInner

xchg AX,CX          ;xchg takes up less space then mov
call GetNextClusterInChain
xchg AX,CX

;If we are at the last cluster in the chain
;then the file doesn't exist, hang the system
cmp CX,0xFFFE
jne .FindFileOuter

cli
hlt


;;;;;;;;;;;;;;;;;
;;  LoadFileUnreal
;;
;;  Parameters:
;;  word FileSize
;;	word FirstCluster
;;	word OffsetCluster
;;	word ClusterSize (in bytes)
;;	word ClusterSize (in sectors)
;;
;;  Returns:
;;  None
;;;;;;;;;;;;;;;;;
LoadFileUnreal:
enter 0,0
pusha

xor CX,CX           ;CX contains offset into memory

.LoadLoop:
mov AX,[SS:BP+6]    ;The cluster to read
xor DX,DX
mul word [SS:BP+10]
add AX,[SS:BP+8]    ;Add offset to first cluster (in sectors)

xor BX,BX           ;Base address of where to load stage 2
mov SI,[SS:BP+10]
mov DL,[BootDevice]
call LoadSectorsCHS

;Copy sector
xor EBX,EBX
.Copy:
mov AL,[ES:BX]
push EBX
add BX,CX
add EBX,0x1000000   ;16MiB mark
mov [GS:EBX],AL
pop EBX

inc BX
cmp BX,512
jb .Copy

mov AX,[SS:BP+6]
call GetNextClusterInChain
mov [SS:BP+6],AX

add CX,[SS:BP+12]
cmp CX,[SS:BP+4]
jl .LoadLoop

popa
leave
ret 10


;Enter protected mode
EnterProtectedMode:
mov EAX,CR0
or EAX,1
mov CR0,EAX

;Force a far jump to clear the instruction cache pipeline
;and to load the CS segment register with our new code segment
jmp 0x08:ProtectedMode

;Compile all code from here on out as 32-bit instructions
[BITS 32]
ProtectedMode:

;Set all other segments to the data segment descriptor
mov AX,0x10
mov DS,AX
mov SS,AX
mov ES,AX
mov FS,AX
mov GS,AX

mov ESP,0x9FB00     ;Setup the new stack


;Print a message saying that we made it into protected mode
push word 0x0001
push dword MessageProtectedMode
call PrintStringProtected


;Begin executing the kernel
jmp 0x1000000		;16MiB mark


;This code should never be reached, but just in case
;we will hang the system here
cli
hlt


;;;;;;;;;;;;;;;;;
;;  PrintStringProtected
;;
;;  Arguments:
;;  dword PointerToString
;;	word (low byte) Attribute
;;
;;  Returns:
;;  None
;;;;;;;;;;;;;;;;;
PrintStringProtected:
enter 0,0
push EAX
push EBX
push ESI

xor EBX,EBX
mov ESI,[EBP+8]
mov AH,[EBP+12]

.Loop:
mov AL,[ESI]
cmp AL,0
je .Exit

mov byte [0xB8000 + EBX],AL
mov byte [0xB8000 + EBX + 1],AH
inc ESI
add EBX,2

jmp .Loop
    
.Exit:

pop ESI
pop EBX
pop EAX
leave
ret 4


;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;
;;  Stage 2 and Stage 3 Data
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
KernelFilename              db 'Kernel.exe',0

MessageGeometrySectors      db 'Sectors: ',0
MessageGeometryHeads        db 'Heads: ',0
MessageGeometryCylinders    db 'Cylinders: ',0

MessageA20Enabled           db 'A20 Line is enabled',13,10,0
MessageA20Disabled          db 'A20 Line is disabled',13,10,0
MessageA20Failed            db 'A20 Line failed to enable. Halting',13,10,0

MessageTotalSystemMemory    db 'Total System Memory (KiB): ',0
MessageMemoryMap			db 'Memory map entry count: ',0

MessageVesaSupported		db 'Vesa is supported',13,10,0
MessageVesaNotSupported		db 'Vesa is not supported',13,10,0

MessageUnrealMode			db 'Entered Unreal Mode',0
MessageKernelLoaded         db 'Kernel Loaded',0
MessageProtectedMode		db 'Entered Protected Mode',0


Gdt:

GdtNull:
dd 0
dd 0

GdtCode:
dw 0xFFFF           ;Limit
dw 0x0              ;Base address

db 0x0              ;Continued base address
db 10011010b        ;Type, privilage, ring level
db 11001111b        ;Continued limit, attributes, granularity
db 0x0              ;Continued base address

GdtData:
dw 0xFFFF           ;Limit
dw 0x0              ;Base address

db 0x0              ;Continued base address
db 10010010b        ;Type, privilage, ring level
db 11001111b        ;Continued limit, attributes, granularity
db 0x0              ;Continued base address

GdtEnd:


GdtDesc:
dw GdtEnd - Gdt - 1
dd Gdt


times 4096 - ($ - $$) db 0





;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;
;;  Memory Persistent Variables
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
[ABSOLUTE 0x500]
BootDevice              resb 1

DiskGeometry:
.SectorCount            resb 1  ;Low 6 bits only
.HeadCount              resb 1
.CylinderCount          resw 1  ;Low 10 bits only
.DriveCount             resb 1

[ABSOLUTE 0x600]
SystemMemory            resd 1
MemoryMapEntryCount     resd 1





;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;
;;  Memory Map
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

;Address            Size    Description

;0x500              1h      Boot Device
;0x501              1h      Sector Count of Boot Device
;0x502              1h      Head Count of Boot Device
;0x503              2h      Cylinder Count of Boot Device
;0x505              1h      Drive Count

;0x600              4h      Memory Size
;0x604              4h      Memory Map Entry Count
;0x608 - 0x800      200h    Memory map Entries

;0x10000             200h    VESA Information Buffer
;0x10200             4h      Video Mode Count
;0x10300 - 0x10400   100h    Video Modes (128 Max)
;0x10500 - 0x18500   8000h   Video Mode Definitions