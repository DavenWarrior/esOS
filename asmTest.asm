[bits 16]
[ORG 0x7c00]

jmp 0x0:start   ; begin long jump to 0x0:0x7c00

start:
	; begin boot-loader in assembly
	cli
	
	;set up stack
	mov ax, 0x9000
	mov ss, ax
	mov sp, 0xfb00  ; stack top is now at 0x9fb00
	
	xor ax, ax
	mov ds, ax      ; clear ds register
	mov [BootDevNum], AL
	
	;get disk geometry
	call GetDiskGeometry
	
LoadStageTwo:    ;load stage 2
	;set up first cluster
	mov AX, 0x2000
	mov ES, AX        ;load to address 0x2000:0x0
	xor BX, BX        ;offset 0x0
	
	;load file system definition
	
	
	
GetDiskGeometry:      ;get data on disk (assuming hdd)
	enter 0, 0        ;reduces space used by stage 1 (enter opcode is 4 bytes long, conventional 3 stage entry is 6 bytes long)
	pusha
	push ES
	
	xor AX, AX   ;avoid bios bug(s)
	mov ES, AX
	xor DI, DI
	
	mov AH, 08h   ;bios call
	int 13h
	
	;find number of sectors
	mov BL, CL
	and BL, 00111111b  ;low 6 bits
	mov [DiskGeometry.SectorCount], BL
	;get number of cylinders
	mov BL, CH            ;lower 8 bits
	mov BH, CL            ;upper 2 bits
	and BH, 11000000b
	shr BH, 6
	mov [DiskGeometry.CylinderCount], BX
	;get number of heads
	mov [DiskGeometry.HeadCount], DH
	;get number of drives
	mov [DiskGeometry.DriveCount], DL
	
	pop ES
	popa
	leave
	ret

LoadSectors:
;loads sectors from hdd to memory
; AX = start LBA address
; SI = number of sectors to read
; DL = device number
; ES:BX = buffer to write data to
	enter 0, 0
	pusha
	
	.loop:
	call LBAtoCHS
	push AX         ;Preserve LBA for later loops
	
	xor AH, AH
	int 13h         ;reset drive with AH = 0
	xor DI, DI      ;counter
	
	;try reading a sector
	.try:
	mov AH, 02h
	mov AL, 1       ;number of sectors to read = 1
	int 13h         ;int 13h 02h - read sectors
	
	jnc .skipTries  ;If success skip extra tries
	inc DI
	cmp DI, 5       ;try 5 more times
	jl .try
	
	cli             ;If all attempts fail, shutdown system
	hlt
	
	.skipTries:
	pop AX          ;get LBA value
	add BX, 512     ;increment memory to write to by 512 bytes
	inc AX          ;increment LBA by one
	dec SI          ;decrement number of sectors left to read
	jnz LoadSectors.loop
	
	popa
	leave
	ret
	

LBAtoCHS:
;convert LBA address to CHS for bios interrupt
; AX = LBA address
	enter 0, 0
	push AX
	push BX
	push DX
	
	xor DX, DX      ;DX:AX used for dividend
	xor BH, BH      ;BX = divisor
	mov BL, [DiskGeometry.sectors]
	div BX          ;DX:AX / BX = quotient + rem   i.e  LBA / sectors = 'quotient AX  rem DX'
	mov CL, DL      ;                                    AX = to be used later   DL = Sector
	inc CL          ; --Sector--
	
	cmp AX, 0
	je .sectorOnly
	
	xor DX, DX
	mov BL, [DiskGeometry.heads]
	inc BL
	div BX         ;DX:AX / BX = quotient + rem   i.e.  prev. Quotient / heads = 'quotient AX  rem DX'
	jump .Exit     ;                                       * --AX = Cylinders--   --DX = Heads-- *
	
	.sectorOnly:
	xor AX, AX      ;cylinders = 0
	xor DX, DX      ;heads = 0
	
	.Exit:
	;from  :  AX has Cylinders, CL has Sectors, and DX has Heads
	;                   to
	;  CH|CCCCCCCC|   CL|CCSSSSSS|   DH|HHHHHHHH| 
	and CL, 00111111b ; low 6 bits of CL for sector
	mov DH, DL        ; move heads into DH
	mov CH, AL        ; low 8 bits of Cylinders in CH
	shl AH, 6         ; move 2 highest bits of cylinders into two highest bits of AH
	or CL, AH         ; 2 highest bits of Cylinders now in 2 highest bits of CL
	
	pop AX            ; preserve DL from before
	mov DL, AL
	pop BX
	pop AX
	leave
	ret
	
Stage2FileName db 'Stage2.bin', 0

times 510 - ($-$$) db 0
dw 0xAA55


;-----------------------------------------------------------------------------------------------
;---------------------Important Variables-------------------------------------------------------
;-----------------------------------------------------------------------------------------------
[Absolute 0x500]
BootDevNum        resb 1

DiskGeometry:
.SectorCount      resb 1  ;low 6 bits
.HeadCount        resb 1
.CylinderCount    resw 1  ;low 10 bits
.DriveCount       resb 1

[Absolute 0x600]
sysMem resd 1
MemMapEC resd 1
