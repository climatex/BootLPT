; BootLPT/86 Boot ROM, (c) J. Bogin
; INT13h handler based on ROMOS, (c) 2001-2017 M. Rehak
; Licensed under GPLv3

; Compile with FASM
			
use16					; 8086/8088 compatible, real mode ROM code
org 0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Compile-time constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


LPT1_ADDRESS		equ 0378h	; Can be either 378h or even 3BCh (on vintage systems).

ROM_SIZE		equ 8		; !!! (E/EE)PROM size in kB. Minimum 2K at this moment.

IMG_DISK_DRIVE		equ 0		; ! Drive to use. 0=A,1=B,80h=C: etc. Go 1 if there also is a real FDD!
IMG_DISK_LOAD_SEG	equ 9000h	; !!! Segment where to load the boot image. (max. 64K). Offset 0.
IMG_DISK_CYLINDERS	equ 7		; !!! Boot image geometry. Adjust this to your image:
IMG_DISK_HEADS		equ 1		; 512B*18sec*7cyl*1hd = 64 512 bytes BOOTDISK.IMG.
IMG_DISK_SECTORS	equ 18		; There's no validation - don't mess this up!
IMG_DISK_BOOT_DRIVE	equ 24h		; Offset in bootsector where the boot drive is stored

STACK_SEG		equ 08000h	; ! Stack segment
STACK_OFS		equ 0FFFEh	; Stack offset
					; Stack at addr. 8FFFEh, growing down.
					; Address = (segment * 0x10) + offset, here in real mode.
					; Boot image set @ 90000h (grows up), stack @ 8FFFEh (grows down).
					; If you have an ancient system with less than 640K RAM, change this!

OLD_INT13H		equ 85h		; INT 85h will be the original BIOS INT13h handler


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ROM signature ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


SIGNATURE1		db 055h		; 55h
SIGNATURE2		db 0AAh		; AAh
SIGNATURE3		db ROM_SIZE * 2	; ROM size in 0,5 kB chunks
jmp short start				; One-byte short jump
CHECKSUM		db 0		; !!! Checksum byte (BOOTLPT.BIN offs. 5). PATCH after compiling.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Entry point ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


start:
	cli				; Disable interrupts for a while.
	cld				; Clear direction flag.	
	
	mov ax,cs			; DS = CS, as we have both code and data around here
	mov ds,ax
	mov es,ax			; ES too (used when addressing DI)
	
	sti				; Enable them again.
	
	lea si,[INTERVAL]		; 'Press any key for BootLPT/86...'
	call print
	
	xor ax,ax			; AX=0, INT 1Ah: Get tick count (circa 18.2 per sec)
	int 1Ah				; Returns two words in CX:DX. Original low WORD kept in DX.
@@:
	push dx				; DX on stack contains the low order tickcount. CX discarded.
	mov ax,0100h			; Check for a keypress.
	int 16h
	jnz key_pressed			; There was a keypress - continue with the ROM.
	xor ax,ax			; No keypress - check the current tick count.
	int 1Ah
	mov cx,dx			; *New* low order WORD is now in CX...
	pop dx				; And the *old* low order WORD to DX.
	sub cx,dx			; Compare the time difference.
	cmp cx,18*4			; More than four seconds?
	ja @f				; Then, will continue with BIOS boot.
	hlt				; Nope. Halt the CPU for a while (timer IRQ0 will wake it up)...
	jmp @b				; ... and check the time again.
	
@@:
	xor bx,bx			; Bailing out.
	mov ax,0E0Dh			; Get rid of the "Press any key ..." message.
	int 10h				; Print carriage return
	mov cx,40d			; Erase the message with blanks
@@:
	mov ax,0E20h			; 40 spaces (ASCII 0x20)
	int 10h
	loop @b
	mov ax,0E0Dh			; and a CR again, for a good measure.
	int 10h
	jmp escape_noprint		; Finally, get out.	
	
key_pressed:				; We wish to continue with the ROM.
	pop dx				; Discard the tickcount...
	xor ax,ax			; Also discard the keypress...
	int 16h				; And continue with the ROM.
	
	cli				; Hold your breaths...
	
	mov ax,STACK_SEG		; Set stack pointer
	mov ss,ax
	mov sp,STACK_OFS
	
	sti				; Okay, interrupts enabled again
	
	mov ax,0003			; Set video mode to 80x25
	int 10h
	
	lea si,[WELCOME]		; Print out welcome screen.
	call print			; Give one more chance to continue with BIOS boot.
		
@@:					; Read key loop
	xor ax,ax
	int 16h
	cmp ah,1Ch			; ENTER key pressed
	je continue
	cmp ah,01h			; ESC key or read again
	je escape
	jmp @b
	
escape:	
	lea si,[QUITTING]		; ESC pressed. Print "Passing control to BIOS"
	call print
escape_noprint:
	xor ax,ax			; Clear the first two bytes (the ROM signature in RAM)
	mov word[cs:0],ax		; so that BIOS can free this memory block for the UMB area	
	int 19h				; Execute the BIOS bootstrap
	cli				; We shouldn't be here! Make the CPU freeze...
	hlt				; like Jack Nicholson inside the maze in "The Shining" :)
	
continue:
	lea si,[SYNC]			; Print "Waiting for connection..."
	call print	
	
@@:	
	xor bx,bx			; Determine input boot image file size in bytes.
	call readLPT			; BX - size in bytes (max. 64K)
	mov bl,al			; Store low order byte...
	call readLPT			; ... and the high order byte
	mov bh,al			; goes into the high order part of BX
	cmp bx,0			; Got something wacky?
	je @b				; Read again
		
	lea si,[ACTIVE_RX]		; Print "Receiving..."
	call print
	
	mov ax,IMG_DISK_LOAD_SEG	; Store the read data into ES:DI (ES=IMG_DISK_LOAD_SEG)
	mov es,ax			; Set segment register
	xor di,di			; DI=0 (number of bytes done)
	
read_file:
	cmp di,bx			; Have we read all bytes ?
	je read_finished		; Yes, jump	
	call readLPT			; Byte read from LPT in AL...
	stosb				; AL -> ES:DI
	jmp read_file			; Read the next byte
	
read_finished:				; We have the boot image in memory!
	xor di,di			; DI = 0
	
	lea si,[DONE]			; Display "Done"
	call print
	
	xor ax,ax			; Wait for a keypress
	int 16h
	
	call override_int13h		; Used to simulate the drive in BIOS calls.
	
	xor ax,ax			; If emulating drive B:, patch the BIOS equipment byte.
	mov es,ax			; ES = DS = 0
	mov ds,ax
	mov si,0411h			; Get equipment byte LOW ORDER
	mov al,[ds:si]			; ... to AL
	mov ah,IMG_DISK_DRIVE		; Compare it with our defined value
	cmp ah,0			; A: ?
	je @f
	cmp ah,1			; B: ?
	jne boot			; Nope - patch not required
	and al,7Fh			; B:. Okay, patch the number of floppies to 2
	or al,40h
@@:
	or al,1				; Enable bit 0 (the system booted from a floppy)
	mov [ds:si],al			; Apply the lower equipment byte
	
boot:	
	mov ax,IMG_DISK_LOAD_SEG	; The last thing we need to do, is...
	mov ds,ax			; ...to copy the boot sector of the virtual disk to 0:7c00h.
	xor si,si			; DS:SI = IMG_DISK_LOAD_SEG:0
	mov di,7C00h			; ES:DI = 0:7C00h (classic BIOS bootsector location)
	mov cx,512d			; A 512-byte bootsector that is.
	rep movsb			; Copy!
	
	mov dl,IMG_DISK_DRIVE		; Used by the boot sector
	mov di,7C00h			; Patch it, so that it "knows" the drive, nevertheless.
	mov [es:di+IMG_DISK_BOOT_DRIVE],dl
	
	push es				; Far jump to bootsector (0:7C00h)
	push di
	retf				; Transfer control to the bootsector!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
readLPT:				; Read two nibbles with software handshaking.
	push cx				; Returns AL - 1byte read (or waits indefinitely).
	push dx				; Get the low order 4-bit nibble first.
	mov dx,LPT1_ADDRESS		; LPT1 data port
	mov al,10h			; Set not busy signal (inverted at the input)
	out dx,al
	inc dx				; LPT1 status port
@@:
	in al,dx			; Are data available ?
	test al,80h			; (BUSY bit 7 == 0)
	jnz @b				; Nonzero - poll again
	mov cx,3			; Data available here.
	shr al,cl			; 8086 compatibility: shr al,3
	and al,0Fh			; Got a 4-bit low order nibble
	mov ah,al			; Save it in AH.
	dec dx				; LPT1 data port
	xor al,al			; AL=0 - signal data acknowledged
	out dx,al
	inc dx				; LPT1 status port
@@:
	in al,dx			; Wait for the sender app (bit 7 == 1)
	test al,80h
	jz @b
	dec dx				; LPT1 data port
	mov al,10h			; Set not busy signal (inverted)
	out dx,al
	inc dx				; LPT1 status port
@@:					; And now get the high order 4-bit nibble!
	in al,dx			; Are data available ?
	test al,80h			; (BUSY bit 7 == 0)
	jnz @b				; Nonzero - poll again
	shl al,1			; High order 4-bit nibble available.
	and al,0F0h			; Merge it with the existing low order nibble
	or ah,al			; into AH (since we are still using AL).
	dec dx				; LPT1 data port
	xor al,al			; AL=0 - signal data acknowledged
	out dx,al
	inc dx				; LPT1 status port
@@:
	in al,dx			; Wait for the sender app (bit 7 == 1)
	test al,80h
	jz @b
	mov al,ah			; Get the full 8-bit value into AL
	xor ah,ah			; We're not occupying the high-order register any longer
	pop dx				; Restore the other two CPU registers used here
	pop cx
	ret				; AL - byte read
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
override_int13h:			; Install a new INT 13h routine, in order to
	push ds				; simulate a read only RAM drive
	push ax
	push bx
	cli				; We don't want interrupts here
	xor ax,ax
	mov ds,ax			; DS = 0
	mov ax,word[ds:13h*4]		; Get offset of the original INT 13h handler
	mov bx,word[ds:13h*4+2]		; Get segment of the original INT 13h handler
	mov word[ds:OLD_INT13H*4],ax	; Copy the offset to point to OLD_INT13H ISR
	mov word[ds:OLD_INT13H*4+2],bx	; Copy the segment to point to OLD_INT13H ISR
	mov ax,shiny_new_int13h		; Get offset of the new INT 13h handler (here)
	mov bx,cs			; Segment of the new INT 13h handler; this code segment.
	mov word[ds:13h*4],ax		; Override original INT 13h offset
	mov word[ds:13h*4+2],bx		; Override original INT 13h segment
	sti				; Interrupts enabled
	pop bx
	pop ax
	pop ds
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
shiny_new_int13h:			; Shiny and new INT 13h handler that simulates a read only drive.
	pushf				; Based on ROMOS's INT 13h routine,
	cmp dl,IMG_DISK_DRIVE		; but I've made it to be 8086 compatible.
	je @f				; Is someone requesting our disk ? Yes, handle it
	popf
	int OLD_INT13H			; If not, invoke the old handler
	push bp
	mov bp,sp
	push ax
	lahf
	mov [bp+6],ah			; Correct the flags (accounting our pushes)
	pop ax
	pop bp
	iret				; Return from ISR
@@:
	cli				; Sorry, we need this.
	mov [cs:STORE_SP],sp		; Oh yeah baby!
	push ax				; Now we're talkin' !
	push cx				; Come on!
	push dx				; Push it in!
	push bx				; HARDER!!!
	push word[cs:STORE_SP]		; ..... Aah! WAIT!!! Gotta be careful - no push sp!!!
	push bp				; The darn thing lacks the PUSHA opcode :)
	push si
	push di				; Allright, that's 16 bytes...
	push ds				; + 2 bytes (DS).
	sti				; Breath again.
	cmp ah,02h			; AH=02 Read sectors (supported)
	je new_int13h_read
	cmp ah,03h			; AH=03 Write sectors (supported, but we won't do much)
	je new_int13h_write
	cmp ah,08h			; AH=08 Get drive parameters (supported)
	je new_int13h_params
	cmp ah,15h			; AH=15h Get disk type (supported)
	je new_int13h_disktype
	jmp new_int13h_end		; Everything else is unhandled
new_int13h_read:			; Simulate reading
	push ax
	mov di,bx			; ES:BX -> ES:DI
	xor si,si
	and cl,03Fh			; 6-bit sector number
	dec cl				; Convert it to 0-based
	mov ax,IMG_DISK_SECTORS		; Allright, I confess I really did not bother with this. :)
	mul dh				; Fading out.
	add si,ax
	mov ax,IMG_DISK_HEADS		; Oh, and there's no cylinder head sector validation here.
	mov bl,IMG_DISK_SECTORS		; You gotta be careful with the math. ;)
	mul bl				; Fading out again.
	mul ch
	add si,ax
	xor ch,ch
	add si,cx
	mov cx,9
	shl si,cl			; Another 8086 compatibility rig-up	
	mov ax,IMG_DISK_LOAD_SEG	; DS = IMG_DISK_LOAD_SEG
	mov ds,ax			; DS:SI ready, ES:DI ready
	pop dx				; PUSH AX (up there) POPped to DX
	mov cx,9
	shl dx,cl			; Ain't 8086 fun ?
	mov cx,dx			; CX now contains number of bytes to copy.
	rep movsb			; Copy memory - simulate disk reading!
	jmp new_int13h_end		; Our work here is done
new_int13h_write:			; Simulate writing
	mov bp,sp			; BP = stack pointer
	mov ax,1			; Carry flag (byte 1)
	or [bp+24],al			; Look kids! A stack unwind!.... (Carry flag in stack unwind)
	mov ax,3			; Error code 3, unsupported operation
	mov [bp+17],al			; ...Yaaaaaaay!.... (to AH in stack unwind)
	jmp new_int13h_end
new_int13h_params:			; Geometry
	mov bp,sp
	mov ch,IMG_DISK_CYLINDERS	; Number of cylinders
	mov cl,IMG_DISK_SECTORS		; Number of sectors per cylinder
	mov al,IMG_DISK_HEADS
	mov [bp+13],al			; Number of heads. DH in stack unwind.
	mov [bp+14],cx			; Modify CX in stack unwind
	jmp new_int13h_end
new_int13h_disktype:			; Non-removable read only disk
	mov bp,sp
	mov al,1
	mov [bp+17],al			; AH in stack unwind
	mov ax,0
	mov [bp+14],ax			; CX high word of sectors to 0 in stack unwind
	mov ax,IMG_DISK_CYLINDERS*IMG_DISK_HEADS*IMG_DISK_SECTORS
	mov [bp+12],ax			; DX contains that thing up there :) [total number of sectors]
	jmp new_int13h_end
new_int13h_end:				; The end
	cli				; Hold your breaths...
	pop ds				; And here we go!
	pop di
	pop si
	pop bp				; Pop pop pop
	add sp,2			; Beware of "pop sp" on an 8086!!!
	pop bx
	pop dx
	pop cx				; <= This is an example of a pop.
	pop ax
	popf				; Don't forget the flaaaags
	sti				; Breath!
	iret				; Whew.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print:					; Print a 0-terminated ASCII string in DS:SI
	mov ax,0E00h
	lodsb				; byte[DS:SI] => AL
	cmp al,00			; Terminating character
	je @f
	push bp				; I had an ancient BIOS that destroyed these two!!!
	push si
	int 10h				; Blurp it out
	pop si
	pop bp
	jmp print
@@:
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


INTERVAL		db 13,10,'Press any key to boot BootLPT/86...',7,0
WELCOME			db 'BootLPT/86 Boot ROM (c) J. Bogin',13,10
			db 13,10,'At first, connect this computer with a LapLink parallel cable to LPT1 port.'
			db 13,10,'Then run BOOTLPT.EXE on the other computer, so that it waits for a connection.',13,10
			db 13,10,'When you have done that, press ENTER to go.'
			db 13,10,'Otherwise, press ESC to exit BootLPT/86 and boot your system normally.'
			db 13,10,0
QUITTING		db 'Passing control to the BIOS.',13,10,0
SYNC			db 13,10,'Waiting for connection...',0
ACTIVE_RX		db 13
			times 25 db 32
			db 13,'Receiving...',0
DONE			db 13
			times 12 db 32
			db 13,'Done.',13,10,13,10,'Press any key to boot.',13,10,0					
					

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


STORE_SP		dw 0		; PUSHA/POPA on 8086 nonexistent. Careful with PUSH SP !


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Zero padding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


times ROM_SIZE*1024-($-$$) db 0