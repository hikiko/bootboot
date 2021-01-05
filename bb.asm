; author Eleni Maria Stea <elene.mst@gmail.com>
; my x86 assembly helloworld :)

; org: all instructions from now on start in 7c00h
; 7c00h is where the bios loads the 1st sector
; assembler formatting:
; column 0: labels
; tab: commands
	org 7c00h
; at boot: real mode (like it's 8086)
; we have to tell assembler that code is 16bit mode
	bits 16

; macros
%macro SETPAL 4	; 4 = num arguments of macro (index rgb)
	mov dx, 3c8h	; index register
	mov al, %1
	out dx, al
	inc dx
	mov al, %2 >> 2
	out dx, al
	mov al, %3 >> 2
	out dx, al
	mov al, %4 >> 2 ; shift to give the value from 0 - 255
	out dx, al
%endmacro

bios_param_block:
	jmp start	; 2 bytes
	nop		; 1 byte
	; start of BPB at offset 3
	db "BSPL 0.1"	; 03h: OEM ident, 8 bytes
	dw 512		; 0bh: bytes per sector
	db 1		; 0dh: sectors per cluster
	dw 1		; 0eh: reserved sectors (including boot record)
	db 2		; 10h: number of FATs
	dw 224		; 11h: number of dir entries
	dw 2880		; 13h: number of sectors in volume
	db 0fh		; 15h: media descriptor type (f = 3.5" HD floppy)
	dw 9		; 16h: number of sectors per FAT
	dw 18		; 18h: number of sectors per track
	dw 2		; 1ah: number of heads
	dd 0		; 1ch: number of hidden sectors
	dd 0		; 20h: high bits of sector count
	db 0		; 24h: drive number
	db 0		; 25h: winnt flags
	db 28h		; 26h: signature(?)
	dd 0		; 27h: volume serial number
	db "BOOTBOOT   "; 2bh: volume label, 11 bytes
	db "FAT12   "	; 36h: filesystem id, 8 bytes

start:
	mov sp, 7c00h

; initialize segment registers
	mov ax, 0
	mov ds, ax
	mov es, ax
	mov ss, ax

; save dl value
; bios sets in dl the num of the drive that
; loaded you so if you overwrite dl you can reuse it
	mov [saved_drive_num], dl

; service 0: set videomode ah
; param: which video mode (13h: 320x200, 256 colors) al
; ax: ah (high 8 bits), al (low)
; call 0 = set_videomode it expects the video mode in al
; ah : 0, al: 13, ax:ahal
	mov ax, 13h

; calling video bios
; software interrupt 10h
; what's the value of ah? al=video mode when ah=0
; which set which video mode
	int 10h
	mov ax, 3
	call clearscreen

; load 2nd sector from boot device and jump to it
; bios helpers, 13h = disk io
	mov ax, 0
	mov ds, ax
	mov ah, 02h 				; call 2: read sectors into memory
	mov al, 3					; number of sectors to read
	mov ch, 0					; low 8 bits of cylinder number
	mov cl, 2					; sector number that starts from 1
	mov dh, 0					; head number
	mov dl, [saved_drive_num]	; 8bits
	mov bx, sector_2
	int 13h
; error check: if carry flag isn't set jump to loaded code
	jnc	sector_2
.inf_loop:
	jmp .inf_loop


clearscreen:
; video ram is mapped in a0000
; first 64000 bytes appear immediately on screen
; mem addresses in real mode have 2 parts: segment and offset
; bits overlap: segment is shifted by 4 and is added to the
; overlapping offset (20 bits number = x86's addressable space = 1MB)
; segment register for the segment: es, ds, cs, ss (and: fs, gs)
; default register = ds (data segment), es = extra segment
; cs = code segment cs:ip (it points where to read instructions and is
; automatically set to 7c0 (7c00h)
; offset can be paired with any register
; pair the registers
	push ax
	mov ax, 0a000h
	mov ds, ax
	mov di, 0
	pop ax
; counter cx
	mov cx, 64000

.loop_begin:
; dereferrence[] address
	mov [di], al
	inc	di
	dec cx
; when cx is 0, 0 flag is set
	jnz .loop_begin
	ret

saved_drive_num:
	db 0 ; define byte 0

; assembler trick: write as many 0 needed to fill 510 bytes
; $ <- means here
	times 510-($-$$) db 0
	dw 0aa55h

sector_2:
; disp palette
	mov ax, 0a000h	; video segment points to video memory
	mov ds, ax
	mov bx, 0		; video offset
	mov cx, 0		; y
.y_loop:
	mov dx, 0		; x
.x_loop:
	mov ax, dx
	test ax, 0ff00h	; lower 8 bits 0, highest 1, Z flag is set while < 256, test = and but doesnt change ax	
	jz	.skip_clear_ax
	mov al, 0
.skip_clear_ax:
	mov [bx], al	; pixel written on screen
	inc bx			; increment
	inc dx
	cmp dx, 320
	jnz .x_loop
	inc cx
	cmp cx, 200
	jnz .y_loop

; setup grayscale palette (64 first colors because ramdac uses 6 bits / color so 2^6)
; ram dac (digital to analog converter) tis vga
; in a ^ register which index we want to write
; 3 writes in another ramdac register (data)
; ramdac feature: when you need to set a palette
	mov al, 0		; first index
	mov dx, 3c8h	; ramdac index registe
	out dx, al		; because io port (address) > 255
	inc dx			; ramdac data register: 3c9h
.pal_loop:
	out dx, al		; r
	out dx, al		; g
	out dx, al		; b
	inc al
	test al, 3fh	; test with 3fh to keep the lowest 6 bits of al
	jnz .pal_loop

; setup lovebug palette
	SETPAL 64, 0, 255, 0	; chr(64) = @
	SETPAL 65, 0, 0, 0 		; A
	SETPAL 66, 50, 50, 50	; B
	SETPAL 67, 57, 4, 4		; C
	SETPAL 68, 108, 9, 9	; D
	SETPAL 69, 164, 4, 4	; E

; disable all interrupts (for bios to not read the keyboard)
	cli

	; ds on 0 because rand uses it for accesses to mem
	xor ax, ax
	mov ds, ax
	mov ax, backbuffer
	shr ax, 4 ; to use it as segment! (shift right)
	mov es, ax
main_loop:
; draw to back buffer
	xor bx, bx		; mov bx, 0 to clear the register
.static_loop:
	call rand
	and ax, 3fh		; last six bits of eax (see rand)
	mov [es:bx], al    ; [] -> bx offset and default segment ds we change it to es
	inc bx
	cmp bx, 64000	; num pixels
	jnz .static_loop

	; draw lovebug
	mov di, (200 - 32) * 320 + 160 - 16
	mov ax, [num_frames]
	shr ax, 2		; /4
	cmp ax, 200 - 32
	jz .end
	mov bx, ax
	shl ax, 8
	shl bx, 6
	add ax, bx
	sub di, ax
	call rand
	and ax, 3		; random value in 0, 2
	sub ax, 1		; random value in -1, 1
	add di, ax
	mov si, lovebug
	call blit32

; wait for vblank
.vsync_blank:
	mov dx, 3dah
	in al, dx
	and al, 8
	jnz	.vsync_blank
.vsync_visible:
	in al, dx
	and al, 8 		; the 4th bit is 1 when we are in the vertical blanking period
	jz .vsync_visible

; copy backbuffer to video memory
	push es
	push ds
	mov ax, es
	mov ds, ax			; ds points to backbuffer now (was in es)
	xor si, si
	mov ax, 0a000h
	mov es, ax
	xor di, di			; write in es:di
	mov ecx, 16000		; 16000 double words (dwords) = 64000 bytes
	rep movsd
	pop ds
	pop es

	inc word [num_frames]
;   moved above:
;	cmp word [num_frames], 200 - 32
;	jz .end

	in al, 64h		; 60h = keyb data port, 64h = keyb status port
	and al, 1		; 1 = OUTBUF_FULL = the keyb controller out buf is full
	jz main_loop	; no key pressed, loop back
	in al, 60h		; reads the keyb that was pressed to reset the flag

.end:
; re-enable all interrupts
	sti

; text mode
	mov ax, 3		; text mode
	int 10h
; use bios to print
; Int 10/AH=0Eh
	mov si, str_frames  
	call print_str
	mov ax, [num_frames]
	call print_num
	mov si, str_newline
	call print_str

	mov dx, 80h		; default to load from drive 80h
	cmp byte [saved_drive_num], 80h
	jnz .not_hd
	inc dl			; next hd
.not_hd:
; load hard disk boot sector
	xor ax, ax
	mov es, ax
	mov bx, 7c00h
	mov ah, 02h
	mov al, 1
	mov ch, 0
	mov cl, 1
	mov dh, 0
	int 13h
	jnc 7c00h
.inf_loop:
	jmp .inf_loop

; reads from ds:si writes to es:di (bug top left corner)
; knows how many pixels on screen and how many in the lovebug
blit32:
	mov cx, 1024	; 32 x 32 lovebug
.loop:
	mov al, [ds:si]
	cmp al, 64
	jz .skip_pixel
	mov [es:di], al
.skip_pixel:
	inc si
	inc di

	dec cx				; dec the num_pixels we want to write 
	jz	.end			; all pixels have been written
	test cx, 1fh		; keep 5 least significant bits, 3 most significant bits 0
	jnz .loop			; when this is 0 we are in a multiple of 32
	add di, 320 - 32	; screen width - bug width
	jmp .loop
.end:
	ret

print_str:
	;Int 10/AH=0Eh
	mov al, [si]
	test al, al
	jz .end
	mov ah, 0eh
	xor bx, bx
	int 10h
	inc si
	jmp print_str
.end:
	ret

print_num:
	; converts digits to ascii - print
	push word 0
	test ax, ax
	jnz .conv_loop
	push word '0'
	jmp .conv_end
.conv_loop:
	test ax, ax
	jz .conv_end
	xor dx, dx
	mov cx, 10
	div cx	; ax contains cx / 10
	add dl, '0'
	push dx
	jmp .conv_loop
.conv_end:
	mov bp, sp
	mov ax, [ss:bp]
	test ax, ax
	jz .print_end
	mov ah, 0eh
	xor bx, bx
	int 10h
	add sp, 2
	jmp .conv_end
.print_end:
	add sp, 2
	ret

; random number generator
; by nuclear
rand:
	mov eax, [randval]
	mul dword [randmul]
	add eax, 12345
	and eax, 0x7fffffff
	mov [randval], eax
	shr eax, 16
	ret

randmul dd 1103515245
randval dd 0ace1h

num_frames dw 0
str_frames db 'frames: ', 0
str_newline db 13, 10, 0	; crlf carriage return line feed newline

; db to write pixels transparent 255
; @: transparent
; A: black
; B: gray
; C: dark red
; D: medium red
; E: red
lovebug:
	db '@@@@BAAAB@@@@@BAAB@@@@@BAAAB@@@@'
	db '@@@@@@@@AAAB@BAAAAB@BAAA@@@@@@@@'
	db '@@@@@@@@@@@AAAAAAAAAA@@@@@@@@@@@'
	db '@@@@@@@@@@@@AAAAAAAA@@@@@@@@@@@@'
	db '@@@@@@@@@@@@AAAAAAAA@@@@@@@@@@@@'
	db '@@@@@@@@@@@@BAAAAAAB@@@@@@@@@@@@'
	db '@@@@@@@@@@@@@AAACAA@@@@@@@@@@@@@'
	db '@@@@@@@@@@CDDEECDEEDDC@@@@@@@@@@'
	db '@@@@@@@CDDEEEEEDCEEEEEDDC@@@@@@@'
	db '@@@@@CDEEEEDCDECDEDCDEEEEDC@@@@@'
	db '@@@@CDDCDEECACEDCECACEEDCDDC@@@@'
	db '@@@CDECACEEDCDECDEDCDEECACEDC@@@'
	db '@@@DEEDCDEEEEEEDCEEEEEEDCDEED@@@'
	db '@@CEEEEEEEEEEEDCDDEEEEEEEEEEEC@@'
	db '@@DEDCDEEEDCDECACCEDCDEEEDCDED@@'
	db '@@EECACEEECACEDCDDECACEEECACEE@@'
	db '@CEEDCDEEEDCDEEDCEEDCDEEEDCDEEC@'
	db '@DEEEEEEEEEEEEDCCDEEEEEEEEEEEED@'
	db 'CEEEEEEDCDEEEDCBACDEEEDCDEEEEEEC'
	db 'CEDCDEECACEEDCAABACDEECACEEDCDEC'
	db 'DECACEEDCDEDCAABAAACDEDCDEECACED'
	db 'DEDCDEEEEEDCAAAABAAACDEEEEEDCDED'
	db 'CEEEEEEEEECAAAABAAAAACEEEEEEEEEC'
	db 'CEEEEEEEEDAAAAAABAAAAADEEEEEEEEC'
	db '@DEDCDEEDCAAAAABAAAAAACDEEDCDED@'
	db '@DECACEDCAAAAAAABAAAAAACDECACED@'
	db '@DEDCDDCAAAAAAABAAAAAAAACDDCDED@'
	db '@CEEEECAAAAAAAAABAAAAAAAACEEEEC@'
	db '@@EEED@AAAAAAAABAAAAAAAAA@DEEE@@'
	db '@@EEDC@@AAAAAAAABAAAAAAA@@CDEE@@'
	db '@@DEC@@@@@AAAAABAAAAAA@@@@@CED@@'
	db '@@@C@@@@@@@@AAAABAAA@@@@@@@@C@@@'

	align	16	; if the address is not a multiple of 16 add some 0 to become one (because we'll use backbuffer as a segment)
backbuffer:
