; org: all instructions from now on start in 7c00h
; 7c00h is where the bios loads the 1st sector
; assembler formatting:
; column 0: labels
; tab: commands
	org 7c00h
; at boot: real mode (like it's 8086)
; we have to tell assembler that code is 16bit mode
	bits 16

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

; waiting for keypress
; x86 has 2 special instructions to read/write from I/O
; devices: in and out (because some processors have different
; address spaces for devices and for memory, arm not)
.key_wait:
	in al, 64h	; 60h = keyb data port, 64h = keyb status port
	and al, 1	; 1 = OUTBUF_FULL = the keyb controller out buf is full
	jz .key_wait

	in al, 60h	; reads the keyb that was pressed to reset the flag

; load 2nd sector from boot device and jump to it
; bios helpers, 13h = disk io
	mov ax, 0
	mov ds, ax
	mov ah, 02h 			; call 2: read sectors into memory
	mov al, 1				; number of sectors to read
	mov ch, 0				; low 8 bits of cylinder number
	mov cl, 2				; sector number that starts from 1
	mov dh, 0				; head number
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

; fix palette (256 colors)
	
.inf_loop:
	jmp .inf_loop
