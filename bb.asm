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

; service 0: set videomode ah
; param: which video mode (13h: 320x200, 256 colors) al
; ax: ah (high 8 bits), al (low)
	mov ax, 13h

; calling video bios
; software interrupt 10h
	int 10h
	call clearscreen

; infinite loop
end:
	jmp end

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
	mov ax, 0a000h
	mov ds, ax
	mov di, 0
; counter cx
	mov cx, 64000
	mov ax, 3
.loop_begin:
; dereferrence[] address
	mov [di], al
	inc	di
	dec cx
; when cx is 0, 0 flag is set
	jnz .loop_begin
	ret

; assembler trick: write as many 0 needed to fill 510 bytes
; $ <- means here
	times 510-($-$$) db 0
	dw 0aa55h
