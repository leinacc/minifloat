INCLUDE "defines.asm"

SECTION "Text", ROM0

LoadFont::
; Main ascii font
	assert Font.end-Font <= $9800-$9200
	ld de, Font
	ld hl, $9200
	ld bc, Font.end-Font
	; Increment B if C is non-zero
	dec bc
	inc b
	inc c
	.loop:
		wait_vram
		ld a, [de]
		ld [hl+], a
		ld [hl+], a
		inc de
		dec c
		jr nz, .loop
		dec b
		jr nz, .loop

; Misc
    ld de, .misc
    ld hl, $8800
    ld bc, .end-.misc
    jp LCDMemcpy

.misc:
    dw `00000000
    dw `03333333
    dw `03000003
    dw `03000003
    dw `03000003
    dw `03000003
    dw `03000003
    dw `03333333

    dw `00000000
    dw `03333333
    dw `03300033
    dw `03030303
    dw `03003003
    dw `03030303
    dw `03300033
    dw `03333333

    dw `00000000
    dw `03000000
    dw `03300000
    dw `03330000
    dw `03333000
    dw `03330000
    dw `03300000
    dw `03000000
.end:


Font:
	incbin "res/ascii.1bpp"
.end:


; DE - dest
; HL - src
; $00 is the terminator
PrintText::
.nextChar:
    wait_vram
    ld a, [hl+]
    and a
    ret z

    ld [de], a
    inc e
    jr .nextChar


; DE - dest of start of row
; HL - src
; $00 is the terminator
; Trashes B
PrintCenteredText::
    push hl
    ld b, 0

    .nextChar:
        ld a, [hl+]
        and a
        jr z, .gotLen

        inc b
        jr .nextChar

.gotLen:
    ld a, SCRN_X_B
    sub b
    srl a
    add e
    ld e, a
    adc d
    sub e
    ld d, a
    
    pop hl
    jp PrintText


; A - hex value
; DE - dest
; Trashes B
PrintHexChar::
    cp 10
    jr c, .printNum

    add "A"-10
    jr .printVal

.printNum:
    add "0"

.printVal:
    ld b, a
    wait_vram
    ld a, b
    ld [de], a
    ret


; B - len
; DE - dest
; Trashes A
ClearText::
.nextTile:
    wait_vram
    xor a
    ld [de], a
    inc e
    dec b
    jr nz, .nextTile

    ret


PrintOne::
	wait_vram
	ld a, "1"
	ld [de], a
	ret


PrintZero::
	wait_vram
	ld a, "0"
	ld [de], a
	ret


PrintDot::
	wait_vram
	ld a, "."
	ld [de], a
	ret
