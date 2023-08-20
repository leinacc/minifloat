INCLUDE "defines.asm"

SECTION "Minifloat Display Code", ROM0

HEX_ROW = 4
SIGN_ROW = 6
EXP_ROW = 9
MANTISSA_ROW = 12
FLOAT_VAL_ROW = 15
RIGHT_COLUMN_OFFS = $b

TILE_EMPTY_BOX = $80
TILE_CROSSED_BOX = $81
TILE_CURSOR = $82


InitDisplayView::
	ld hl, Text_Minifloat
	ld a, 1
	call GetTilemapRow
	call PrintCenteredText

	ld hl, Text_DisplayView
	ld a, 2
	call GetTilemapRow
	call PrintCenteredText

	ld hl, Text_Hex
	ld a, HEX_ROW
	call GetTilemapRow
	inc e
	call PrintText
    ld a, e
    and $e0
    or RIGHT_COLUMN_OFFS
    ld e, a
	wait_vram
    ld a, "$"
    ld [de], a

	ld hl, Text_Sign
	ld a, SIGN_ROW
	call GetTilemapRow
	inc e
	call PrintText

	ld hl, Text_Exponent
	ld a, EXP_ROW
	call GetTilemapRow
	inc e
	call PrintText

	ld hl, Text_Mantissa
	ld a, MANTISSA_ROW
	call GetTilemapRow
	inc e
	call PrintText

	ld hl, Text_FloatValue
	ld a, FLOAT_VAL_ROW
	call GetTilemapRow
	inc e
	call PrintText

    xor a
    ld [wDisplayedValue], a
	ld [wCursorCol], a
	ld [wCursorSelectedCol], a
	ld [wCursorSelectedRow], a

    call PrintHexValue
    call PrintSignRow
	call DisplaySignBoxes
	call PrintExponentRow
	call DisplayExponentBoxes
	call PrintMantissaRow
	call DisplayMantissaBoxes
	call PrintFloatValue
	call DisplayCursor

    ret


PrintHexValue:
    ld a, HEX_ROW
    call GetTilemapRow
    ld a, e
    or RIGHT_COLUMN_OFFS+1
    ld e, a

    ld a, [wDisplayedValue]
    push af
    swap a
    and $0f
    call PrintHexChar
    inc e

    pop af
    and $0f
    call PrintHexChar
    ret


PrintSignRow:
    ld a, SIGN_ROW
    call GetTilemapRow
    ld a, e
    or RIGHT_COLUMN_OFFS
    ld e, a

    ld a, [wDisplayedValue]

; A - displayed value
PrintSign:
    bit 7, a
    ld b, "+"
    jr z, :+
    ld b, "-"
:	wait_vram
	ld a, b
	ld [de], a
	ret


DisplaySignBoxes:
	assert TILE_CROSSED_BOX == TILE_EMPTY_BOX+1
    ld a, SIGN_ROW+1
    call GetTilemapRow
	inc e
	inc e
	ld a, [wDisplayedValue]
	ld c, 1
	jp DisplayBoxes


PrintExponentRow:
    ld a, EXP_ROW
    call GetTilemapRow
    ld a, e
    or RIGHT_COLUMN_OFFS
    ld e, a

	push de
	ld b, strlen("Invalid")
	call ClearText
	pop de

	ld a, [wDisplayedValue]
	rla
	swap a
	and $0f
	cp $0f
	jr z, .printInvalid

	sub 7
	push af
	call PrintSign
	inc e
	pop af

	jp nc, PrintHexChar

	cpl
	inc a
	jp PrintHexChar

.printInvalid:
	ld hl, Text_Invalid
	jp PrintText


DisplayExponentBoxes:
	assert TILE_CROSSED_BOX == TILE_EMPTY_BOX+1
    ld a, EXP_ROW+1
    call GetTilemapRow
	inc e
	inc e
	ld a, [wDisplayedValue]
	add a
	ld c, 4
	jp DisplayBoxes


PrintMantissaRow:
    ld a, MANTISSA_ROW
    call GetTilemapRow
    ld a, e
    or RIGHT_COLUMN_OFFS
    ld e, a

	push de
	ld b, strlen("1.125")
	call ClearText
	pop de

	call PrintOne
	inc e

	call PrintDot
	inc e

	ld a, [wDisplayedValue]
	and $07
	rla
	jp PrintFloatDecimal


DisplayMantissaBoxes:
	assert TILE_CROSSED_BOX == TILE_EMPTY_BOX+1
    ld a, MANTISSA_ROW+1
    call GetTilemapRow
	inc e
	inc e
	ld a, [wDisplayedValue]
	swap a
	add a
	ld c, 3

; A - bitfield in upper bits
; C - num boxes/bits
; DE - dest addr
; Trashes B
DisplayBoxes:
.nextBox:
	ld b, TILE_EMPTY_BOX
	rla

	push af
	jr nc, :+
	inc b
:	wait_vram
	ld a, b
	ld [de], a
	pop af

	inc e
	inc e

	dec c
	jr nz, .nextBox
ret


; A - decimal part in bits 1-3, so integer part is in upper nybble
	assert (7<<1) * 10 < $100
; DE - dest
PrintFloatDecimal:
	and a
	jp z, PrintZero

.nextShift:
; B = 2A, so we can later add to 8A
	rla
	ld b, a
	rla
	rla
	add b

	push af
	swap a
	and $0f
	call PrintHexChar
	inc e
	pop af

	and $0f
	ret z
	
	jr .nextShift


PrintNaN:
	ld hl, Text_NaN
	jp PrintText


; DE - dest
PrintFloatValue:
    ld a, FLOAT_VAL_ROW+1
    call GetTilemapRow
    inc e
	inc e

	push de
	ld b, strlen("+0.0087890625") ; 2^-7 * 1.125
	call ClearText
	pop de

	ld a, [wDisplayedValue]
	ld c, a
; Check NaN - %0111 1xxx - where x is non-0, ie $79 to $7f
	and $7f
	cp $79
	jr nc, PrintNaN

	ld a, c
	call PrintSign
	inc e

; Check non-NaN special values
	ld a, c
	and $7f
	ld c, a ; ignore sign from here on
	jp z, PrintZero

	cp $78 ; Infinity
	jr z, .printInfinity

; Print regular value
	ld a, c
	and $78
	rla
	swap a
	sub 7
	ld b, a ; B is exponent

	ld a, c
	and $07
	rla
	swap a
	ld c, a ; C is float part in upper 3 bits

	ld a, b
	and a
	jr z, .print1dotDecimal
	cp $80
	jr nc, .printSub0

	ld a, 1 ; A is whole part

	.nextShiftUp:
		sla c
		rla
		dec b
		jr nz, .nextShiftUp

	cp 10
	jr c, .do1s
	cp 100
	jr c, .do10s

	ld h, 0

	.nextSub100:
		sub 100
		jr c, .print100s
		inc h
		jr .nextSub100

.print100s:
	add 100
	push af
	ld a, h
	call PrintHexChar
	inc e
	pop af

.do10s:
	ld h, 0

	.nextSub10:
		sub 10
		jr c, .print10s
		inc h
		jr .nextSub10

.print10s:
	add 10
	push af
	ld a, h
	call PrintHexChar
	inc e
	pop af

.do1s:
	call PrintHexChar
	inc e

	jr .printDotAfterWhole

.print1dotDecimal:
	call PrintOne
	inc e

.printDotAfterWhole:
	call PrintDot
	inc e

	ld a, c
	swap a
	jp PrintFloatDecimal

.printInfinity:
	ld hl, Text_Infinity
	jp PrintText

.printSub0:
	call PrintZero
	inc e
	call PrintDot
	inc e

; HL (mask %yyyy xxxx xxxx xxx0) = the float part * 2^-B
; where Y is the integer part, and X is the decimal part
	ld a, c
	swap a
	or $10
	ld c, a
	xor a
	.nextShiftDown:
		rr c
		rra
		inc b
		jr nz, .nextShiftDown

	ld h, c
	ld l, a

	.nextDigit:
	; BC = 2HL, so we can later add to 8HL
		rl l
		rl h
		ld c, l
		ld b, h
		rl l
		rl h
		rl l
		rl h
		add hl, bc

		ld a, h
		swap a
		and $0f
		call PrintHexChar
		inc e

		ld a, h
		and $0f
		ld h, a
		or l
		ret z
		
		jr .nextDigit


; C - highest bit index for float component
; Trashes A, B, and HL
ToggleBitfieldValFromCol:
	ld a, [wCursorSelectedCol]
	ld b, a
	ld a, c
	sub b
	ld hl, Bitfield
	add l
	ld l, a
	adc h
	sub l
	ld h, a
	ld b, [hl]

	ld a, [wDisplayedValue]
	xor b
	ld [wDisplayedValue], a
	ret


Bitfield:
	db $01, $02, $04, $08, $10, $20, $40, $80


ToggleSignBox:
	ld a, [wDisplayedValue]
	xor $80
	ld [wDisplayedValue], a

    call PrintHexValue
    call PrintSignRow
	call DisplaySignBoxes
	jp PrintFloatValue


ToggleExponentBox:
	ld c, 6
	call ToggleBitfieldValFromCol

    call PrintHexValue
    call PrintExponentRow
	call DisplayExponentBoxes
	jp PrintFloatValue


ToggleMantissaBox:
	ld c, 2
	call ToggleBitfieldValFromCol

    call PrintHexValue
    call PrintMantissaRow
	call DisplayMantissaBoxes
	jp PrintFloatValue


UpdateDisplayView::
	ldh a, [hPressedKeys]
	bit PADB_UP, a
	jr nz, .moveCursorUp

	bit PADB_DOWN, a
	jr nz, .moveCursorDown

	bit PADB_LEFT, a
	jr nz, .moveCursorLeft

	bit PADB_RIGHT, a
	jr nz, .moveCursorRight

	bit PADB_A, a
	ret z

; Handle toggling a box
	ld a, [wCursorSelectedRow]
	and a
	jr z, ToggleSignBox
	
	dec a
	jp z, ToggleExponentBox

	jp ToggleMantissaBox

.moveCursorUp:
	call HideCursor

	ld a, [wCursorSelectedRow]
	dec a
	cp $ff
	jr nz, :+
	ld a, 2
:	ld [wCursorSelectedRow], a
	
	call SetCursorSelectedCol
	jp DisplayCursor

.moveCursorDown:
	call HideCursor

	ld a, [wCursorSelectedRow]
	inc a
	cp 3
	jr nz, :+
	xor a
:	ld [wCursorSelectedRow], a
	
	call SetCursorSelectedCol
	jp DisplayCursor

.moveCursorLeft:
	call HideCursor

	ld a, [wCursorSelectedRow]
	call BequCursorOptMaxForRow
	dec b

	ld a, [wCursorSelectedCol]
	dec a
	cp $ff
	jr nz, :+
	ld a, b
:	ld [wCursorSelectedCol], a
	ld [wCursorCol], a

	jp DisplayCursor

.moveCursorRight:
	call HideCursor

	ld a, [wCursorSelectedRow]
	call BequCursorOptMaxForRow

	ld a, [wCursorSelectedCol]
	inc a
	cp b
	jr nz, :+
	xor a
:	ld [wCursorSelectedCol], a
	ld [wCursorCol], a

	jp DisplayCursor


; A - wCursorSelectedRow
SetCursorSelectedCol:
	call BequCursorOptMaxForRow

	ld a, [wCursorCol]
	cp b
	jr c, .setSelectedCol

	ld a, b
	dec a

.setSelectedCol:
	ld [wCursorSelectedCol], a
	ret


BequCursorOptMaxForRow:
	ld b, 1
	and a
	ret z

	ld b, 4
	cp 1
	ret z

	ld b, 3
	ret


; Trashes A, B, C and DE
DisplayCursor:
	ld c, TILE_CURSOR

; C - tile idx to place at cursor position
; Trashes A, B and DE
DispAtCursorPos:
; Cursor rows are 3 tiles apart
	ld a, [wCursorSelectedRow]
	ld b, a
	add a
	add b
	add SIGN_ROW+1
	call GetTilemapRow
	ld a, [wCursorSelectedCol]
	add a
	inc a
	add e
	ld e, a
	adc d
	sub e
	ld d, a
	wait_vram
	ld a, c
	ld [de], a
	ret


HideCursor:
	xor a
	jr DispAtCursorPos


Text_Minifloat:
	db "Mini-float", 0

Text_DisplayView:
	db "Display View", 0

Text_Hex:
	db "Hex:", 0

Text_Sign:
	db "Sign:", 0

Text_Exponent:
	db "Exponent:", 0

Text_Invalid:
	db "Invalid", 0

Text_Mantissa:
	db "Mantissa:", 0

Text_FloatValue:
	db "Float value:", 0

Text_Infinity:
	db "Infinity", 0

Text_NaN:
	db "NaN", 0


SECTION "Minifloat Display Ram", WRAM0

wDisplayedValue: db
wCursorSelectedRow: db ; 0-2
wCursorCol: db ; 0-3
wCursorSelectedCol: db ; ie min(wCursorCol, <max col for row>)
