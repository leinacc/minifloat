INCLUDE "defines.asm"

SECTION "Utils", ROM0

; A - row idx
GetTilemapRow::
	ld d, HIGH(_SCRN0)
	bit 4, a
	jr z, :+
	ld d, HIGH(_SCRN0)+2
:	and $0f
	swap a
	rla
	jr nc, :+
	inc d
:	ld e, a
	ret
