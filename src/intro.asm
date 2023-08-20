INCLUDE "defines.asm"

SECTION "Intro", ROMX

Intro::
	call LoadFont
	ld a, %11100100
	ldh [hBGP], a

	call InitDisplayView

.infLoop:
	rst WaitVBlank
	call UpdateDisplayView
	jr .infLoop
