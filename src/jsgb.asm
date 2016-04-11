; system includes
INCLUDE	"lib/hardware.inc"


;****************************************************************************************************************************************************
;*	definitions
;****************************************************************************************************************************************************

; $ff80 to $fffe is 128 bytes of internal RAM
STACK_TOP				equ		$fff4		; put the stack here

; video ram display locations
TILES_MEM_LOC_0			equ		$8800		; tile map tiles only
TILES_MEM_LOC_1			equ		$8000		; tile maps and sprite tiles

MAP_MEM_LOC_0			equ		$9800		; background and window tile maps
MAP_MEM_LOC_1			equ		$9c00		; (select which uses what mem loc in LCDC_CONTROL register)


TILES_PER_LINE  equ  $20
ANIMATION_CYCLE equ $20

;****************************************************************************************************************************************************
;*	cartridge header
;****************************************************************************************************************************************************

	SECTION	"Org $00",HOME[$00]
RST_00:	
	jp	$100

	SECTION	"Org $08",HOME[$08]
RST_08:	
	jp	$100

	SECTION	"Org $10",HOME[$10]
RST_10:
	jp	$100

	SECTION	"Org $18",HOME[$18]
RST_18:
	jp	$100

	SECTION	"Org $20",HOME[$20]
RST_20:
	jp	$100

	SECTION	"Org $28",HOME[$28]
RST_28:
	jp	$100

	SECTION	"Org $30",HOME[$30]
RST_30:
	jp	$100

	SECTION	"Org $38",HOME[$38]
RST_38:
	jp	$100

	SECTION	"V-Blank IRQ Vector",HOME[$40]
VBL_VECT:
  jp  VBlankHandler
	
	SECTION	"LCD IRQ Vector",HOME[$48]
LCD_VECT:
	reti

	SECTION	"Timer IRQ Vector",HOME[$50]
TIMER_VECT:
	reti

	SECTION	"Serial IRQ Vector",HOME[$58]
SERIAL_VECT:
	reti

	SECTION	"Joypad IRQ Vector",HOME[$60]
JOYPAD_VECT:
	reti
	
	SECTION	"Start",HOME[$100]
	nop
	jp	Start

	; $0104-$0133 (Nintendo logo - do _not_ modify the logo data here or the GB will not run the program)
	DB	$CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
	DB	$00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
	DB	$BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E

	; $0134-$013E (Game title - up to 11 upper case ASCII characters; pad with $00)
	DB	"JELLYSPLASH"
		;0123456789A

	; $013F-$0142 (Product code - 4 ASCII characters, assigned by Nintendo, just leave blank)
	DB	"    "
		;0123

	; $0143 (Color GameBoy compatibility code)
	DB	$00	; $00 - DMG 
			; $80 - DMG/GBC
			; $C0 - GBC Only cartridge

	; $0144 (High-nibble of license code - normally $00 if $014B != $33)
	DB	$00

	; $0145 (Low-nibble of license code - normally $00 if $014B != $33)
	DB	$00

	; $0146 (GameBoy/Super GameBoy indicator)
	DB	$00	; $00 - GameBoy

	; $0147 (Cartridge type - all Color GameBoy cartridges are at least $19)
	DB	$00	;

	; $0148 (ROM size)
	DB	$00	;

	; $0149 (RAM size)
	DB	$00	; $00 - None

	; $014A (Destination code)
	DB	$00	; $01 - All others
			; $00 - Japan

	; $014B (Licensee code - this _must_ be $33)
	DB	$33	; $33 - Check $0144/$0145 for Licensee code.

	; $014C (Mask ROM version - handled by RGBFIX)
	DB	$00

	; $014D (Complement check - handled by RGBFIX)
	DB	$00

	; $014E-$014F (Cartridge checksum - handled by RGBFIX)
	DW	$00



;****************************************************************************************************************************************************
;*	Program Start
;****************************************************************************************************************************************************


	SECTION "Program Start",HOME[$0150]
Start:
	; init the stack pointer
	ld		sp, $FFFE

	; enable only vblank interrupts
	ld		a, IEF_VBLANK			; set vblank interrupt bit
	ldh		[rIE], a	; load it to the hardware register

	; standard inits
	sub		a	;	a = 0
	ldh		[rSTAT], a	; init status
	ldh		[rLCDC], a	; init LCD to everything off
	ldh		[rSCX], a	; background map will start at 0,0
	ldh		[rSCY], a

	sub		a
	ld		[vblank_flag], a

  ld    [switch], a
  ld    a, ANIMATION_CYCLE
  ld    [switch_timer], a


	; load the tiles
	ld		bc, jellysplash_tile_data
	call	LoadTiles

  call  ClearMap

  ld    b, $01
  ld    c, $05
  ld    de, TILES_PER_LINE * $5 + $7
  call  LoadAtPosition

  ld    b, $03
  ld    c, $07
  ld    de, TILES_PER_LINE * $5 + $A
  call  LoadAtPosition

  ld    b, $01
  ld    c, $05
  ld    de, TILES_PER_LINE * $8 + $A
  call  LoadAtPosition

  ld    b, $03
  ld    c, $07
  ld    de, TILES_PER_LINE * $8 + $7
  call  LoadAtPosition


	; init the palettes
	call	InitPalettes


	; set display options
	ld		a, LCDCF_ON + LCDCF_BGON + LCDCF_WINOFF + LCDCF_OBJOFF + LCDCF_OBJ8 + LCDCF_BG8000 + LCDCF_BG9800 + LCDCF_WIN9C00
	ldh		[rLCDC],a

	; allow interrupts to start occuring
	ei


; main game loop
.Game_Loop
	; don't do a frame update unless we have had a vblank
	ld		a, [vblank_flag]
	cp		0
	jp		z, .end


	; reset vblank flag
	ld		a, 0
	ld		[vblank_flag], a

.end
	jp		.Game_Loop




;***************************************************************
;* Subroutines
;***************************************************************

	SECTION "Support Routines",HOME



;----------------------------------------------------
; load the tiles from ROM into the tile video memory
;
; IN:	bc = address of tile data to load
;----------------------------------------------------
LoadTiles:
	ld		hl, TILES_MEM_LOC_1	; load the tiles to tiles bank 1

	ld		de, 4 * 16
	ld		d, $10  ; 16 bytes per tile
	ld		e, 12  ; number of tiles to load

.loop
	; only write during
	ldh		a, [rSTAT]	; get the status
	and		STATF_BUSY			; don't write during sprite and transfer modes
	jr		nz, .loop

	ld		a, [bc]		; get the next value from the source
	ld		[hli], a	; load the value to the destination, incrementing dest. ptr
	inc		bc			; increment the source ptr

	; now loop de times
	dec		d
	jp		nz, .loop
	dec		e
	jp		nz, .loop

	ret




;----------------------------------------------------
; Clear background map
;----------------------------------------------------
ClearMap:
  ld    hl, MAP_MEM_LOC_0

  ld    d, $20
  ld    e, $20

.loop
	; TODO turn into macro
	ldh		a, [rSTAT]	; get the status
	and		STATF_BUSY			; don't write during sprite and transfer modes
	jr		nz, .loop

  ld    a, 0
  ld    [hli], a

  dec   d
  jp    nz, .loop
  ld    d, $20

  dec   e
  jp    nz, .loop

  ret


;----------------------------------------------------
; load tile at position
;
; IN:	b = Tile address upper part
;     c = Tile address lower part
;     de = tile pos
;----------------------------------------------------
LoadAtPosition:
	ld		hl, MAP_MEM_LOC_0	; load the map to map bank 0
  add   hl, de

.loop
	; only write during
	ldh		a, [rSTAT]	; get the status
	and		STATF_BUSY			; don't write during sprite and transfer modes
	jr		nz, .loop

  ld    a, b
  ld    [hli], a
  inc   a
  ld    [hli], a

  ld    de, TILES_PER_LINE - $02
  add   hl, de  ; Go one line down
  ld    a, c
  ld    [hli], a
  inc   a
  ld    [hli], a

  ret

;----------------------------------------------------
; init the palettes to basic
;----------------------------------------------------
InitPalettes:
	ld		a, %11100100	; set palette colors

	; load it to all the palettes
	ldh		[rBGP], a
	ldh		[rOBP0], a
	ldh		[rOBP1], a

	ret


;----------------------------------------------------
; V-Blank interrupt handler
;----------------------------------------------------
VBlankHandler:
  ld    a, [switch_timer]
  dec   a
  ld    [switch_timer], a

  jr    z, .switch_tiles
  jp    .end


.switch_tiles
  ld   a, [switch]
  cp   0
  jr   z, .switch_to_1


.switch_to_0
  ld    a, 0
  ld    [switch], a

  ld    b, $03
  ld    c, $07
  ld    de, TILES_PER_LINE * $5 + $7
  call  LoadAtPosition

  ld    b, $01
  ld    c, $05
  ld    de, TILES_PER_LINE * $5 + $A
  call  LoadAtPosition

  ld    b, $03
  ld    c, $07
  ld    de, TILES_PER_LINE * $8 + $A
  call  LoadAtPosition

  ld    b, $01
  ld    c, $05
  ld    de, TILES_PER_LINE * $8 + $7
  call  LoadAtPosition

  jp    .reset_switch_timer


.switch_to_1
  ld    a, 1
  ld    [switch], a

  ld    b, $01
  ld    c, $05
  ld    de, TILES_PER_LINE * $5 + $7
  call  LoadAtPosition

  ld    b, $03
  ld    c, $07
  ld    de, TILES_PER_LINE * $5 + $A
  call  LoadAtPosition

  ld    b, $01
  ld    c, $05
  ld    de, TILES_PER_LINE * $8 + $A
  call  LoadAtPosition

  ld    b, $03
  ld    c, $07
  ld    de, TILES_PER_LINE * $8 + $7
  call  LoadAtPosition


.reset_switch_timer
  ld    a, ANIMATION_CYCLE
  ld    [switch_timer], a


.end
	ld		a, 1
	ld		[vblank_flag], a
  reti



;----------------------------------------------------
; Graphics and Data
;----------------------------------------------------
  SECTION "Tiles", HOME

  INCLUDE "graphics.inc"



;----------------------------------------------------
; Internal ram
;----------------------------------------------------

SECTION	"RAM_Other_Variables",BSS[$C000]

; frame timing
vblank_flag:
ds		1		; set if a vblank occured since last pass through game loop

switch_timer:
ds    1

switch:
ds    1

