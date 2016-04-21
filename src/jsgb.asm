; system includes
INCLUDE	"lib/hardware.inc"


;****************************************************************************************************************************************************
;*	definitions
;****************************************************************************************************************************************************

; $ff80 to $fffe is 128 bytes of internal RAM
STACK_TOP				equ		$fff4		; put the stack here


TILES_PER_LINE  equ  $20
ANIMATION_CYCLE equ $20

;****************************************************************************************************************************************************
;*	Program Start
;****************************************************************************************************************************************************


	SECTION "Program Start",HOME[$0150]
Start::
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

	; init the palettes
	call	InitPalettes  ; non-color palette

  ld    hl, palette1
  ld    b, $1
  call  WriteColorPalette

	; load the tiles
	ld		bc, jellysplash_tile_data
	call	LoadTiles

  call  ClearMap

  ld    b, $20
  ld    c, $20
  ld    d, $1
  ld    e, $0
  call  ShowSprite


	; set display options
	ld		a, LCDCF_ON + LCDCF_BGON + LCDCF_WINOFF + LCDCF_OBJON + LCDCF_OBJ8 + LCDCF_BG8000 + LCDCF_BG9800 + LCDCF_WIN9C00
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
; Wait while LCD is busy
;----------------------------------------------------
WaitBusy: MACRO
.loop\@
	; only write during
	ldh		a, [rSTAT]	; get the status
	and		STATF_BUSY			; don't write during sprite and transfer modes
	jr		nz, .loop\@
ENDM


;----------------------------------------------------
; load the tiles from ROM into the tile video memory
;
; IN:	bc = address of tile data to load
;----------------------------------------------------
LoadTiles:
	ld		hl, _VRAM	; load the tiles to tiles bank 1

	ld		de, 4 * 16
	ld		d, $10  ; 16 bytes per tile
	ld		e, 12  ; number of tiles to load

.loop
  WaitBusy
	ld		a, [bc]		; get the next value from the source
	ld	  [hli], a	; load the value to the destination, incrementing dest. ptr
	inc		bc			; increment the source ptr

	; now loop de times
	dec		d
	jr		nz, .loop
	dec		e
	jr		nz, .loop

	ret



;----------------------------------------------------
; Clear background map
;----------------------------------------------------
ClearMap:
  ld    hl, _SCRN0

  ld    d, $20
  ld    e, $20

.loop
  WaitBusy

  ld    a, 0
  ld    [hli], a

  dec   d
  jr    nz, .loop
  ld    d, $20

  dec   e
  jr    nz, .loop

  ret


;----------------------------------------------------
; load tile at position
;
; IN:	b = Tile address upper part
;     c = Tile address lower part
;     de = tile pos
;----------------------------------------------------
LoadTileAtPosition:
	ld		hl, _SCRN0	; load the map to map bank 0
  add   hl, de  ; move to tile position

  WaitBusy

  push  hl


  ; set palette
  ld    a, 1        ; switch to VRAM bank 1
  ldh   [rVBK], a

  ld    a, %00000001   ; which palette to use
  ld    [hli], a
  ld    [hli], a
  ld    de, TILES_PER_LINE - 2
  add   hl, de  ; Go one line down
  ld    [hli], a
  ld    [hl], a

  ld    a, 0        ; switch back to VRAM bank 0
  ldh   [rVBK], a


  pop   hl   ; go back to start position


  ; set tile data
  ld    a, b
  ld    [hli], a
  inc   a
  ld    [hli], a

  ld    de, TILES_PER_LINE - 2
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
; Store color palette
;
; IN: hl = address of palette
;     b  = palette number
;----------------------------------------------------
WriteColorPalette:

  ; set up b for control register (palette number starts at bit 3)
  ld    a, b
  and   7
  sla   a
  sla   a
  sla   a
  ld    b, a

  ; init loop counter
  ld    d, 0

.loop
  ld    a, [hli]
  ld    e, a

  push  hl

  ; write actual color data
  ld	hl, rBCPS	      ; set up a pointer to the BCPS
  ld  a, b
  ld  [hli], a        ; set control register and move hl to data register
  ld  a, e
  ld  [hl], a         ; send data

  pop   hl

  inc   b     ; write next color on next iteration

  ld    a, d  ; loop
  cp    7     ; did we write all color parts?
  jp    z, .end
  inc   d
  jp    .loop

.end
  ret


;----------------------------------------------------
; Show a sprite at specific position
;
; in: b = x coord
;     c = y coord
;     d = tile
;     e = palette
;
; TODO later in the game, a tile will always be displayed
;      with the same color, so we should encode that 
;      relationship somewhere.
;----------------------------------------------------

ShowSprite:

  ld  hl, _OAMRAM
  ld  a, c
  ld  [hli], a ; y coord

  ld  a, b
  ld  [hli], a ; x coord

  ld  a, d
  ld  [hli], a ; tile

  ld  a, e     ; Note: All other attributes except palette are 0 for now
  ld  [hl], a  ; attributes

  ret


UpdateSpritePosition:

  ld  hl, _OAMRAM
  ld  a, [hl]

  cp  $80
  jp  nz, .update

  ld  a, 0  ; reset to 0

.update

  inc a
  ld  [hli], a
  ld  [hl], a

  ret


;----------------------------------------------------
; Push and pop registers
;----------------------------------------------------
PushRegs: MACRO
  push af
  push bc
  push de
  push hl
ENDM

PopRegs: MACRO
  pop hl
  pop de
  pop bc
  pop af
ENDM


;----------------------------------------------------
; V-Blank interrupt handler
;----------------------------------------------------
VBlankHandler::
  PushRegs

  call  UpdateSpritePosition


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
  call  LoadTileAtPosition

  ld    b, $01
  ld    c, $05
  ld    de, TILES_PER_LINE * $5 + $A
  call  LoadTileAtPosition

  ld    b, $03
  ld    c, $07
  ld    de, TILES_PER_LINE * $8 + $A
  call  LoadTileAtPosition

  ld    b, $01
  ld    c, $05
  ld    de, TILES_PER_LINE * $8 + $7
  call  LoadTileAtPosition

  jr    .reset_switch_timer


.switch_to_1
  ld    a, 1
  ld    [switch], a

  ld    b, $01
  ld    c, $05
  ld    de, TILES_PER_LINE * $5 + $7
  call  LoadTileAtPosition

  ld    b, $03
  ld    c, $07
  ld    de, TILES_PER_LINE * $5 + $A
  call  LoadTileAtPosition

  ld    b, $01
  ld    c, $05
  ld    de, TILES_PER_LINE * $8 + $A
  call  LoadTileAtPosition

  ld    b, $03
  ld    c, $07
  ld    de, TILES_PER_LINE * $8 + $7
  call  LoadTileAtPosition


.reset_switch_timer
  ld    a, ANIMATION_CYCLE
  ld    [switch_timer], a


.end
	ld		a, 1
	ld		[vblank_flag], a

  PopRegs
  reti



;----------------------------------------------------
; Graphics and Data
;----------------------------------------------------
  SECTION "Tiles", HOME

  INCLUDE "graphics.inc"


  SECTION "Palettes", HOME

palette1:
dw $ffff
dw $ffe7
dw $3c82
dw $0000


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

