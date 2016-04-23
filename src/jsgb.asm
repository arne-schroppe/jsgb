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
  ld    hl, activeJelly1
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
; in:	bc = address of tile data to load
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
; in:	hl = jelly address
;     de = tile pos
;----------------------------------------------------

MoveNextBCToHL: MACRO
  inc   bc
  ld    a, [bc]
  ld    [hli], a
ENDM

LoadTileAtPosition:
  ld    b, h
  ld    c, l  ; move to bc, since we need hl for other stuff

	ld		hl, _SCRN0	; load the map to map bank 0
  add   hl, de  ; move to tile position

  WaitBusy

  push  hl

  ; set palette
  ld    a, 1        ; switch to VRAM bank 1
  ldh   [rVBK], a

  ld    a, [bc]   ; load palette
  ld    [hli], a  ; set color for upper left
  ld    [hli], a  ; upper right
  ld    de, TILES_PER_LINE - 2
  add   hl, de    ; Go one line down
  ld    [hli], a  ; set color for lower left
  ld    [hl], a   ; lower right

  ld    a, 0      ; switch back to VRAM bank 0
  ldh   [rVBK], a


  pop   hl   ; go back to start position


  ; set tile data

  ; TODO The first inc inside the macro moves the bc pointer from palette data to
  ; tile data. This only works because the data is exactly one byte long. But this
  ; is more or less secret knowledge and so this is fairly fragile.
  MoveNextBCToHL
  MoveNextBCToHL
  ld    de, TILES_PER_LINE - 2
  add   hl, de  ; Go one line down
  MoveNextBCToHL
  MoveNextBCToHL

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
; Write color palette to both bg and obj memory
;
; in: hl = address of palette
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

  ld	hl, rOCPS	      ; also write object palette
  ld  a, b
  ld  [hli], a        ; set control register and move hl to data register
  ld  a, e
  ld  [hl], a         ; send data

  pop   hl

  inc   b     ; write next color on next iteration

  ; loop handling
  ld    a, d
  cp    7     ; did we write all color parts?
  jp    z, .end
  inc   d
  jp    .loop

.end
  ret


;----------------------------------------------------
; Show a sprite at specific position
;
; in: b  = x coord
;     c  = y coord
;     hl = jelly address
;
;----------------------------------------------------

addM: MACRO
  ld   a, \1
  add  a, \2
  ld   \1, a
ENDM

subM: MACRO
  ld   a, \1
  sub  a, \2
  ld   \1, a
ENDM


ShowSprite:
  ld    d, h   ; TODO create a macro to move hl to another reg
  ld    e, l

  ld    a, [de]    ; load color
  ld    [var1], a
  inc   de         ; move to tile data

  ld    hl, _OAMRAM  ; TODO allow for an offset by index here
  call  ShowSingleSprite
  inc   de
  addM  b, 8
  call  ShowSingleSprite
  inc   de
  subM  b, 8
  addM  c, 8
  call  ShowSingleSprite
  inc   de
  addM  b, 8
  call  ShowSingleSprite

  ret


ShowSingleSprite:
  ld  a, c
  ld  [hli], a ; y coord

  ld  a, b
  ld  [hli], a ; x coord

  ld  a, [de]
  ld  [hli], a ; tile

  ld  a, [var1]
  ld  [hli], a  ; attributes

  ret



UpdateSpritePosition:

  ld    hl, _OAMRAM
  ld    a, [hl]

  cp    $80
  jp    nz, .update

  ld    a, 0  ; reset to 0

.update

  inc   a

  ld    b, a
  ld    c, a
  call  WriteSpritePos
  addM  b, 8
  call  WriteSpritePos
  subM  b, 8
  addM  c, 8
  call  WriteSpritePos
  addM  b, 8
  call  WriteSpritePos

  ret

WriteSpritePos:
  ld    a, c
  ld    [hli], a    ; y coord
  ld    a, b
  ld    [hli], a    ; x coord
  inc   hl
  inc   hl
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

  ld    hl, activeJelly1
  ld    de, TILES_PER_LINE * $5 + $7
  call  LoadTileAtPosition

  ld    hl, inactiveJelly1
  ld    de, TILES_PER_LINE * $5 + $A
  call  LoadTileAtPosition

  ld    hl, activeJelly1
  ld    de, TILES_PER_LINE * $8 + $A
  call  LoadTileAtPosition

  ld    hl, inactiveJelly1
  ld    de, TILES_PER_LINE * $8 + $7
  call  LoadTileAtPosition

  jr    .reset_switch_timer


.switch_to_1
  ld    a, 1
  ld    [switch], a

  ld    hl, inactiveJelly1
  ld    de, TILES_PER_LINE * $5 + $7
  call  LoadTileAtPosition

  ld    hl, activeJelly1
  ld    de, TILES_PER_LINE * $5 + $A
  call  LoadTileAtPosition

  ld    hl, inactiveJelly1
  ld    de, TILES_PER_LINE * $8 + $A
  call  LoadTileAtPosition

  ld    hl, activeJelly1
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
dw  $ffff
dw  $ffe7
dw  $3c82
dw  $0000




; This section describes each graphic. First byte is the palette, next 4 bytes
; are the relative tile addresses for the active frame (eyes open) and next 4
; bytes again are the states for inactive frame

SPR_DATA_LEN equ 5
SPR_TILE_DATA_OFFSET equ 1


SECTION "Graphics", HOME

activeJelly1:
db 1 ; palette
db 1, 2, 5, 6


inactiveJelly1:
db 1 ; palette
db 3, 4, 7, 8



;----------------------------------------------------
; Internal ram
;----------------------------------------------------

SECTION	"RAM_Other_Variables",BSS[$C000]

var1:
ds    1

; frame timing
vblank_flag:
ds		1		; set if a vblank occured since last pass through game loop

switch_timer:
ds    1

switch:
ds    1



