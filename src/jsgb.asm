; system includes
INCLUDE	"lib/hardware.inc"


;****************************************************************************************************************************************************
;*	definitions
;****************************************************************************************************************************************************

; $ff80 to $fffe is 128 bytes of internal RAM
STACK_TOP				equ		$fffe		; put the stack here


TILES_PER_LINE  equ  $20
ANIMATION_CYCLE equ  $20

GRID_WIDTH   equ 8
GRID_HEIGHT  equ 4


GRID_START_X  equ 1
GRID_START_Y  equ 2

ACTIVE_JELLY_MASK equ %10000000
ACTIVE_JELLY_BIT  equ 7



jellysplash_tile_data_size EQU $D0
jellysplash_tile_count EQU $20


font_tile_data_size EQU $0300
font_tile_count EQU $30

;****************************************************************************************************************************************************
;*	Program Start
;****************************************************************************************************************************************************


	SECTION "Program Start",HOME[$0150]
Start::
	; init the stack pointer
	ld		sp, STACK_TOP

	; enable only vblank interrupts
	ld		a, IEF_VBLANK			; set vblank interrupt bit
	ldh		[rIE], a	; load it to the hardware register

	; standard inits
	sub		a	;	a = 0
	ldh		[rSTAT], a	; init status
	ldh		[rLCDC], a	; init LCD to everything off
	ldh		[rSCX], a	; background map will start at 0,0
	ldh		[rSCY], a
  ld    [input], a
  ld    [prev_input], a
  ld    [input_down], a
  ld    [input_up], a
  ld    [input_held], a
	ld		[vblank_flag], a
	ld		[activation_length], a

  ld    a, 1
  ld    [grid_changed], a

	; init the palettes
	call	InitPalettes  ; non-color palette

  ld    hl, palette1
  ld    b, $1
  call  WriteColorPalette

  ld    hl, palette2
  ld    b, $2
  call  WriteColorPalette


	; load the tiles
	ld		hl, _VRAM	; load the tiles to tiles bank 1
	ld		bc, jellysplash_tile_data
  ld    e, jellysplash_tile_count
	call	LoadTiles

  ; from the previous call to LoadTiles, hl is already set to the next free space after jelly tiles
	ld		bc, font_tile_data
  ld    e, $10  ; TODO can we somehow load all of the font?
	call	LoadTiles


  call  ClearMap

  ; load sprite
  ld    b, 0
  ld    c, 0
  ld    hl, cursor_sprite
  call  ShowSprite

  ; Init cursor
  ld    a, 0
  ld    [cursor_x], a
  ld    [cursor_y], a


  ; load level
  ld    hl, level_1
  call  LoadLevel

	; set display options
	ld		a, LCDCF_ON + LCDCF_BGON + LCDCF_WINOFF + LCDCF_OBJON + LCDCF_OBJ8 + LCDCF_BG8000 + LCDCF_BG9800 + LCDCF_WIN9C00
	ldh		[rLCDC],a

	; allow interrupts to start occuring
	ei


; main game loop
.Game_Loop
  halt  ; preserve energy!
  nop

	; don't do a frame update unless we have had a vblank
	ld		a, [vblank_flag]
	cp		0
	jp		z, .Game_Loop

	; reset vblank flag
	ld		a, 0
	ld		[vblank_flag], a


  call  UpdateInput
  call  ApplyInput

	jp		.Game_Loop




;***************************************************************
;* Subroutines
;***************************************************************

	SECTION "Support Routines",HOME

INP_SELECT_DIR equ %00100000
INP_SELECT_BTN equ %00010000
INPUT_MASK     equ %00001111

INP_BIT_DOWN   equ 7
INP_BIT_UP     equ 6
INP_BIT_LEFT   equ 5
INP_BIT_RIGHT  equ 4
INP_BIT_START  equ 3
INP_BIT_SELECT equ 2
INP_BIT_B      equ 1
INP_BIT_A      equ 0

;----------------------------------------------------
; Update input
;
; updates:
;    [input]
;    [prev_input]
;----------------------------------------------------
UpdateInput:

  ld   a, [input]
  ld   [prev_input], a

  ; select what type of data we want to read
  ld   a, INP_SELECT_DIR
  ld   b, 0

  ld   [rP1], a

  ; read state of directional pad (several times because of button bounce)
  ld   a, [rP1]
  ld   a, [rP1]
  ld   a, [rP1]
  ld   a, [rP1]

  cpl  ; if a button is pressed the bit is 0, so we invert it
  and  INPUT_MASK
  swap a
  ld   b, a

  ; read buttons
  ld   a, INP_SELECT_BTN

  ld   [rP1], a

  ; read state of directional pad (several times because of button bounce)
  ld   a, [rP1]
  ld   a, [rP1]
  ld   a, [rP1]
  ld   a, [rP1]

  cpl  ; if a button is pressed the bit is 0, so we invert it
  and  INPUT_MASK
  or   b  ; combine with directional input in high nibble
  ld   b, a  ; add back to b

  ; write to input
  ld   [input], a

  ; get changes in input
  ld   a, [prev_input]
  cpl
  and  b
  ld   [input_down], a

  ; get buttons being held down
  ld   a, [input]
  ld   b, a
  ld   a, [prev_input]
  and  b
  ld   a, b
  ld   [input_held], a

  ld   a, [prev_input]
  ld   b, a
  ld   a, [input]
  cpl
  and  b
  ld   [input_up], a

  ; reset joypad (not sure why this is needed)
  ld   a, $30
  ld   [rP1], a

.end
  ret



;----------------------------------------------------
; Apply input
;
; updates:
;    [cursor_x]
;    [cursor_y]
;----------------------------------------------------
ApplyInput:

  ; store previous cursor values
  ld   a, [cursor_x]
  ld   d, a
  ld   a, [cursor_y]
  ld   e, a

.process_up
  ld   a, [input_up]
  ld   b, a

.button_a_up
  bit  INP_BIT_A, b
  jp   z, .process_down

  ld   a, 0
  ld   [activation_length], a  ; reset activation chain

  ; TODO remove all jellies in activation chain
  call DeactivateAllJellies
  jp   .end


.process_down
  ld   a, [input_down]
  ld   b, a

.up
  bit  INP_BIT_UP, b
  jp   z, .down

  ld   a, [cursor_y]
  cp   0
  jp   z, .down

  sub  a, 1
  ld   [cursor_y], a
  ; intentional fall-through

.down
  bit  INP_BIT_DOWN, b
  jp   z, .left

  ld   a, [cursor_y]
  cp   GRID_HEIGHT - 1
  jp   z, .left

  add  a, 1
  ld   [cursor_y], a
  ; intentional fall-through

.left
  bit  INP_BIT_LEFT, b
  jp   z, .right

  ld   a, [cursor_x]
  cp   0
  jp   z, .right

  sub  a, 1
  ld   [cursor_x], a
  ; intentional fall-through

.right
  bit  INP_BIT_RIGHT, b
  jp   z, .button_a

  ld   a, [cursor_x]
  cp   GRID_WIDTH - 1
  jp   z, .button_a

  add  a, 1
  ld   [cursor_x], a
  ; intentional fall-through


; initial press of a button. Starts input mode
.button_a
  bit  INP_BIT_A, b
  jp   z, .handle_cursor_moved

  push de
  push bc
  ld   a, [cursor_x]
  ld   d, a
  ld   a, [cursor_y]
  ld   e, a
  call GetGridIndexForXAndY
  call ActivateJellyAtIndex
  call AddCursorPositionToActivationChain
  ld   a, 1
  ld   [grid_changed], a

  pop  bc
  pop  de



.handle_cursor_moved

  ; compare cursor to original values to determine if the cursor moved
  ld   a, [cursor_x]
  cp   d
  jp   nz, .cursor_did_move

  ld   a, [cursor_y]
  cp   e
  jp   nz, .cursor_did_move

  jp   .cursor_did_not_move

.cursor_did_move

  ; ## Check if we need to deactivate the jelly that is currently under the cursor
  ; TODO use activation chain to deactivate until the position we moved to

  ; takes d and e as parameters, which is the previous grid position here
  call GetGridCellForXAndY
  ld   a, [hl]
  bit  ACTIVE_JELLY_BIT, a  ; check if the jelly under the old cursor position was highlighted
  jp   z, .process_held  ; if not, go on

  ;push de
  ld   a, [cursor_x]
  ld   d, a
  ld   a, [cursor_y]
  ld   e, a
  call GetGridCellForXAndY
  ;pop  de   ; reset d and e to old cursor position (we need it later) TODO do we actually?

  ld   a, [hl]
  bit  ACTIVE_JELLY_BIT, a  ; check if the jelly under the new cursor position is also higlighted
  jp   z, .process_held  ; if not, go on

  ; deactivate the jelly under the previous cursor position
  ld   a, [cursor_x]
  ld   d, a
  ld   a, [cursor_y]
  ld   e, a
  call DeactivateJelliesUntilPosition

  ld   a, 1
  ld   [grid_changed], a

  jp   .end


.process_held
  ld   a, [input_held]
  ld   b, a

.process_a_held
  bit  INP_BIT_A, b
  jp   z, .end

  ld   a, [cursor_x]
  ld   d, a
  ld   a, [cursor_y]
  ld   e, a
  call ActivateJellyAtIndex
  ld   a, 1
  ld   [grid_changed], a
  call AddCursorPositionToActivationChain

.cursor_did_not_move
.end
  ret


;




;----------------------------------------------------
; in:
;    d = x position
;    e = y position
;
;----------------------------------------------------
AddCursorPositionToActivationChain:
  ld   a, [activation_length]
  ld   c, a
  ld   b, 0
  ld   hl, activation_chain
  add  hl, bc ; get pointer to current activation chain index

  push bc
  call GetGridIndexForXAndY ; d, e -> c
  ld   [hl], c
  pop  bc

  ld   a, c
  inc  a      ; increase activation length
  ld   [activation_length], a

  ret




;----------------------------------------------------
; in:
;    d = x position
;    e = y position
;----------------------------------------------------
DeactivateJelliesUntilPosition:

  call GetGridIndexForXAndY  ; d, e -> c
  ld   a, [activation_length]
  ld   b, a
  ld   d, 0 ; loop index
  ld   hl, activation_chain

.find_index_loop

  ; did we reach the end of the activation chain?
  ld   a, d
  cp   b
  jp   z, .failed_find_index_loop

  ; read and compare next index
  ld   a, [hli]
  cp   c
  jp   z, .end_find_index_loop

  inc  d
  jp   .find_index_loop

.failed_find_index_loop
  stop ; was `ret` before, but should never happen, so we panic

.end_find_index_loop
  ; our index is now in d and the current activation_chain position is in hl

  ld   a, [activation_length]
  sub  a, d
  ld   d, a
  ld   [activation_length], a

.loop

  ld   a, d
  cp   0
  jp   z, .end

  ld   a, [hli]
  ld   c, a
  push hl
  push de
  call DeactivateJellyAtIndex
  pop  de
  pop  hl

  dec  d
  jp   .loop


.end
  ret

;   call GetGridIndexForXAndY  ; d, e -> c
;   ld   d, c
; 
;   ; loop and reset until we find the index
; .loop
; 
;   ; did we exhaust the entire chain? (This would be an error, as we should always go back to a specified element)
;   ld   a, [activation_length]
;   cp   0
;   jp   z, .end
; 
;   ; get index in activation chain
;   dec  a
;   ld   c, a
;   ld   hl, activation_chain
;   ld   b, 0
;   add  hl, bc
; 
;   ; is this our final grid cell?
;   ld   a, [hl]
;   cp   d
;   jp   z, .end  ; we found our original x and y position, go to end
; 
;   ; deactivate this jelly
;   ld   c, a
;   call DeactivateJellyAtIndex
; 
;   ; decrement activation length
;   ld   a, [activation_length]
;   dec  a
;   ld   [activation_length], a
; 
;   jp   .loop
; 
; .end
;   ; signal that grid was changed
;   ; TODO turn this into macro or subroutine!!
;   ld    a, 1
;   ld    [grid_changed], a

  ret



;----------------------------------------------------
; in:
;    d = x position
;    e = y position
;
;----------------------------------------------------
SwitchJelly:

  ; highlight jelly at current position
  call GetGridCellForXAndY
  ld   a, [hl]

  ; check if jelly is active or inactive
  bit  ACTIVE_JELLY_BIT, a
  jp   nz, .reset_jelly

  set  ACTIVE_JELLY_BIT, a
  jp   .update_jelly

.reset_jelly
  res  ACTIVE_JELLY_BIT, a

.update_jelly
  ld   [hl], a

  ld   a, 1
  ld   [grid_changed], a

  ret



;----------------------------------------------------
; in:
;    c = grid index
;
;----------------------------------------------------
DeactivateJellyAtIndex:
  call GetGridCellForIndex
  ld   a, [hl]
  res  ACTIVE_JELLY_BIT, a
  ld   [hl], a

  ret

ActivateJellyAtIndex:
  call GetGridCellForIndex
  ld   a, [hl]
  set  ACTIVE_JELLY_BIT, a
  ld   [hl], a

  ret

;----------------------------------------------------
; Deactivate all jellies
;----------------------------------------------------
DeactivateAllJellies:
  ld   hl, grid

  ld   e, 0
  ld   c, GRID_WIDTH * GRID_HEIGHT

.loop
  ld   a, [hl]
  res  ACTIVE_JELLY_BIT, a
  ld   [hli], a

  inc  e
  ld   a, c
  cp   e
  jp   nz, .loop

  ld   a, 1
  ld   [grid_changed], a

  ret



;----------------------------------------------------
; in:
;    d = x position
;    e = y position
;
; out:
;    hl = pointer to grid cell
;----------------------------------------------------
GetGridCellForXAndY:
  call GetGridIndexForXAndY  ; d, e -> c
  call GetGridCellForIndex
  ret

;----------------------------------------------------
; in:
;    c = index
;
; out:
;    hl = pointer to grid cell
;----------------------------------------------------
GetGridCellForIndex:
  ld   hl, grid
  push bc
  ld   b, 0
  add  hl, bc
  pop  bc

  ret


;----------------------------------------------------
; in:
;    d = x position
;    e = y position
;
; out:
;    c = offset into grid from 0
;----------------------------------------------------
GetGridIndexForXAndY:

  ld   a, e  ; load y
  sla  a  ; multiply by 8, our grid width
  sla  a
  sla  a
  ld   c, a

  ld   a, d  ; load x
  add  a, c
  ld   c, a

 ret

;----------------------------------------------------
; Wait while LCD is busy
;----------------------------------------------------
WaitBusy: MACRO
.loop\@
	; only write during
	ldh		a, [rSTAT]	; get the status
	bit		1, a		  	; don't write during sprite and transfer modes
	jr		nz, .loop\@
ENDM


;----------------------------------------------------
; load the tiles from ROM into the tile video memory
;
; in:	bc = address of tile data to load
;      e = num tiles to load
;     hl = start of VRAM to load into
;----------------------------------------------------
LoadTiles:

	ld		d, $10  ; 16 bytes per tile
	; ld		e, 12  ; number of tiles to load

.loop
  ; WaitBusy
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
  ; WaitBusy

  ld    a, 0
  ld    [hli], a

  dec   d
  jr    nz, .loop
  ld    d, $20

  dec   e
  jr    nz, .loop

  ret


;----------------------------------------------------
; Load tile at position
;
; in:	hl = jelly address
;     de = tile pos
;----------------------------------------------------
MoveNextBCToHL: MACRO
  WaitBusy
  inc   bc
  ld    a, [bc]
  ld    [hli], a
ENDM

LoadTileAtPosition:
  ld    b, h
  ld    c, l  ; move to bc, since we need hl for other stuff

	ld		hl, _SCRN0	; load the map to map bank 0
  add   hl, de  ; move to tile position

  push  hl

  ; set palette
  ld    a, 1        ; switch to VRAM bank 1
  ldh   [rVBK], a

  WaitBusy
  ld    a, [bc]   ; load palette
  ld    [hli], a  ; set color for upper left
  ld    [hli], a  ; upper right
  ld    de, TILES_PER_LINE - 2
  add   hl, de    ; Go one line down
  ld    [hli], a  ; set color for lower left
  ld    [hl], a   ; lower right

  WaitBusy
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
; init DMG palettes to basic
;----------------------------------------------------
InitPalettes:
	ld		a, %11100100	; set palette colors

	; load it to all the palettes
	ldh		[rBGP], a
	ldh		[rOBP0], a
	ldh		[rOBP1], a

	ret

;----------------------------------------------------
; Write CGB palette to both bg and obj memory
;
; in: hl = address of palette
;     b  = palette number
;----------------------------------------------------
WriteColorPalette:

  ; set up b for control register (palette number starts at bit 3)
  ld    a, b
  and   7
  sla   a    ; move palette number to bit 3 by left-shifting 3 times
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

AddM: MACRO
  ld   a, \1
  add  a, \2
  ld   \1, a
ENDM

SubM: MACRO
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

  ; WaitBusy

  ld    hl, _OAMRAM  ; TODO allow for an offset by index here
  call  ShowSingleSprite
  inc   de
  AddM  b, 8
  call  ShowSingleSprite
  inc   de
  SubM  b, 8
  AddM  c, 8
  call  ShowSingleSprite
  inc   de
  AddM  b, 8
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



;----------------------------------------------------
; Update position of cursor
;
;----------------------------------------------------
UpdateCursorSpritePosition:


  ld    a, [cursor_x]
  add   a, GRID_START_X

  ; Multiply by 16
  sla   a
  sla   a
  sla   a
  sla   a
  add   a, 8
  ld    b, a

  ld    a, [cursor_y]
  add   a, GRID_START_Y
  sla   a
  sla   a
  sla   a
  sla   a
  add   a, 16
  ld    c, a

  ld    hl, _OAMRAM
  call  WriteSpritePos
  AddM  b, 8
  call  WriteSpritePos
  SubM  b, 8
  AddM  c, 8
  call  WriteSpritePos
  AddM  b, 8
  call  WriteSpritePos

  ret

WriteSpritePos:
  WaitBusy
  ld    a, c
  ld    [hli], a    ; y coord
  ld    a, b
  ld    [hli], a    ; x coord
  inc   hl
  inc   hl
  ret

;----------------------------------------------------
; Load level to ram
;
; in
;    hl = level address
;----------------------------------------------------

LoadLevel:

  ld   de, grid

  ld   b, 0
  ld   c, GRID_WIDTH * GRID_HEIGHT

.loop
  ld   a, [hl]
  ld   [de], a

  inc  de
  inc  hl

  inc  b
  ld   a, c
  cp   b
  jp   nz, .loop

  ret



;----------------------------------------------------
; Show grd
;
;----------------------------------------------------

ShowGrid:

  ld    hl, grid
  ld    de, TILES_PER_LINE * (GRID_START_Y * 2) + (GRID_START_X * 2)

  ld    c, 0   ; y

.loop_height
  ld    b, 0   ; x

.loop_width
  ld    a, [hli]
  push  hl
  push  de
  push  bc

  cp    1
  jp    z, .show_1
  cp    1 + ACTIVE_JELLY_MASK
  jp    z, .show_1_active
  cp    2
  jp    z, .show_2
  cp    2 + ACTIVE_JELLY_MASK
  jp    z, .show_2_active
  jp    .next


.show_1
  ld    hl, inactive_jelly_1
  call  LoadTileAtPosition
  jp    .next

.show_2
  ld    hl, inactive_jelly_2
  call  LoadTileAtPosition
  jp    .next

.show_1_active
  ld    hl, active_jelly_1
  call  LoadTileAtPosition
  jp    .next

.show_2_active
  ld    hl, active_jelly_2
  call  LoadTileAtPosition
  jp    .next


.next
  pop   bc
  pop   de
  pop   hl

  inc   de
  inc   de

  ; next b
  inc  b
  ld   a, b
  cp   GRID_WIDTH
  jp   nz, .loop_width

  ; move de to next row
  push hl
  ld   h, d
  ld   l, e
  ld   de, TILES_PER_LINE + (TILES_PER_LINE - GRID_WIDTH * 2)
  add  hl, de
  ld   d, h
  ld   e, l
  pop  hl

  ; next c
  inc  c
  ld   a, c
  cp   GRID_HEIGHT
  jp   nz, .loop_height

.end
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
  di
  PushRegs

  call  UpdateCursorSpritePosition

  ; was the grid changed?
  ld    a, [grid_changed]
  cp    1
  jp    nz, .end

  call  ShowGrid
  ld    a, 0
  ld    [grid_changed], a

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

palette2:
dw  $ffff
dw  $e7ff
dw  $823c
dw  $1234



; This section describes each graphic. First byte is the palette, next 4 bytes
; are the relative tile addresses for the active frame (eyes open) and next 4
; bytes again are the states for inactive frame

SPR_DATA_LEN equ 5
SPR_TILE_DATA_OFFSET equ 1


SECTION "Graphics", HOME

active_jelly_1:
db 1 ; palette
db 1, 2, 5, 6

inactive_jelly_1:
db 1 ; palette
db 3, 4, 7, 8


active_jelly_2:
db 2 ; palette
db 1, 2, 5, 6

inactive_jelly_2:
db 2 ; palette
db 3, 4, 7, 8

cursor_sprite:
db 1 ; palette
db 9, 10, 11, 12


SECTION "Levels", HOME

level_1:
db 2, 1, 2, 1, 2, 1, 1, 1
db 1, 2, 1, 2, 1, 2, 2, 2
db 1, 2, 1, 2, 1, 1, 1, 1
db 1, 1, 2, 1, 1, 2, 1, 2


;----------------------------------------------------
; Internal ram
;----------------------------------------------------

SECTION	"RAM_Other_Variables",BSS[$C000]

var1:
ds 1

grid_changed:
ds 1

cursor_x:
ds 1

cursor_y:
ds 1


input:
ds 1

prev_input:
ds 1

input_down: ; which buttons just went down
ds 1

input_up: ; which buttons just went up
ds 1

input_held: ; which buttons are being held down
ds 1


; frame timing
vblank_flag:
ds 1		; set if a vblank occured since last pass through game loop

grid:
ds    GRID_WIDTH * GRID_HEIGHT

activation_chain:
ds    GRID_WIDTH * GRID_HEIGHT

activation_length:
ds 1

