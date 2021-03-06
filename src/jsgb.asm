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
GRID_HEIGHT  equ 7


GRID_START_X  equ 1
GRID_START_Y  equ 1

ACTIVE_JELLY_MASK equ %10000000
ACTIVE_JELLY_BIT  equ 7

MIN_JELLIES_FOR_MOVE  equ 3


sprites_tile_map_size EQU $40
sprites_tile_map_width EQU $08
sprites_tile_map_height EQU $08

sprites_tile_data_size EQU $0400
sprites_tile_count EQU $40

_B equ 1
_Y equ 2
_P equ 3
_R equ 4
SB equ 5

;****************************************************************************************************************************************************
;*	Program Start
;****************************************************************************************************************************************************


	SECTION "Program Start",ROM0[$0150]
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

  ld    hl, palette3
  ld    b, $3
  call  WriteColorPalette

  ld    hl, palette4
  ld    b, $4
  call  WriteColorPalette

	; load the tiles
	ld		hl, _VRAM	; load the tiles to tiles bank 1
	ld		bc, sprites_tile_data
  ld    e, sprites_tile_count
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

	SECTION "Support Routines",ROM0

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

  ; read state of buttons (several times because of button bounce)
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


.process_buttons_up
  ld   a, [input_up]
  ld   b, a

.button_a_up
  bit  INP_BIT_A, b
  jp   z, .process_buttons_down

  call CheckPopJellies

  jp   .end


.process_buttons_down

  ; check if there was any new press on the direction pad
  ld   a, [input_down]
  and  %11110000
  cp   0
  jp   z, .check_can_move

  ; combine down and held buttons (for easier diagonal movement)
  ld   a, [input_down]
  ld   b, a
  ld   a, [input_held]
  or   b
  ld   b, a
  call CheckDirectionPad


.check_can_move

  ; do we need to check? (i.e. are we in connect-mode?)
  ld   a, [input_held]
  bit  INP_BIT_A, a
  jp   z, .button_a

  ; get color under previous cursor position
  push bc
  call GetGridCellForXAndY ; d, e -> hl
  ld   a, [hl]
  res  ACTIVE_JELLY_BIT, a ; ignore whether jelly is active
  ld   c, a  ; store current color in c

  push de ; store previous cursor position

  ; get color under new cursor position
  ld   a, [cursor_x]
  ld   d, a
  ld   a, [cursor_y]
  ld   e, a

  push bc
  call GetGridCellForXAndY ; d, e -> hl
  pop  bc

  ld   a, [hl]
  res  ACTIVE_JELLY_BIT, a ; ignore whether jelly is active

  ; check if color at new position is same as c
  cp   c

  pop  de
  pop  bc
  jp   z, .same_color

  ; reset to previous position
  ld   a, d
  ld   [cursor_x], a
  ld   a, e
  ld   [cursor_y], a
  jp   .button_a

.same_color
  ; fall through

; initial press of a button. Starts input mode
.button_a
  ld   a, [input_down]
  ld   b, a
  bit  INP_BIT_A, b
  jp   z, .button_select

  push de
  push bc
  ld   a, [cursor_x]
  ld   d, a
  ld   a, [cursor_y]
  ld   e, a
  call GetGridCellForXAndY ; d, e -> hl

  call ActivateJellyAtIndex ; c -> void
  call AddCursorPositionToActivationChain ; d, e -> void
  ld   a, 1
  ld   [grid_changed], a

.button_a_done
  pop  bc
  pop  de
  jp   .handle_cursor_moved


.button_select
  bit  INP_BIT_SELECT, b
  jp   z, .handle_cursor_moved

  ld    hl, level_1
  call  LoadLevel

  ld   a, 1
  ld   [grid_changed], a

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

  ; takes d and e as parameters, which is the previous grid position here
  call GetGridCellForXAndY
  ld   a, [hl]
  bit  ACTIVE_JELLY_BIT, a  ; check if the jelly under the old cursor position was highlighted
  jp   z, .process_held  ; if not, go on

  ld   a, [cursor_x]
  ld   d, a
  ld   a, [cursor_y]
  ld   e, a
  call GetGridCellForXAndY

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



;----------------------------------------------------
; in:
;   b = input state
;----------------------------------------------------
CheckDirectionPad:

.up
  bit  INP_BIT_UP, b
  jp   z, .down

  ld   a, [cursor_y]
  cp   0
  jp   z, .down

  dec  a
  ld   [cursor_y], a
  ; intentional fall-through

.down
  bit  INP_BIT_DOWN, b
  jp   z, .left

  ld   a, [cursor_y]
  cp   GRID_HEIGHT - 1
  jp   z, .left

  inc  a
  ld   [cursor_y], a
  ; intentional fall-through

.left
  bit  INP_BIT_LEFT, b
  jp   z, .right

  ld   a, [cursor_x]
  cp   0
  jp   z, .right

  dec  a
  ld   [cursor_x], a
  ; intentional fall-through

.right
  bit  INP_BIT_RIGHT, b
  jp   z, .done

  ld   a, [cursor_x]
  cp   GRID_WIDTH - 1
  jp   z, .done

  inc  a
  ld   [cursor_x], a

.done
  ret


;----------------------------------------------------
; in:
;   <nothing>
;----------------------------------------------------
CheckPopJellies:

  ; if activation_length < 3, just reset, otherwise pop all active jellies
  ld  a, [activation_length]
  cp  MIN_JELLIES_FOR_MOVE
  jp  c, .not_enough ; a < MIN_JELLIES_FOR_MOVE

  call RemoveActiveJellies
  call PullDownAllJellies
  ret

.not_enough
  call DeactivateAllJellies
  ld   a, 0
  ld   [activation_length], a  ; reset activation chain
  ret



;----------------------------------------------------
; in:
;   <nothing>
;----------------------------------------------------
PullDownAllJellies:
  ld   e, GRID_HEIGHT - 1

.loop_y
  ld   d, 0

.loop_x
    push de
    call PullDownJellyIfNeeded
    pop  de

    inc  d
    ld   a, d
    cp   GRID_WIDTH
    jp   nz, .loop_x

  ld   a, e
  cp   0
  jp   z, .done

  dec  e
  jp   .loop_y

.done
  ret


;----------------------------------------------------
; in:
;   d = x coordinate
;   e = y coordinate
;----------------------------------------------------
PullDownJellyIfNeeded:
  ld   a, 0
  cp   e
  jp   z, .get_cell_and_spawn_new

  call GetGridCellForXAndY
  ld   a, [hl]

  ; check if cell is empty
  cp   0
  ret  nz ; return if not

  push hl

.loop
  ; check if we've reached the top of the board
  ld   a, 0
  cp   e
  jp   z, .spawn_new_jelly

  dec  e ; go up

  ; loop again if we haven't found a non-empty cell
  call GetGridCellForXAndY
  ld   a, [hl]
  cp   0
  jp   z, .loop

  ld   a, [hl]
  ld   b, a
  ld   [hl], 0 ; empty this cell
  jp   .next

.get_cell_and_spawn_new
  call GetGridCellForXAndY

  ; check if cell is empty
  ld   a, [hl]
  cp   0
  ret  nz ; return if not (we don't overwrite existing jellies)

  push hl ; needed for consistency with other code branch
  ; fall-through

.spawn_new_jelly
  call GetRandomNumber
  and  %00000011 ; limit to number of jellies
  add  1
  ld   b, a


.next
  pop  hl
  ld   a, b
  ld   [hl], a ; move content from upper cell into this position

  ret


;----------------------------------------------------
; in:
;   <nothing>
; out:
;   a = random number
;
; Source: http://www.z80.info/pseudo-random.txt
;----------------------------------------------------
GetRandomNumber:
 ld a, [random_number_seed]
 ld b, a

 rrca ; multiply by 32
 rrca
 rrca
 xor  $1f

 add a, b
 sbc a, 255 ; carry

 ld [random_number_seed], a
 ret

;----------------------------------------------------
; in:
;   <nothing>
;----------------------------------------------------
RemoveActiveJellies:

  ld   a, [activation_length]
  ld   d, a
  ld   e, 0
  ld   hl, activation_chain

.loop
  ld   a, [hli]
  ld   c, a

  push de
  push hl
  call RemoveJellyAtIndex ; args: c
  pop  hl
  pop  de

  ; check our index (e) against the max (d)
  inc  e
  ld   a, d
  cp   e
  jp   nz, .loop

.end
  ld   a, 1
  ld   [grid_changed], a

  ld   a, 0
  ld   [activation_length], a  ; reset activation chain

  ret



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
  inc  d ; add one for cursor position

  ; our index is now in d and the current activation_chain position is in hl

  ld   b, d ; store our new activation length

  ; store remaining length in d
  ld   a, [activation_length]
  sub  a, d
  ld   d, a

  ; store new activation_length
  ld   a, b
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

RemoveJellyAtIndex:
  call GetGridCellForIndex
  ld   a, 0
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
;     c = offset into grid from 0
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

  cp    SB
  jp    z, .show_hole
  cp    _B
  jp    z, .show_1
  cp    _B + ACTIVE_JELLY_MASK
  jp    z, .show_1_active
  cp    _Y
  jp    z, .show_2
  cp    _Y + ACTIVE_JELLY_MASK
  jp    z, .show_2_active
  cp    _P
  jp    z, .show_3
  cp    _P + ACTIVE_JELLY_MASK
  jp    z, .show_3_active
  cp    _R
  jp    z, .show_4
  cp    _R + ACTIVE_JELLY_MASK
  jp    z, .show_4_active
  jp    .hide


.show_1
  ld    hl, inactive_jelly_1
  call  LoadTileAtPosition
  jp    .next

.show_2
  ld    hl, inactive_jelly_2
  call  LoadTileAtPosition
  jp    .next

.show_3
  ld    hl, inactive_jelly_3
  call  LoadTileAtPosition
  jp    .next

.show_4
  ld    hl, inactive_jelly_4
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

.show_3_active
  ld    hl, active_jelly_3
  call  LoadTileAtPosition
  jp    .next

.show_4_active
  ld    hl, active_jelly_4
  call  LoadTileAtPosition
  jp    .next

.hide
  ld    hl, blank_jelly
  call  LoadTileAtPosition
  jp    .next

.show_hole
  ld    hl, hole
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
SECTION "Tiles", ROM0

INCLUDE "graphics.inc"


SECTION "Palettes", ROM0

palette1:
dw  $ffff
dw  $ffe7
dw  $3c82
dw  $0000

palette2:
dw  $ffff
dw  $03df
dw  $823c
dw  $1234

palette3:
dw  $ffff
dw  $7CDC
dw  $2849
dw  $0000

; Reds
palette4:
dw  $ffff
dw  $00ff ; light
dw  $0033 ; medium dark
dw  $0047 ; dark

; This section describes each graphic. First byte is the palette, next 4 bytes
; are the relative tile addresses for the active frame (eyes open) and next 4
; bytes again are the states for inactive frame

SPR_DATA_LEN equ 5
SPR_TILE_DATA_OFFSET equ 1


SECTION "Graphics", ROM0

blank_jelly:
db 1 ; palette
db 20, 20, 20, 20


active_jelly_1:
db 1 ; palette
db 2, 3, 8, 9

inactive_jelly_1:
db 1 ; palette
db 2, 3, 10, 11

active_jelly_2:
db 2 ; palette
db 4, 5, 8, 9

inactive_jelly_2:
db 2 ; palette
db 4, 5, 10, 11

active_jelly_3:
db 3 ; palette
db 12, 13, 8, 9

inactive_jelly_3:
db 3 ; palette
db 12, 13, 10, 11

active_jelly_4:
db 4 ; palette
db 0, 1, 8, 9

inactive_jelly_4:
db 4 ; palette
db 0, 1, 10, 11

cursor_sprite:
db 1 ; palette
db 16, 17, 24, 25

hole:
db 4 ; palette
db 18, 19, 26, 27

guard:
db 4 ; palette
db 6, 7, 14, 15


SECTION "Levels", ROM0

level_1:
db 1, 2, 2, 4, 4, 4, 1, 2
db 1, 3, 3, 3, 1, 2, 1, 1
db 1, 1, 2, 2, 2, 1, 2, 1
db 2, SB, SB, 3, SB, SB, 1, 2
db 2, SB, 2, 2, 3, SB, 1, 2
db 2, SB, 4, 3, 1, SB, 1, 2
db 1, SB, 2, 4, 1, SB, 1, 2


;----------------------------------------------------
; Internal ram
;----------------------------------------------------

SECTION	"RAM_Other_Variables",WRAM0[$C000]

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

random_number_seed:
ds 1

