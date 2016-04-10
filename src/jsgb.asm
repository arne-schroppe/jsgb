;****************************************************************************************************************************************************
;*	Blank Simple Source File
;*
;****************************************************************************************************************************************************
;*
;*
;****************************************************************************************************************************************************

;****************************************************************************************************************************************************
;*	Includes
;****************************************************************************************************************************************************
	; system includes
	INCLUDE	"lib/hardware.inc"

	; project includes
;	INCLUDE	"blankasm.inc"


; GAMEBOY SYSTEM CONSTANTS
; the hardware registers for the Game Boy begin at address $FF00
; All the 8 bit register addresses below are offsets relative to $FF00
JOYPAD_REGISTER			equ		$00		; joypad
PAD_PORT_DPAD			equ		%00100000	; select d-pad buttons
PAD_PORT_BUTTONS		equ		%00010000	; select other buttons
PAD_OUTPUT_MASK			equ		%00001111	; mask for the output buttons
DPAD_DOWN				equ		7
DPAD_UP					equ		6
DPAD_LEFT				equ		5
DPAD_RIGHT				equ		4
START_BUTTON			equ		3
SELECT_BUTTON			equ		2
B_BUTTON				equ		1
A_BUTTON				equ		0
DPAD_DOWN_MASK			equ		%10000000
DPAD_UP_MASK			equ		%01000000
DPAD_LEFT_MASK			equ		%00100000
DPAD_RIGHT_MASK			equ		%00010000
START_BUTTON_MASK		equ		%00001000
SELECT_BUTTON_MASK		equ		%00000100
B_BUTTON_MASK			equ		%00000010
A_BUTTON_MASK			equ		%00000001

DIV_REGISTER			equ		$04		; divide timer... read to get time, write to reset it to 0
TIMA_REGISTER			equ		$05		; main timer... freq is set in TAC reg, generates interupt when overflows
TMA_REGISTER			equ		$06		; Timer Modulo... main timer loaded with this value after it overflows
TAC_REGISTER			equ		$07		; Timer Control
TIMER_STOP				equ		%00000100	; timer halt flag... 0=stop, 1=run
TIMER_FREQ_MASK			equ		%00000011	; mask for timer frequency bits
TIMER_FREQ_4KHz			equ		%00000000	; main timer runs at 4.096 KHz
TIMER_FREQ_262KHz		equ		%00000001	; main timer runs at 262.144 KHz
TIMER_FREQ_65KHZ		equ		%00000010	; main timer runs at 65.536 KHz
TIMER_FREQ_16KHz		equ		%00000011	; main timer runs at 15.384 KHz

IRQ_FLAG_REGISTER		equ		$0F		; Interrupt Flag
VBLANK_INT				equ		%00000001	; bit 0 = vblank interrupt on/off
LCDC_INT				equ		%00000010	; bit 1 = LCDC interrupt on/off
TIMER_INT				equ		%00000100	; bit 2 = Timer Overflow interrupt on/off
SERIAL_INT				equ		%00001000	; bit 3 = Serial I/O Transfer Completion interrupt on/off
CONTROLLER_INT			equ		%00010000	; bit 4 = ??

LCDC_CONTROL			equ		$40		; LCD (Graphics) Control
BKG_DISP_FLAG			equ		%00000001	; bit 0 = background tile map is on if set
SPRITE_DISP_FLAG		equ		%00000010	; bit 1 = sprites are on if set
SPRITE_DISP_SIZE		equ		%00000100	; bit 2 = sprite size (0=8x8 pixels, 1=16x8)
BKG_MAP_LOC				equ		%00001000	; bit 3 = background tile map location (0=$9800-$9bff, 1=$9c00-$9fff)
TILES_LOC				equ		%00010000	; bit 4 = tile data location (0=$8800-$97ff, 1=$8000-$8fff)
WINDOW_DISP_FLAG		equ		%00100000	; bit 5 = window tile map is on if set
WINDOW_MAP_LOC			equ		%01000000	; bit 6 = window tile map location (0=$9800-$9bff, 1=$9c00-9fff)
DISPLAY_FLAG			equ		%10000000	; bit 7 = LCD display on if set

LCDC_STATUS				equ		$41		; LCDC Status
DISP_CYCLE_MODE			equ		%00000011	; mask for the display cycle mode bits
VBLANK_MODE				equ		%00000000	; system is in vertical blanking interval
HBLANK_MODE				equ		%00000001	; system is in a horizontal blanking interval
SPRITE_MODE				equ		%00000010	; system is reading sprite RAM
LCD_TRANSFER			equ		%00000011	; system is transfering data to the LCD driver

SCROLL_BKG_Y			equ		$42		; vertical scroll position of background tile map
SCROLL_BKG_X			equ		$43		; horizontal scroll position of background tile map

LCDC_LY_COUNTER			equ		$44		; increments every scan line (0..143 = display, 144-153 = vblank)
LY_COMPARE				equ		$45		; ??

DMA_REGISTER			equ		$46		; DMA Transfer and Start Address

PALETTE_BKG				equ		$47		; palette data for background tile map
PALETTE_SPRITE_0		equ		$48		; sprite palette 0 data
PALETTE_SPRITE_1		equ		$49		; sprite palette 1 data

POS_WINDOW_Y			equ		$4A		; window tile map Y position
POS_WINDOW_X			equ		$4B		; window tile map X position

INTERRUPT_ENABLE		equ		$ff		; Interrupt Enable

; $ff80 to $fffe is 128 bytes of internal RAM
STACK_TOP				equ		$fff4		; put the stack here

; video ram display locations
TILES_MEM_LOC_0			equ		$8800		; tile map tiles only
TILES_MEM_LOC_1			equ		$8000		; tile maps and sprite tiles

MAP_MEM_LOC_0			equ		$9800		; background and window tile maps
MAP_MEM_LOC_1			equ		$9c00		; (select which uses what mem loc in LCDC_CONTROL register)

SPRITE_ATTRIB_MEM_LOC	equ		$fe00		; OAM memory (sprite attributes)

; sprite attribute flags
SPRITE_FLAGS_PAL		equ		%00010000	; palette (0=sprite pal 0, 1=sprite pal 1)
SPRITE_FLAGS_XFLIP		equ		%00100000	; sprite is horizontal flipped
SPRITE_FLAGS_YFLIP		equ		%01000000	; sprite is vertical flipped
SPRITE_FLAGS_PRIORITY	equ		%10000000	; sprite display priority (0=on top bkg & win, 1=behind bkg & win)


	
;****************************************************************************************************************************************************
;*	user data (constants)
;****************************************************************************************************************************************************


;****************************************************************************************************************************************************
;*	equates
;****************************************************************************************************************************************************


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
	reti
	
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
Start::
	; init the stack pointer
	ld		sp, $FFFE

	; enable only vblank interrupts
	ld		a, VBLANK_INT			; set vblank interrupt bit
	ldh		[INTERRUPT_ENABLE], a	; load it to the hardware register

	; standard inits
	sub		a	;	a = 0
	ldh		[LCDC_STATUS], a	; init status
	ldh		[LCDC_CONTROL], a	; init LCD to everything off
	ldh		[SCROLL_BKG_X], a	; background map will start at 0,0
	ldh		[SCROLL_BKG_Y], a

	ld		a, 0
	ld		[vblank_flag], a

	; load the tiles
	ld		bc, jellysplash_tile_data
	call	LoadTiles

	; load the background map
	ld		bc, jellysplash_map_data
	call	LoadMapToBkg

	; init the palettes
	call	InitPalettes

	; clear the sprite data
	call	InitSprites

	; init  my spaceship sprite
;	ld		a, $40
;	ld		(spaceship_xpos), a
;	ld		(spaceship_ypos), a
;	ld		a, 2
;	ld		(spaceship_tile), a
;	ld		a, 0
;	ld		(spaceship_flags), a

	; set display to on, background on, window off, sprites on, sprite size 8x8
	;	tiles at $8000, background map at $9800, window map at $9C00
	ld		a, DISPLAY_FLAG + BKG_DISP_FLAG + SPRITE_DISP_FLAG + TILES_LOC + WINDOW_MAP_LOC
	ldh		[LCDC_CONTROL],a

	; allow interrupts to start occuring
	ei


; main game loop
Game_Loop
	; don't do a frame update unless we have had a vblank
	ld		a, [vblank_flag]
	cp		0
	jp		z, end_game_loop

	; get this frame's joypad info
	call	ReadJoypad

	; adjust sprite due to d-pad presses
;	call	MoveSpaceship

	; reset vblank flag
	ld		a, 0
	ld		[vblank_flag], a

end_game_loop
; time to loop!
	jp		Game_Loop




;***************************************************************
;* Subroutines
;***************************************************************

	SECTION "Support Routines",HOME




;------------------------------------------
; init the local copy of the sprites
;------------------------------------------
InitSprites
	ld		hl, $c000	; my sprites are at $c000
	ld		b, 40*4		; 40 sprites, 4 bytes per sprite
	ld		a, $ff
init_sprites_loop
	ld		[hli], a
	dec		b
	jr		nz, init_sprites_loop

	ret


;----------------------------------------------------
; load the tiles from ROM into the tile video memory
;
; IN:	bc = address of tile data to load
;----------------------------------------------------
LoadTiles
	ld		hl, TILES_MEM_LOC_1	; load the tiles to tiles bank 1

	ld		de, 4 * 16
	ld		d, $10  ; 16 bytes per tile
	ld		e, 12  ; number of tiles to load

load_tiles_loop
	; only write during
	ldh		a, [LCDC_STATUS]	; get the status
	and		SPRITE_MODE			; don't write during sprite and transfer modes
	jr		nz, load_tiles_loop

	ld		a, [bc]		; get the next value from the source
	ld		[hli], a	; load the value to the destination, incrementing dest. ptr
	inc		bc			; increment the source ptr

	; now loop de times
	dec		d
	jp		nz, load_tiles_loop
	dec		e
	jp		nz, load_tiles_loop

	ret



;----------------------------------------------------
; load the tile map to the background
;
; IN:	bc = address of map to load
;----------------------------------------------------
LoadMapToBkg
	ld		hl, MAP_MEM_LOC_0	; load the map to map bank 0

	ld		d, $00	; 256 bytes per "block"
	ld		e, $04	; 4 blocks (32x32 tiles, 1024 bytes)

load_map_loop
	; only write during
	ldh		a, [LCDC_STATUS]	; get the status
	and		SPRITE_MODE			; don't write during sprite and transfer modes
	jr		nz, load_map_loop

	ld		a, [bc]		; get the next value from the source
	ld		[hli], a	; load the value to the destination, incrementing dest. ptr
	inc		bc			; increment the source ptr

	; now loop de times
	dec		d
	jp		nz, load_map_loop
	dec		e
	jp		nz, load_map_loop

	ret




;----------------------------------------------------
; init the palettes to basic
;----------------------------------------------------
InitPalettes
	ld		a, %11100100	; set palette colors

	; load it to all the palettes
	ldh		[PALETTE_BKG], a
	ldh		[PALETTE_SPRITE_0], a
	ldh		[PALETTE_SPRITE_1], a

	ret



;-----------------------------------------------------------------------
; read the joypad
;
; output:
; 		This loads two variables:
;			joypad_held		- what buttons are currently held
;			joypad_down		- what buttons went down since last joypad read
;-----------------------------------------------------------------------
ReadJoypad
	; get the d-pad buttons
	ld		a, PAD_PORT_DPAD		; select d-pad
	ldh		[JOYPAD_REGISTER], a	; send it to the joypad
	ldh		a, [JOYPAD_REGISTER]
	ldh		a, [JOYPAD_REGISTER]
	ldh		a, [JOYPAD_REGISTER]
	ldh		a, [JOYPAD_REGISTER]
	ldh		a, [JOYPAD_REGISTER]
	ldh		a, [JOYPAD_REGISTER]	; get the result back (takes a few cycles)
	cpl			; bit-flip the result
	and		PAD_OUTPUT_MASK		; mask out the output bits
	swap	a					; put the d-pad button results to top nibble
	ld		b, a				; and store it

	; get A / B / SELECT / START buttons
	ld		a, PAD_PORT_BUTTONS		; select buttons
	ldh		[JOYPAD_REGISTER], a	; send it to the joypad
	ldh		a, [JOYPAD_REGISTER]
	ldh		a, [JOYPAD_REGISTER]
	ldh		a, [JOYPAD_REGISTER]
	ldh		a, [JOYPAD_REGISTER]
	ldh		a, [JOYPAD_REGISTER]
	ldh		a, [JOYPAD_REGISTER]	; get the result back (takes even more cycles?)
	cpl			; bit-flip the result
	and		PAD_OUTPUT_MASK		; mask out the output bits
	or		b					; add it to the other button bits
	ld		b, a			; put it back in c

	; calculate the buttons that went down since last joypad read
	ld		a, [joypad_held]	; grab last button bits
	cpl							; invert them
	and		b					; combine the bits with current bits
	ld		[joypad_down], a	; store just-went-down button bits

	ld		a, b
	ld      [joypad_held], a	; store the held down button bits

	ld		a, $30       ; reset joypad
    ldh		[JOYPAD_REGISTER],A

	ret			; done




;---------------------------------------------------
; my vblank routine - do all graphical changes here
; while the display is not drawing
;---------------------------------------------------
VBlankFunc
	di		; disable interrupts
	push	af

	; increment my little timer
	ld		a, [ScrollTimer]			; get the scroll timer
	inc		a					; increment it
	ld		[ScrollTimer], a

	; is it time to scroll yet?
	and		%00000111
	jr		nz, vblank_sprite_DMA

vblank_do_scroll
	; do a background screen scroll
	ldh		a, [SCROLL_BKG_X]		; scroll the background horiz one bit
	inc		a
	ldh		[SCROLL_BKG_X], a

; load the sprite attrib table to OAM memory
vblank_sprite_DMA
	ld		a, $c0				; dma from $c000 (where I have my local copy of the attrib table)
	ldh		[DMA_REGISTER], a	; start the dma

	ld		a, $28		; wait for 160 microsec (using a loop)
vblank_dma_wait
	dec		a
	jr		nz, vblank_dma_wait

	ld		hl, SPRITE_ATTRIB_MEM_LOC

	; set the vblank occured flag
	ld		a, 1
	ld		[vblank_flag], a

	pop af
	ei		; enable interrupts
	reti	; and done



;-------------------------------------------------------------
; adjust my spaceship sprite based on d-pad presses.  This
; both moves the sprite and chooses the sprite attributes to
; make the sprite face the correct direction
;-------------------------------------------------------------
MoveSpaceship
	push	af

	; check buttons for d-pad presses
check_for_up
	ld		a, [joypad_held]
	bit		PADB_UP, a
	jp		z, check_for_down	; if button not pressed then done

	; up was held down
	ld		a, [ScrollTimer]	; only move sprite every 2nd vblank
	and		%00000001
	jr		nz, check_for_left

	; move sprite up a pixel
	ld		a, [spaceship_ypos]
	dec		a
	ld		[spaceship_ypos], a

	; don't check down, since up + down should never occur
	jp		check_for_left

check_for_down
	ld		a, [joypad_held]
	bit		PADB_DOWN, a
	jp		z, check_for_left	; if button not pressed then done

	; down was held down
	ld		a, [ScrollTimer]	; only move sprite every 2nd vblank
	and		%00000001
	jr		nz, check_for_left

	; move sprite up a pixel
	ld		a, [spaceship_ypos]
	inc		a
	ld		[spaceship_ypos], a

check_for_left
	ld		a, [joypad_held]
	bit		PADB_LEFT, a
	jp		z, check_for_right	; if button not pressed then done

	; left was pressed
	ld		a, [ScrollTimer]	; only move sprite every 2nd vblank
	and		%00000001
	jr		nz, done_checking_dpad

	; move sprite left one pixel
	ld		a, [spaceship_xpos]
	dec		a
	ld		[spaceship_xpos], a

	jp		done_checking_dpad	; if left was pressed, don't check right

check_for_right
	ld		a, [joypad_held]
	bit		PADB_RIGHT, a
	jp		z, done_checking_dpad	; if button not pressed then done

	; right was pressed
	ld		a, [ScrollTimer]	; only move sprite every 2nd vblank
	and		%00000001
	jr		nz, done_checking_dpad

	; move sprite left one pixel
	ld		a, [spaceship_xpos]
	inc		a
	ld		[spaceship_xpos], a

done_checking_dpad
	pop		af
	ret



  SECTION "Tiles", HOME

  INCLUDE "graphics.inc"




; Internal ram


SECTION	"RAM_Start_Sprites",BSS[$C000]

; local version of sprite attrib table
spaceship_ypos:
ds 		1
spaceship_xpos:
ds		1
spaceship_tile:
ds		1
spaceship_flags:
ds		1

; bullet sprites start here (16 of them)
bullet_sprites:
ds		1





SECTION	"RAM_Other_Variables",BSS[$C0A0]
; other variables

; joypad values
joypad_held:
ds		1		; what buttons are currently held
joypad_down:
ds		1		; what buttons went down since last joypad read

; scroll values
world_x:
ds		2
world_y:
ds		2

x_scrl_accum:
ds		1
y_scrl_accum:
ds		1

; bullets (16 of them, 2 bytes for each)
;	1st byte = orientation (3 bits) - if $ff, this bullet is unused
;	2nd byte = time left to live (in vblanks)
bullet_data:
ds		32

; frame timing
vblank_flag:
ds		1		; set if a vblank occured since last pass through game loop

; scroll direction flag
scrl_dir_flag:
ds		1

; temp variables
ScrollTimer:
ds		1		; temp variable for slowing down scroll speed





