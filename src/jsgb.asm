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

TILES_PER_LINE  equ  $20
	
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

  call  ClearMap


  ld    b, $01
  ld    c, $05
  ld    de, TILES_PER_LINE * $3 + $4
  call  LoadAtPosition


	; init the palettes
	call	InitPalettes


	; set display to on, background on, window off, sprites off, sprite size 8x8
	;	tiles at $8000, background map at $9800, window map at $9C00
	ld		a, DISPLAY_FLAG + BKG_DISP_FLAG + TILES_LOC + WINDOW_MAP_LOC
	ldh		[LCDC_CONTROL],a

	; allow interrupts to start occuring
	ei


; main game loop
Game_Loop
	; don't do a frame update unless we have had a vblank
	ld		a, [vblank_flag]
	cp		0
	jp		z, end_game_loop


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




ClearMap
  ld    hl, MAP_MEM_LOC_0

  ld    d, $20
  ld    e, $20

clear_map_loop
	; TODO turn into macro
	ldh		a, [LCDC_STATUS]	; get the status
	and		SPRITE_MODE			; don't write during sprite and transfer modes
	jr		nz, clear_map_loop

  ld    a, 0
  ld    [hli], a

  dec   d
  jp    nz, clear_map_loop
  ld    d, $20

  dec   e
  jp    nz, clear_map_loop

  ret


;----------------------------------------------------
; load tile at position
;
; IN:	b = Tile address upper part
;     c = Tile address lower part
;     de = tile pos
;----------------------------------------------------
LoadAtPosition
	ld		hl, MAP_MEM_LOC_0	; load the map to map bank 0
  add   hl, de

load_pos_loop
	; only write during
	ldh		a, [LCDC_STATUS]	; get the status
	and		SPRITE_MODE			; don't write during sprite and transfer modes
	jr		nz, load_pos_loop

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
InitPalettes
	ld		a, %11100100	; set palette colors

	; load it to all the palettes
	ldh		[PALETTE_BKG], a
	ldh		[PALETTE_SPRITE_0], a
	ldh		[PALETTE_SPRITE_1], a

	ret



; Graphics and Data

  SECTION "Tiles", HOME

  INCLUDE "graphics.inc"


; Internal ram




SECTION	"RAM_Other_Variables",BSS[$C000]

; frame timing
vblank_flag:
ds		1		; set if a vblank occured since last pass through game loop


