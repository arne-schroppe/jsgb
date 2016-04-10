; ///////////////////////
; //                   //
; //  File Attributes  //
; //                   //
; ///////////////////////

; Filename: jelly-splash.png
; Pixel Width: 64px
; Pixel Height: 32px

; /////////////////
; //             //
; //  Constants  //
; //             //
; /////////////////

jellysplash_tile_map_size EQU $20
jellysplash_tile_map_width EQU $08
jellysplash_tile_map_height EQU $04

jellysplash_tile_data_size EQU $90
jellysplash_tile_count EQU $20

; ////////////////
; //            //
; //  Map Data  //
; //            //
; ////////////////

jellysplash_map_data:
DB $00,$00,$01,$02,$00,$00,$03,$04,$00,$00
DB $00,$00,$05,$06,$00,$00,$07,$08,$00,$00

; /////////////////
; //             //
; //  Tile Data  //
; //             //
; /////////////////

jellysplash_tile_data:
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$73,$73,$8C,$73,$80,$6D,$80,$7F,$80,$7F,$80,$7F,$80,$43,$BC
DB $00,$CE,$CE,$31,$CE,$01,$B6,$01,$FE,$01,$FE,$01,$FE,$01,$C2,$3D
DB $00,$73,$73,$8C,$73,$80,$6D,$80,$7F,$80,$7F,$80,$7F,$80,$7F,$80
DB $00,$CE,$CE,$31,$CE,$01,$B6,$01,$FE,$01,$FE,$01,$FE,$01,$FE,$01
DB $4D,$AE,$6D,$9E,$73,$8C,$7F,$80,$3F,$44,$1F,$23,$06,$1B,$00,$07
DB $B2,$75,$B6,$79,$CE,$31,$FE,$01,$FC,$22,$F8,$C4,$60,$D8,$00,$E0
DB $7F,$80,$5D,$A2,$63,$9C,$7F,$80,$3F,$40,$1F,$20,$07,$19,$00,$07
DB $FE,$01,$BA,$45,$C6,$39,$FE,$01,$FC,$02,$F8,$04,$E0,$98,$00,$E0

