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


; /////////////////
; //             //
; //  Tile Data  //
; //             //
; /////////////////

jellysplash_tile_data:
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$73,$73,$8C,$73,$80,$6D,$80,$7F,$80,$7F,$80,$47,$B8,$5B,$9C
DB $00,$9C,$9C,$62,$9C,$02,$6C,$02,$FC,$02,$FC,$02,$C4,$3A,$B4,$72
DB $00,$73,$73,$8C,$73,$80,$6D,$80,$7F,$80,$7F,$80,$7F,$80,$7F,$80
DB $00,$9C,$9C,$62,$9C,$02,$6C,$02,$FC,$02,$FC,$02,$FC,$02,$FC,$02
DB $5B,$BC,$67,$98,$7F,$80,$3F,$44,$1F,$23,$06,$1B,$00,$07,$00,$00
DB $B4,$7A,$CC,$32,$FC,$02,$F8,$44,$F0,$88,$C0,$B0,$00,$C0,$00,$00
DB $5B,$A4,$67,$98,$7F,$80,$3F,$40,$1F,$21,$07,$19,$00,$07,$00,$00
DB $B4,$4A,$CC,$32,$FC,$02,$F8,$04,$F0,$08,$C0,$30,$00,$C0,$00,$00
DB $00,$FC,$78,$80,$40,$80,$40,$80,$40,$80,$00,$80,$00,$00,$00,$00
DB $00,$3F,$1E,$01,$02,$01,$02,$01,$02,$01,$00,$01,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$80,$40,$80,$40,$80,$40,$80,$78,$80,$00,$FC
DB $00,$00,$00,$00,$00,$01,$02,$01,$02,$01,$02,$01,$1E,$01,$00,$3F


font_tile_data:
DB $C3,$C3,$BD,$BD,$B9,$B9,$B5,$B5,$AD,$AD,$9D,$9D,$C3,$C3,$FF,$FF
DB $EF,$EF,$CF,$CF,$AF,$AF,$EF,$EF,$EF,$EF,$EF,$EF,$83,$83,$FF,$FF
DB $C3,$C3,$BD,$BD,$FD,$FD,$F3,$F3,$CF,$CF,$BD,$BD,$81,$81,$FF,$FF
DB $C3,$C3,$BD,$BD,$FD,$FD,$E3,$E3,$FD,$FD,$BD,$BD,$C3,$C3,$FF,$FF
DB $F7,$F7,$E7,$E7,$D7,$D7,$B7,$B7,$01,$01,$F7,$F7,$E3,$E3,$FF,$FF
DB $81,$81,$BF,$BF,$83,$83,$FD,$FD,$FD,$FD,$BD,$BD,$C3,$C3,$FF,$FF
DB $E3,$E3,$DF,$DF,$BF,$BF,$83,$83,$BD,$BD,$BD,$BD,$C3,$C3,$FF,$FF
DB $81,$81,$BD,$BD,$FB,$FB,$F7,$F7,$EF,$EF,$EF,$EF,$EF,$EF,$FF,$FF
DB $C3,$C3,$BD,$BD,$BD,$BD,$C3,$C3,$BD,$BD,$BD,$BD,$C3,$C3,$FF,$FF
DB $C3,$C3,$BD,$BD,$BD,$BD,$C1,$C1,$FD,$FD,$FB,$FB,$C7,$C7,$FF,$FF
DB $FF,$FF,$EF,$EF,$EF,$EF,$FF,$FF,$FF,$FF,$EF,$EF,$EF,$EF,$FF,$FF
DB $FF,$FF,$EF,$EF,$EF,$EF,$FF,$FF,$FF,$FF,$EF,$EF,$EF,$EF,$DF,$DF
DB $F7,$F7,$EF,$EF,$DF,$DF,$BF,$BF,$DF,$DF,$EF,$EF,$F7,$F7,$FF,$FF
DB $FF,$FF,$FF,$FF,$81,$81,$FF,$FF,$FF,$FF,$81,$81,$FF,$FF,$FF,$FF
DB $EF,$EF,$F7,$F7,$FB,$FB,$FD,$FD,$FB,$FB,$F7,$F7,$EF,$EF,$FF,$FF
DB $C3,$C3,$BD,$BD,$FD,$FD,$FB,$FB,$F7,$F7,$FF,$FF,$F7,$F7,$FF,$FF
DB $C3,$C3,$BD,$BD,$A1,$A1,$AD,$AD,$A1,$A1,$BF,$BF,$C3,$C3,$FF,$FF
DB $E7,$E7,$DB,$DB,$BD,$BD,$BD,$BD,$81,$81,$BD,$BD,$BD,$BD,$FF,$FF
DB $83,$83,$DD,$DD,$DD,$DD,$C3,$C3,$DD,$DD,$DD,$DD,$83,$83,$FF,$FF
DB $E3,$E3,$DD,$DD,$BF,$BF,$BF,$BF,$BF,$BF,$DD,$DD,$E3,$E3,$FF,$FF
DB $87,$87,$DB,$DB,$DD,$DD,$DD,$DD,$DD,$DD,$DB,$DB,$87,$87,$FF,$FF
DB $81,$81,$DD,$DD,$D7,$D7,$C7,$C7,$D7,$D7,$DD,$DD,$81,$81,$FF,$FF
DB $81,$81,$DD,$DD,$D7,$D7,$C7,$C7,$D7,$D7,$DF,$DF,$8F,$8F,$FF,$FF
DB $E3,$E3,$DD,$DD,$BF,$BF,$BF,$BF,$B1,$B1,$DD,$DD,$E1,$E1,$FF,$FF
DB $BD,$BD,$BD,$BD,$BD,$BD,$81,$81,$BD,$BD,$BD,$BD,$BD,$BD,$FF,$FF
DB $C7,$C7,$EF,$EF,$EF,$EF,$EF,$EF,$EF,$EF,$EF,$EF,$C7,$C7,$FF,$FF
DB $F1,$F1,$FB,$FB,$FB,$FB,$FB,$FB,$BB,$BB,$BB,$BB,$C7,$C7,$FF,$FF
DB $9D,$9D,$DB,$DB,$D7,$D7,$CF,$CF,$D7,$D7,$DB,$DB,$9C,$9C,$FF,$FF
DB $8F,$8F,$DF,$DF,$DF,$DF,$DF,$DF,$DF,$DF,$DD,$DD,$81,$81,$FF,$FF
DB $9C,$9C,$AA,$AA,$B6,$B6,$BE,$BE,$BE,$BE,$BE,$BE,$BE,$BE,$FF,$FF
DB $9D,$9D,$AD,$AD,$B5,$B5,$B9,$B9,$BD,$BD,$BD,$BD,$BD,$BD,$FF,$FF
DB $C3,$C3,$BD,$BD,$BD,$BD,$BD,$BD,$BD,$BD,$BD,$BD,$C3,$C3,$FF,$FF
DB $83,$83,$DD,$DD,$DD,$DD,$C3,$C3,$DF,$DF,$DF,$DF,$8F,$8F,$FF,$FF
DB $C3,$C3,$BD,$BD,$BD,$BD,$BD,$BD,$B5,$B5,$C3,$C3,$FC,$FC,$FF,$FF
DB $83,$83,$DD,$DD,$DD,$DD,$C3,$C3,$D7,$D7,$DB,$DB,$8D,$8D,$FF,$FF
DB $C3,$C3,$BD,$BD,$BF,$BF,$C3,$C3,$FD,$FD,$BD,$BD,$C3,$C3,$FF,$FF
DB $80,$80,$B6,$B6,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$E3,$E3,$FF,$FF
DB $BD,$BD,$BD,$BD,$BD,$BD,$BD,$BD,$BD,$BD,$BD,$BD,$C3,$C3,$FF,$FF
DB $BE,$BE,$BE,$BE,$BE,$BE,$BE,$BE,$DD,$DD,$EB,$EB,$F7,$F7,$FF,$FF
DB $BE,$BE,$BE,$BE,$BE,$BE,$B6,$B6,$B6,$B6,$B6,$B6,$C9,$C9,$FF,$FF
DB $BE,$BE,$DD,$DD,$EB,$EB,$F7,$F7,$EB,$EB,$DD,$DD,$BE,$BE,$FF,$FF
DB $BE,$BE,$DD,$DD,$EB,$EB,$F7,$F7,$F7,$F7,$F7,$F7,$E3,$E3,$FF,$FF
DB $80,$80,$BD,$BD,$FB,$FB,$F7,$F7,$EF,$EF,$DE,$DE,$80,$80,$FF,$FF
DB $87,$87,$BF,$BF,$BF,$BF,$BF,$BF,$BF,$BF,$BF,$BF,$87,$87,$FF,$FF
DB $7F,$7F,$BF,$BF,$DF,$DF,$EF,$EF,$F7,$F7,$FB,$FB,$FD,$FD,$FF,$FF
DB $87,$87,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$87,$87,$FF,$FF
DB $EF,$EF,$D7,$D7,$BB,$BB,$7D,$7D,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00
