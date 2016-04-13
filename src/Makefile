NAME=jsgb
all:
	rgbasm -ojsgb.obj jsgb.asm
	rgbasm -orom_header.obj rom_header.asm
	rgblink -m$(NAME).map -n$(NAME).sym -o$(NAME).gb jsgb.obj rom_header.obj
	rgbfix -p0 -v $(NAME).gb

clean:
	-rm *.map
	-rm *.sym
	-rm *.obj
	-rm *.gb
