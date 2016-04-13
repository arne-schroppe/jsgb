NAME=jsgb
OBJ = jsgb.obj rom_header.obj

all: $(OBJ)
	rgblink -m$(NAME).map -n$(NAME).sym -o$(NAME).gb $^
	rgbfix -p0 -v $(NAME).gb

%.obj: %.asm
	rgbasm -o$@ $<

clean:
	-rm *.map
	-rm *.sym
	-rm *.obj
	-rm *.gb
