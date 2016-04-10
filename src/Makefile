NAME=jsgb
all:
	rgbasm -o$(NAME).obj $(NAME).asm
	rgblink -m$(NAME).map -n$(NAME).sym -o$(NAME).gb $(NAME).obj
	rgbfix -p0 -v $(NAME).gb

clean:
	-rm *.map
	-rm *.sym
	-rm *.obj
	-rm *.gb
