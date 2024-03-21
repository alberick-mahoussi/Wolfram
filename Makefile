##
## EPITECH PROJECT, 2023
## wolfram
## File description:
## wolfram
##

BINARY_DIR	:=	$(shell stack path --local-install-root)

NAME	=	wolfram

all	:
			stack build
			cp $(BINARY_DIR)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re