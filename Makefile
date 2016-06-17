
NAME = alan_machine

SRCD = src
FILES_ = main Complexity Turing
FILES = $(addprefix $(SRCD)/,$(FILES_))
SOURCES = $(addsuffix .hs, $(FILES))
OBJS = $(addsuffix .o, $(FILES))
IFACES = $(addsuffix .hi, $(FILES))
MAIN_MODULE_SOURCE = $(SRCD)/main.hs
TOOLS = $(ENCODE_TOOL)
ENCODE_TOOL = EncodeMachine
ENCODE_TOOL_SRC = $(SRCD)/encodeMachine.hs

all: $(NAME)

tools: $(TOOLS)

$(ENCODE_TOOL): $(ENCODE_TOOL_SRC)
	ghc --make -i$(SRCD) $< -o $@

ghc_depend:
	cabal install --only-dependencies

$(NAME): $(SOURCES)
	ghc --make -i$(SRCD) $(MAIN_MODULE_SOURCE) -o $@

clean:
	rm -f $(OBJS)
	rm -f $(IFACES)

fclean: clean
	rm -f $(NAME)

tools_clean:
	rm -f $(ENCODE_TOOL_SRC:.hs=.o) $(ENCODE_TOOL_SRC:.hs=.hi)
	rm -f $(TOOLS)

fclean_all: fclean tools_clean

re: fclean all

.PHONY: clean fclean all re ghc_depend tools fclean_all tools_clean
