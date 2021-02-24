# Makefile
#
# targets are:
# all: rebuild the project (default)
# clean: remove all objects and executables

SOURCES = toylock.ml search.ml
CLEAN = *.cmo *.cmx *.cmi *.o main

.PHONY: all clean

all: main

clean:
	rm -f imp $(CLEAN)

main: $(SOURCES)
	ocamlc -o main $(SOURCES)
