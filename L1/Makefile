OCAMLC=ocamlc
OCAMLFLAGS=-g
LIBS=str.cma
SOURCES=Terms.ml Types.ml Repr.ml main.ml
OBJECTS=$(SOURCES:.ml=.cmo)
EXEC=l1

all: $(EXEC)

$(EXEC): $(OBJECTS)
	$(OCAMLC) -o $@ $(LIBS) $^

%.cmo: %.ml
	$(OCAMLC) -c $<

clean:
	rm -f *.cmo *.cmi *.o $(EXEC)

.PHONY: all clean

