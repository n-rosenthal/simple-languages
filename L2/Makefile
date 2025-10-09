OCAMLC=ocamlc
OCAMLFLAGS=-g
LIBS=str.cma
SOURCES=  Types.ml Terms.ml TypeInference.ml Evaluation.ml Interpreter.ml
OBJECTS=$(SOURCES:.ml=.cmo)
EXEC=l2

all: $(EXEC)

$(EXEC): $(OBJECTS)
	$(OCAMLC) -o $@ $(LIBS) $^

%.cmo: %.ml
	$(OCAMLC) -c $<

clean:
	rm -f *.cmo *.cmi *.o $(EXEC)

.PHONY: all clean

