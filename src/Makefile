OCAMLBUILD= ocamlbuild -no-links -classic-display \
		-libs graphics,unix,bigarray,gnuplot \
		-cflags " -I /usr/lib/ocaml/gnuplot/ -I /usr/local/lib/ocaml/3.11.2/gnuplot/ -I graphics.cmxa"\
		-lflags " -I /usr/lib/ocaml/gnuplot/ -I /usr/local/lib/ocaml/3.11.2/gnuplot/ -I graphics.cmxa"\
		-tags debug,annot

TARGET=native
MAIN=votesys

all: $(MAIN)

native: TARGET := native
native: all
opt: native
$(MAIN).opt: native
$(MAIN).native: native


byte: TARGET := byte
byte: all
$(MAIN).byte: byte


$(MAIN): $(MAIN).target
	cp _build/$(MAIN).$(TARGET) $(MAIN)

$(MAIN).target:
	$(OCAMLBUILD) -use-menhir $(MAIN).$(TARGET)

clean:
	ocamlbuild -classic-display -clean

realclean: clean
	rm -f $(MAIN) *~

cleanall: realclean
