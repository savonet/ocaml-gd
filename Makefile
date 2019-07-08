PKGNAME = gd

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLMKLIB=ocamlmklib
OCAMLFIND=ocamlfind
OCAMLDOC=ocamldoc -html
OCAMLLIB=`$(OCAMLC) -where`

TASKINSTALL  = $(OCAMLFIND) install $(NAME) $(TASKINSTALLOPTS)

# This turns on some checks that should make the library more secure but
# may degrade performance. If you're more concerned about performance than
# security, comment out the next line.
SAFETY_FLAG = -DSAFER
# If you don't have libjpeg, comment out the 'JPEG_LIB' line.
# Similarly for Freetype2.
JPEG_LIB = -ljpeg
FT2_LIB = -lfreetype
CC=gcc
CINCLUDES=-I$(OCAMLLIB) -I/usr/include 
LIBS=-lgd -lpng -lz $(JPEG_LIB) $(FT2_LIB)
ifdef JPEG_LIB
JPEG_FLAG = -DHAVE_JPEG
endif
ifdef FT2_LIB
FT2_FLAG = -DHAVE_FREETYPE
endif
CFLAGS = $(CINCLUDES) -fPIC -W -Wall -Wno-unused \
	$(JPEG_FLAG) $(FT2_FLAG) $(SAFETY_FLAG)

OCAMLCFLAGS=-labels -unsafe
OCAMLOPTFLAGS=-labels -inline 2

BYTE_OBJECTS  = gd.cmi gd.cmo
NAT_OBJECTS = gd.cmi gd.cmx
C_OBJECTS = gdstubs.o
STUBLIBS = dllocamlgd.so libocamlgd.a
BYTE_ARCHIVE  = gd.cma
NAT_ARCHIVE = gd.cmxa


.PHONY : all opt install uninstall test test.opt docs
all : $(BYTE_ARCHIVE)
opt : $(NAT_ARCHIVE)


$(BYTE_ARCHIVE) : $(BYTE_OBJECTS) $(C_OBJECTS)
	$(OCAMLMKLIB) -o gd gd.cmo gdstubs.o -oc ocamlgd $(LIBS)
$(NAT_ARCHIVE) : $(NAT_OBJECTS) $(C_OBJECTS)
	$(OCAMLMKLIB) -o gd gd.cmx gdstubs.o -oc ocamlgd $(LIBS)


gd.cmx: gd.cmi gd.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c gd.ml

gd.cmo: gd.cmi gd.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c gd.ml

gd.cmi: gd.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c gd.mli


gdstubs.o : gdstubs.c
	$(CC) $(CFLAGS) -c gdstubs.c


test : gdtest
	./gdtest gdtest.log

test.opt : gdtest.opt
	./gdtest.opt gdtest.log

gdtest: gdtest.cmo
	$(OCAMLC) -o gdtest -dllpath . str.cma gd.cma gdtest.cmo 

gdtest.cmo: all gdtest.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c gdtest.ml

gdtest.opt: gdtest.cmx
	$(OCAMLOPT) -ccopt "-L ." -o gdtest.opt str.cmxa gd.cmxa gdtest.cmx

gdtest.cmx: opt gdtest.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c gdtest.ml 


docs : gd.mli
	$(OCAMLDOC) -d doc gd.mli


install :
	files=$$( \
		for f in $(BYTE_ARCHIVE) gd.cmx $(NAT_ARCHIVE) $(STUBLIBS) \
			*.mli *.cmi gd.a META; do \
			if [ -f "$$f" ]; then echo $$f; fi; \
		done; \
	) && \
	$(TASKINSTALL) $(PKGNAME) $$files


uninstall :
	 ocamlfind remove $(PKGNAME)


clean :
	 -rm -f *.cmi *.cmo *.cmx *.cma *.cmxa *.a *.o *.so \
	   gdtest gdtest.opt gdtest.log
