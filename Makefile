INSTALLDIR=/tmp/foobar


OCAMLKLIBDIR=/usr/local/lib/ocaml/camltk/
include $(OCAMLKLIBDIR)Makefile.camltk


# Compiler part
BINDIR=
CAMLC=$(BINDIR)ocamlc
CAMLOPT=$(BINDIR)ocamlopt
CAMLCP=$(BINDIR)ocamlcp
CAMLDEP=$(BINDIR)ocamldep
CAMLLEX=$(BINDIR)ocamllex
CAMLYACC=$(BINDIR)ocamlyacc



# Flags part
INCLUDEDIRS = -I parsing/ -I utils/ -I typing/ -I toplevel/ -I batch/ \
	-I interface/
DEPFLAGS = $(INCLUDEDIRS)
COMPFLAG = -w A-4-6-9 -warn-error A $(INCLUDEDIRS) $(TKCOMPFLAGS)
LINKFLAG =



# Object part
TOPOBJS = utils/misc.cmo utils/tbl.cmo utils/config.cmo utils/clflags.cmo \
	utils/terminfo.cmo utils/ccomp.cmo \
	\
	parsing/linenum.cmo  parsing/location.cmo parsing/longident.cmo \
	parsing/asttypes.cmo parsing/syntaxerr.cmo parsing/pstream.cmo \
	parsing/parser.cmo parsing/lexer.cmo parsing/parse.cmo \
	\
	typing/files.cmo typing/stdlibpath.cmo typing/listextra.cmo \
	typing/ident.cmo typing/path.cmo typing/extendhandler.cmo \
	typing/subst.cmo typing/printbasic.cmo typing/printparse.cmo \
	typing/printscope.cmo typing/typecore.cmo typing/envscope.cmo \
	typing/corescope.cmo typing/modscope.cmo typing/topscope.cmo \
	typing/substcore.cmo typing/substmod.cmo typing/printtypes.cmo \
	typing/envtype.cmo typing/inputpt.cmo typing/substract.cmo \
	typing/freevars.cmo typing/outputp.cmo typing/infercore.cmo \
	typing/printmod.cmo typing/infermod.cmo typing/error.cmo \
	\
	toplevel/main.cmo


BATCHOBJS = utils/misc.cmo utils/tbl.cmo utils/config.cmo utils/clflags.cmo \
	utils/terminfo.cmo utils/ccomp.cmo \
	\
	parsing/linenum.cmo  parsing/location.cmo parsing/longident.cmo \
	parsing/asttypes.cmo parsing/syntaxerr.cmo parsing/pstream.cmo \
	parsing/parser.cmo parsing/lexer.cmo parsing/parse.cmo \
	\
	typing/files.cmo typing/stdlibpath.cmo typing/listextra.cmo \
	typing/ident.cmo typing/path.cmo typing/extendhandler.cmo \
	typing/subst.cmo typing/printbasic.cmo typing/printparse.cmo \
	typing/printscope.cmo typing/typecore.cmo typing/envscope.cmo \
	typing/corescope.cmo typing/modscope.cmo typing/topscope.cmo \
	typing/substcore.cmo typing/substmod.cmo typing/printtypes.cmo \
	typing/envtype.cmo typing/inputpt.cmo typing/substract.cmo \
	typing/freevars.cmo typing/outputp.cmo typing/infercore.cmo \
	typing/printmod.cmo typing/infermod.cmo typing/error.cmo \
	\
	batch/main.cmo


LINKOBJS = utils/misc.cmo utils/tbl.cmo utils/config.cmo utils/clflags.cmo \
	utils/terminfo.cmo utils/ccomp.cmo \
	\
	parsing/linenum.cmo  parsing/location.cmo parsing/longident.cmo \
	parsing/asttypes.cmo parsing/syntaxerr.cmo parsing/pstream.cmo \
	parsing/parser.cmo parsing/lexer.cmo parsing/parse.cmo \
	\
	typing/files.cmo typing/stdlibpath.cmo typing/listextra.cmo \
	typing/ident.cmo typing/path.cmo typing/extendhandler.cmo \
	typing/subst.cmo typing/printbasic.cmo typing/printparse.cmo \
	typing/printscope.cmo typing/typecore.cmo typing/envscope.cmo \
	typing/corescope.cmo typing/modscope.cmo typing/topscope.cmo \
	typing/substcore.cmo typing/substmod.cmo typing/printtypes.cmo \
	typing/envtype.cmo typing/inputpt.cmo typing/substract.cmo \
	typing/freevars.cmo typing/outputp.cmo typing/infercore.cmo \
	typing/printmod.cmo typing/infermod.cmo typing/error.cmo \
	\
	link/main.cmo


INTERFACEOBJS = utils/misc.cmo utils/tbl.cmo utils/config.cmo \
	utils/clflags.cmo utils/terminfo.cmo utils/ccomp.cmo \
	\
	parsing/linenum.cmo  parsing/location.cmo parsing/longident.cmo \
	parsing/asttypes.cmo parsing/syntaxerr.cmo parsing/pstream.cmo \
	parsing/parser.cmo parsing/lexer.cmo parsing/parse.cmo \
	\
	typing/files.cmo typing/stdlibpath.cmo typing/listextra.cmo \
	typing/ident.cmo typing/path.cmo typing/extendhandler.cmo \
	typing/subst.cmo typing/printbasic.cmo typing/printparse.cmo \
	typing/printscope.cmo typing/typecore.cmo typing/envscope.cmo \
	typing/corescope.cmo typing/modscope.cmo typing/topscope.cmo \
	typing/substcore.cmo typing/substmod.cmo \
	interface/tklowprint.cmo interface/tkprinttypes.cmo \
	typing/printtypes.cmo typing/envtype.cmo typing/inputpt.cmo \
	typing/substract.cmo typing/freevars.cmo typing/outputp.cmo \
	typing/infercore.cmo interface/tkprintmod.cmo \
	typing/infermod.cmo typing/error.cmo \
	\
	interface/global.cmo interface/tkloadsrc.cmo \
	interface/preferences.cmo interface/rootwindow.cmo \
	interface/modselector.cmo interface/main.cmo



# Sources part
GENERATEDSRCS = parsing/linenum.ml parsing/parser.ml parsing/lexer.ml


TOPSRCS = $(GENERATEDSRCS) \
	parsing/location.ml parsing/longident.ml parsing/syntaxerr.ml \
	parsing/asttypes.ml parsing/pstream.ml parsing/parse.ml \
	\
	utils/misc.ml utils/tbl.ml utils/config.ml utils/clflags.ml \
	utils/terminfo.ml utils/ccomp.ml \
	\
	typing/files.ml typing/stdlibpath.ml typing/listextra.ml \
	typing/ident.ml typing/path.ml typing/subst.ml typing/envscope.ml \
	typing/corescope.ml typing/extendhandler.ml typing/modscope.ml \
	typing/topscope.ml typing/printbasic.ml typing/printparse.ml \
	typing/printscope.ml typing/typecore.ml typing/substcore.ml \
	typing/inputpt.ml typing/substmod.ml typing/envtype.ml \
	typing/printtypes.ml typing/substract.ml typing/freevars.ml \
	typing/outputp.ml typing/infermod.ml typing/infercore.ml \
	typing/printmod.ml typing/error.ml \
	\
	toplevel/main.ml


BATCHSRCS = $(GENERATEDSRCS) \
	parsing/location.ml parsing/longident.ml parsing/syntaxerr.ml \
	parsing/asttypes.ml parsing/pstream.ml parsing/parse.ml \
	\
	utils/misc.ml utils/tbl.ml utils/config.ml utils/clflags.ml \
	utils/terminfo.ml utils/ccomp.ml \
	\
	typing/files.ml typing/stdlibpath.ml typing/listextra.ml \
	typing/ident.ml typing/path.ml typing/subst.ml typing/envscope.ml \
	typing/corescope.ml typing/extendhandler.ml typing/modscope.ml \
	typing/topscope.ml typing/printbasic.ml typing/printparse.ml \
	typing/printscope.ml typing/typecore.ml typing/substcore.ml \
	typing/inputpt.ml typing/substmod.ml typing/envtype.ml \
	typing/printtypes.ml typing/substract.ml typing/freevars.ml \
	typing/outputp.ml typing/infermod.ml typing/infercore.ml \
	typing/printmod.ml typing/error.ml \
	\
	batch/main.ml


LINKSRCS = parsing/location.ml parsing/longident.ml parsing/syntaxerr.ml \
	parsing/asttypes.ml parsing/pstream.ml parsing/parse.ml \
	\
	utils/misc.ml utils/tbl.ml utils/config.ml utils/clflags.ml \
	utils/terminfo.ml utils/ccomp.ml \
	\
	typing/files.ml typing/stdlibpath.ml typing/listextra.ml \
	typing/ident.ml typing/path.ml typing/subst.ml typing/envscope.ml \
	typing/corescope.ml typing/extendhandler.ml typing/modscope.ml \
	typing/topscope.ml typing/printbasic.ml typing/printparse.ml \
	typing/printscope.ml typing/typecore.ml typing/substcore.ml \
	typing/inputpt.ml typing/substmod.ml typing/envtype.ml \
	typing/printtypes.ml typing/substract.ml typing/freevars.ml \
	typing/outputp.ml typing/infermod.ml typing/infercore.ml \
	typing/printmod.ml typing/error.ml \
	\
	link/main.ml


INTERFACESRCS = parsing/location.ml parsing/longident.ml parsing/syntaxerr.ml \
	parsing/asttypes.ml parsing/pstream.ml parsing/parse.ml \
	\
	utils/misc.ml utils/tbl.ml utils/config.ml utils/clflags.ml \
	utils/terminfo.ml utils/ccomp.ml \
	\
	typing/files.ml typing/stdlibpath.ml typing/listextra.ml \
	typing/ident.ml typing/path.ml typing/subst.ml typing/envscope.ml \
	typing/corescope.ml typing/extendhandler.ml typing/modscope.ml \
	typing/topscope.ml typing/printbasic.ml typing/printparse.ml \
	typing/printscope.ml typing/typecore.ml typing/substcore.ml \
	typing/inputpt.ml typing/substmod.ml typing/envtype.ml \
	interface/printcontext.mli \
	interface/tkprinttypes.ml typing/printtypes.ml typing/substract.ml \
	typing/freevars.ml typing/outputp.ml typing/infermod.ml \
	typing/infercore.ml interface/tkprintmod.ml typing/error.ml \
	\
	interface/global.ml interface/tklowprint.ml interface/tkloadsrc.ml \
	interface/preferences.ml interface/rootwindow.ml \
	interface/modselector.ml interface/main.ml



# Exec file names
TOPNAME = bin/ocamlexc
BATCHNAME = bin/ocamlexcc
LINKNAME = bin/ocamlexcl
INTERFACENAME = bin/ocamlexcli


# *******************************************************************
# Avanti...

# Building the world (but the toplevel)
all: batch link toplevel interface
opt: batchopt linkopt toplevelopt interfaceopt

install: batch link toplevel interface stdlibcme
	cp -R bin $(INSTALLDIR)
	ln -s $(INSTALLDIR)/bin/batch /usr/local/bin/batch
	ln -s $(INSTALLDIR)/bin/link /usr/local/bin/link
	ln -s $(INSTALLDIR)/bin/toplevel  /usr/local/bin/toplevel
	ln -s $(INSTALLDIR)/bin/interface /usr/local/bin/interface	
	cp -R stdlib $(INSTALLDIR)

installopt: batchopt linkopt toplevelopt interfaceopt stdlibcme
	cp -R bin $(INSTALLDIR)
	ln -s $(INSTALLDIR)/bin/batchopt /usr/local/bin/batch.opt
	ln -s $(INSTALLDIR)/bin/linkopt /usr/local/bin/link.opt
	ln -s $(INSTALLDIR)/bin/toplevelopt  /usr/local/bin/toplevel.opt
	ln -s $(INSTALLDIR)/bin/interfaceopt /usr/local/bin/interface.opt
	cp -R stdlib $(INSTALLDIR)

stdlibcme:
	./bin/makelibcme


toplevel: $(TOPOBJS)
	$(CAMLC) $(LINKFLAG) $(TOPOBJS) -o $(TOPNAME)

batch: $(BATCHOBJS)
	$(CAMLC) $(LINKFLAG) $(BATCHOBJS) -o $(BATCHNAME)

link: $(LINKOBJS)
	$(CAMLC) $(LINKFLAG) $(LINKOBJS) -o $(LINKNAME)

interface: $(INTERFACEOBJS)
	$(CAMLC) -custom unix.cma $(WITH_TK) $(INTERFACEOBJS) \
		-cclib -lunix -o $(INTERFACENAME)



toplevelopt: $(TOPOBJS:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAG) $(TOPOBJS:.cmo=.cmx) -o $(TOPNAME).opt

batchopt: $(BATCHOBJS:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAG) $(BATCHOBJS:.cmo=.cmx) -o $(BATCHNAME).opt

linkopt: $(LINKOBJS:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAG) $(LINKOBJS:.cmo=.cmx) -o $(LINKNAME).opt

interfaceopt: $(INTERFACEOBJS:.cmo=.cmx)
	$(CAMLOPT) unix.cmxa $(WITH_TK_OPT) $(INTERFACEOBJS:.cmo=.cmx)\
		-cclib -lunix -o $(INTERFACENAME).opt



parsing/parser.mli parsing/parser.ml: parsing/parser.mly
	$(CAMLYACC) parsing/parser.mly

parsing/lexer.ml: parsing/lexer.mll
	$(CAMLLEX) parsing/lexer.mll

parsing/linenum.ml: parsing/linenum.mll
	$(CAMLLEX) parsing/linenum.mll


.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAG) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAG) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAG) -c $<


# Clean up
clean:
	rm -f parsing/*.cm[iox] parsing/*.o parsing/*~
	rm -f parsing/parser.mli
	rm -f utils/*.cm[iox] utils/*.o utils/*~
	rm -f typing/*.cm[iox] typing/*.o typing/*~
	rm -f toplevel/*.cm[iox] toplevel/*.o toplevel/*~
	rm -f batch/*.cm[iox] batch/*.o batch/*~
	rm -f link/*.cm[iox] link/*.o link/*~
	rm -f interface/*.cm[iox] interface/*.o interface/*~
	rm -f oldlib/*.cme oldlib/*~
	rm -f stdlib/*.cme stdlib/*~
	rm -f tests/*.cme tests/*/*.cme
	rm -f $(GENERATEDSRCS)
	rm -f $(TOPNAME) $(BATCHNAME) $(LINKNAME) $(INTERFACENAME)
	rm -f $(TOPNAME).opt $(BATCHNAME).opt $(LINKNAME).opt \
		$(INTERFACENAME).opt
	rm -f *~

# Dependencies
depend: $(GENERATEDSRCS)
	$(CAMLDEP) $(DEPFLAGS) batch/*.ml{,i} interface/*.ml{,i} \
		link/*.ml{,i} parsing/*.ml{,i} toplevel/*.ml{,i} \
		typing/*.ml{,i} utils/*.ml{,i} > .depend

include .depend
# DO NOT DELETE
