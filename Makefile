# $Id$


include vsn.mk
VSN = $(IDX-TSUNAMI_VSN)
TSUNAMIPATH = .
TARDIR = idx-tsunami-$(VSN)
PA = -pa ./ebin -pa ./src -pa . -pa ./system -pa ../ebin -pa .. -pa ../system -pa ../src

prefix = /usr/local/idx-tsunami

DEBUGINFO:=+debug_info
ERLC = erlc $(DEBUGINFO)
OUTDIR = ebin
ALLERLS:= $(wildcard src/*.erl)
ALLBEAMS:=$(patsubst src/%.erl,$(OUTDIR)/%.beam, $(ALLERLS))

all:	tsunami.boot

show:
	@echo "sources: $(ALLERLS)"
	@echo "beam: $(ALLBEAMS)"

tarball: 
	mkdir -p $(TARDIR)
	tar zcf tmp.tgz src/*.erl src/*.src include/*.hrl doc/*.txt LISEZMOI README CONTRIBUTORS COPYING src/idx-tsunami.pl.src idx-tsunamirc TODO Makefile vsn.mk src/analyse_msg.pl.src
	tar -C $(TARDIR) -zxf tmp.tgz
	mkdir $(TARDIR)/ebin
	tar zvcf  idx-tsunami-$(VSN).tar.gz $(TARDIR)
	rm -fr $(TARDIR)
	rm -fr tmp.tgz


clean:
	rm -f $(ALLBEAMS) tsunami.boot tsunami.script ebin/tsunami.app ebin/tsunami.rel ebin/idx-tsunami.pl ebin/analyse_msg.pl

tsunami.boot:	 ebin $(ALLBEAMS) $(UTILS) src/tsunami.rel.src src/tsunami.app.src src/analyse_msg.pl.src src/idx-tsunami.pl.src
	sed -e 's;%VSN%;$(VSN);' ./src/tsunami.app.src > ./ebin/tsunami.app
	sed -e 's;%VSN%;$(VSN);' ./src/tsunami.rel.src > ./ebin/tsunami.rel
	sed -e 's;%VSN%;$(VSN);' ./src/idx-tsunami.pl.src > ./ebin/idx-tsunami.pl
	sed -e 's;%VSN%;$(VSN);' ./src/analyse_msg.pl.src > ./ebin/analyse_msg.pl
	erl -noshell $(PA) ./src -s make_boot make_boot tsunami

ebin:
	mkdir ebin

$(OUTDIR)/%.beam: ebin/%.erl
	$(ERLC) -o $(OUTDIR) $<

$(OUTDIR)/%.beam: src/%.erl include/*.hrl
	$(ERLC) -o $(OUTDIR) $<

install: tsunami.boot
	mkdir -p $(DESTDIR)/$(prefix)
	mkdir -p $(DESTDIR)/$(prefix)/bin
	mkdir -p $(DESTDIR)/$(prefix)/log
	mkdir -p $(DESTDIR)/$(prefix)/etc
	mkdir -p $(DESTDIR)/$(prefix)/bin
	install -m 0644 tsunami.boot $(DESTDIR)/$(prefix)/bin
	install -m 0644 idx-tsunamirc $(DESTDIR)/$(prefix)/etc/idx-tsunamirc.default
	install ebin/idx-tsunami.pl $(DESTDIR)/${prefix}/bin
	install ebin/analyse_msg.pl $(DESTDIR)/${prefix}/bin
	mkdir -p $(DESTDIR)/$(prefix)
	mkdir -p $(DESTDIR)/$(prefix)/erlang
	mkdir -p $(DESTDIR)/$(prefix)/erlang/tsunami-$(VSN)
	mkdir -p $(DESTDIR)/$(prefix)/erlang/tsunami-$(VSN)/ebin
	install $(ALLBEAMS) $(DESTDIR)/$(prefix)/erlang/tsunami-$(VSN)/ebin

