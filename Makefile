# $Id$


include vsn.mk
VSN = $(IDX-TSUNAMI_VSN)
TSUNAMIPATH = .
TARDIR = idx-tsunami-$(VSN)
PA = -pa ./ebin -pa ./src -pa . -pa ./system -pa ../ebin -pa .. -pa ../system -pa ../src

prefix = /usr/local/idx-tsunami

ERLC = erlc
OUTDIR = ebin
OBJS = \
	$(OUTDIR)/tsunami.beam \
	$(OUTDIR)/ts_launcher.beam \
	$(OUTDIR)/ts_client.beam \
	$(OUTDIR)/ts_mon.beam \
	$(OUTDIR)/ts_client_rcv.beam \
	$(OUTDIR)/ts_utils.beam \
	$(OUTDIR)/ts_profile.beam \
	$(OUTDIR)/ts_stats.beam \
	$(OUTDIR)/ts_user_server.beam \
	$(OUTDIR)/ts_msg_server.beam  \
	$(OUTDIR)/ts_req_server.beam \
	$(OUTDIR)/ts_timer.beam \
	$(OUTDIR)/ts_client_sup.beam \
	$(OUTDIR)/ts_sup.beam \
	$(OUTDIR)/jabber_common.beam \
	$(OUTDIR)/jabber_dynamic.beam \
	$(OUTDIR)/jabber_roster.beam \
	$(OUTDIR)/jabber_online.beam \
	$(OUTDIR)/jabber_offline.beam \
	$(OUTDIR)/jabber_auth.beam \
	$(OUTDIR)/jabber_unique.beam \
	$(OUTDIR)/jabber_register.beam \
	$(OUTDIR)/make_boot.beam 

all:	tsunami.boot

tarball: 
	mkdir -p $(TARDIR)
	tar zcf tmp.tgz src/*.erl src/*.src include/*.hrl doc/*.txt LISEZMOI README CONTRIBUTORS COPYING src/idx-tsunami.pl.src idx-tsunamirc TODO Makefile vsn.mk src/analyse_msg.pl.src
	tar -C $(TARDIR) -zxf tmp.tgz
	mkdir $(TARDIR)/ebin
	tar zvcf  idx-tsunami-$(VSN).tar.gz $(TARDIR)
	rm -fr $(TARDIR)
	rm -fr tmp.tgz


clean:
	rm -f $(OBJS) tsunami.boot tsunami.script ebin/tsunami.app ebin/tsunami.rel ebin/idx-tsunami.pl ebin/analyse_msg.pl

tsunami.boot:	 $(OBJS) $(UTILS) src/tsunami.rel.src src/tsunami.app.src src/analyse_msg.pl.src src/idx-tsunami.pl.src
	sed -e 's;%VSN%;$(VSN);' ./src/tsunami.app.src > ./ebin/tsunami.app
	sed -e 's;%VSN%;$(VSN);' ./src/tsunami.rel.src > ./ebin/tsunami.rel
	sed -e 's;%VSN%;$(VSN);' ./src/idx-tsunami.pl.src > ./ebin/idx-tsunami.pl
	sed -e 's;%VSN%;$(VSN);' ./src/analyse_msg.pl.src > ./ebin/analyse_msg.pl
	erl -noshell $(PA) ./src -s make_boot make_boot tsunami

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
	install -m 0644 idx-tsunamirc $(DESTDIR)/$(prefix)/etc
	install ebin/idx-tsunami.pl $(DESTDIR)/${prefix}/bin
	install ebin/analyse_msg.pl $(DESTDIR)/${prefix}/bin
	mkdir -p $(DESTDIR)/$(prefix)
	mkdir -p $(DESTDIR)/$(prefix)/erlang
	mkdir -p $(DESTDIR)/$(prefix)/erlang/tsunami-$(VSN)
	mkdir -p $(DESTDIR)/$(prefix)/erlang/tsunami-$(VSN)/ebin
	install $(OBJS) $(DESTDIR)/$(prefix)/erlang/tsunami-$(VSN)/ebin

