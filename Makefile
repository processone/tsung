# Build the .beam erlang VM files

include vsn.mk
include include.mk

ERL_COMPILER_OPTIONS="[warn_unused_vars]"
export ERL_COMPILER_OPTIONS

ifeq ($(TYPE),debug)
OPT =+debug_info -DDEBUG
else 
 ifeq ($(TYPE),test)
   OPT:=+export_all
  else
   OPT = 
  endif	
endif
INC = ./include
CC  = erlc
ERL = erl
SED = $(shell which sed)

ESRC = ./src
EBIN = ./ebin

VERSION = $(IDX-TSUNAMI_VSN)

# installation path
BINDIR    = $(bindir)
LIBDIR    = $(libdir)/idx-tsunami/bin/
CONFDIR   = $(datadir)/doc/idx-tsunami/examples
SHARE_DIR = $(datadir)/idx-tsunami/
TEMPLATES_DIR = $(SHARE_DIR)/templates
MAN_DIR   = $(datadir)/man/man1/
DOC_DIR   = $(datadir)/doc/idx-tsunami

ERLANG_LIB_DIR = $(libdir)/erlang/lib

PACKAGE = idx-tsunami
APPLICATION = tsunami
CONTROLLER_APPLICATION = tsunami_controller
RECORDER_APPLICATION = tsunami_recorder

RECORDER_TARGETDIR = $(ERLANG_LIB_DIR)/$(RECORDER_APPLICATION)-$(VERSION)
CONTROLLER_TARGETDIR = $(ERLANG_LIB_DIR)/$(CONTROLLER_APPLICATION)-$(VERSION)
TARGETDIR = $(ERLANG_LIB_DIR)/$(APPLICATION)-$(VERSION)

TEMPLATES = $(wildcard $(ESRC)/templates/*.thtml)
TMP       = $(wildcard *~) $(wildcard src/*~) $(wildcard inc/*~)
INC_FILES = $(wildcard $(INC)/*.hrl)
SRC       = $(wildcard $(ESRC)/$(APPLICATION)/*.erl)
CONTROLLER_SRC  = $(wildcard $(ESRC)/$(CONTROLLER_APPLICATION)/*.erl)
RECORDER_SRC    = $(wildcard $(ESRC)/$(RECORDER_APPLICATION)/*.erl)
CONFFILE = idx-tsunami.xml
CONFFILE_SRC = idx-tsunami.xml.in
DTD = idx-tsunami-1.0.dtd
USERMANUAL = doc/user_manual.html  doc/IDXDOC.css
USERMANUAL_IMG = $(wildcard doc/images/*.png)
USERMANUAL_SRC = doc/user_manual.tex

TARGET   = $(addsuffix .beam, $(basename \
             $(addprefix $(EBIN)/, $(notdir $(SRC)))))
CONTROLLER_TARGET   = $(addsuffix .beam, $(basename \
             $(addprefix $(EBIN)/, $(notdir $(CONTROLLER_SRC)))))
RECORDER_TARGET   = $(addsuffix .beam, $(basename \
             $(addprefix $(EBIN)/, $(notdir $(RECORDER_SRC)))))
EMAKE    = $(addsuffix \'., $(addprefix \'../, $(SRC)))
DEBIAN    = debian/changelog debian/control debian/compat debian/copyright debian/docs debian/idx-tsunami.dirs debian/rules

SRC_APPFILES   = $(ESRC)/$(APPLICATION)/$(APPLICATION).app.src $(ESRC)/$(APPLICATION)/$(APPLICATION).rel.src
CONTROLLER_SRC_APPFILES   = $(ESRC)/$(CONTROLLER_APPLICATION)/$(CONTROLLER_APPLICATION).app.src $(ESRC)/$(CONTROLLER_APPLICATION)/$(CONTROLLER_APPLICATION).rel.src
RECORDER_SRC_APPFILES   = $(ESRC)/$(RECORDER_APPLICATION)/$(RECORDER_APPLICATION).app.src $(ESRC)/$(RECORDER_APPLICATION)/$(RECORDER_APPLICATION).rel.src
TGT_APPFILES_E = $(EBIN)/$(APPLICATION).app
CONTROLLER_TGT_APPFILES_E = $(EBIN)/$(CONTROLLER_APPLICATION).app
RECORDER_TGT_APPFILES_E = $(EBIN)/$(RECORDER_APPLICATION).app
TGT_APPFILES_P = priv/$(APPLICATION)*
RECORDER_TGT_APPFILES_P = priv/$(RECORDER_APPLICATION)*
CONTROLLER_TGT_APPFILES_P = priv/$(CONTROLLER_APPLICATION)*

SCRIPT   = $(BINDIR)/idx-tsunami
PWD = $(shell pwd)
BUILD_OPTIONS =	'[{systools, \
        [{variables,[ \
         {"TSUNAMIPATH", "$(PWD)/temp/"}] \
        }]}, \
	    {sh_script, none}, \
        {make_app, true }, {make_rel, true}].'
BUILD_OPTIONS_FILE = ./BUILD_OPTIONS 

.PHONY: doc

idx-tsunami: $(TARGET)  $(RECORDER_TARGET) $(CONTROLLER_TARGET) 

all: clean idx-tsunami

# used to generate the erlang Emakefile
emake:
	@echo $(EMAKE) | tr -s ' ' '\n' > ebin/Emakefile

debug:
	$(MAKE) TYPE=debug

test:
	$(MAKE) TYPE=test

rpm:	release idx-tsunami.spec
	rpmbuild -ta $(PACKAGE)-$(VERSION).tar.gz

deb:
	fakeroot debian/rules clean
	debian/rules build
	fakeroot debian/rules binary

clean:
	-cd priv && rm -f $(shell ls priv | grep -v builder\.erl) && cd ..
	-rm -f $(TARGET) $(TMP) $(BUILD_OPTIONS_FILE) builder.beam
	-rm -f $(TGT_APPFILES) idx-tsunami.sh analyse_msg.pl
	-rm -f ebin/*.beam 
#	-make -C doc clean

install: doc boot  idx-tsunami.sh analyse_msg.pl install_recorder install_controller $(CONFFILE)
	-rm -f $(TMP)

	install -d $(TARGETDIR)/priv
	install -d $(TARGETDIR)/ebin
	install -d $(TARGETDIR)/src
	install -d $(TARGETDIR)/include
	install -d $(LIBDIR)/
	install -d $(BINDIR)/

	cp $(INC_FILES) $(TARGETDIR)/include
	cp $(TARGET) $(TARGETDIR)/ebin

	cp $(TGT_APPFILES_E) $(TARGETDIR)/ebin
	cp $(TGT_APPFILES_P) $(TARGETDIR)/priv

	cp $(SRC) $(SRC_APPFILES) $(TARGETDIR)/src

# install the man page & user's manual
	install -d $(MAN_DIR)
	install doc/idx-tsunami.1 $(MAN_DIR)
	install -d $(DOC_DIR)/images
	install $(USERMANUAL) $(DOC_DIR)
	install $(USERMANUAL_IMG) $(DOC_DIR)/images

# create startup script
	cp idx-tsunami.sh $(SCRIPT)
	install analyse_msg.pl $(LIBDIR)/analyse_msg.pl
	chmod +x $(SCRIPT)

# 
	mkdir -p $(CONFDIR)
	cp $(CONFFILE) $(CONFDIR)

	mkdir -p $(TEMPLATES_DIR)
	cp $(TEMPLATES) $(TEMPLATES_DIR)
	cp $(DTD) $(SHARE_DIR)

install_recorder:
	install -d $(RECORDER_TARGETDIR)/priv
	install -d $(RECORDER_TARGETDIR)/ebin
	install -d $(RECORDER_TARGETDIR)/src
	install -d $(RECORDER_TARGETDIR)/include

	cp $(INC_FILES) $(RECORDER_TARGETDIR)/include
	cp $(RECORDER_TARGET) $(RECORDER_TARGETDIR)/ebin

	cp $(RECORDER_TGT_APPFILES_E) $(RECORDER_TARGETDIR)/ebin
	cp $(RECORDER_TGT_APPFILES_P) $(RECORDER_TARGETDIR)/priv

	cp $(RECORDER_SRC) $(RECORDER_SRC_APPFILES) $(RECORDER_TARGETDIR)/src

install_controller:
	install -d $(CONTROLLER_TARGETDIR)/priv
	install -d $(CONTROLLER_TARGETDIR)/ebin
	install -d $(CONTROLLER_TARGETDIR)/src
	install -d $(CONTROLLER_TARGETDIR)/include
	cp $(INC_FILES) $(CONTROLLER_TARGETDIR)/include
	cp $(CONTROLLER_TARGET) $(CONTROLLER_TARGETDIR)/ebin

	cp $(CONTROLLER_TGT_APPFILES_E) $(CONTROLLER_TARGETDIR)/ebin
	cp $(CONTROLLER_TGT_APPFILES_P) $(CONTROLLER_TARGETDIR)/priv

	cp $(CONTROLLER_SRC) $(CONTROLLER_SRC_APPFILES) $(CONTROLLER_TARGETDIR)/src

uninstall:
	rm -rf $(TARGETDIR) $(SCRIPT)

boot: idx-tsunami priv/tsunami.boot priv/tsunami_recorder.boot priv/tsunami_controller.boot

priv/tsunami.boot: builder.beam  $(SRC_APPFILES)
# use builder to make boot file
	@rm -rf temp
	@mkdir -p temp/lib/$(APPLICATION)-$(VERSION)
	@ln -sf $(PWD)/ebin temp/lib/$(APPLICATION)-$(VERSION)/ebin
	@ln -sf $(PWD)/src/$(APPLICATION) temp/lib/$(APPLICATION)-$(VERSION)/src
	@ln -sf $(PWD)/include temp/lib/$(APPLICATION)-$(VERSION)/include
	@ln -sf $(PWD)/priv temp/lib/$(APPLICATION)-$(VERSION)/priv
	@ln -sf $(PWD)/builder.beam temp/lib/$(APPLICATION)-$(VERSION)/
	@ln -sf $(PWD) temp/lib/$(APPLICATION)-$(VERSION)
	@(cd temp/lib/$(APPLICATION)-$(VERSION) \
	 && echo $(BUILD_OPTIONS) > $(BUILD_OPTIONS_FILE) \
	 && erl -s builder go -s init stop \
	)
	@rm -rf temp

priv/tsunami_controller.boot: builder.beam $(CONTROLLER_SRC_APPFILES)
# use builder to make boot file
	@rm -rf temp
	@mkdir -p temp/lib/$(CONTROLLER_APPLICATION)-$(VERSION)
	@ln -sf $(PWD)/ebin temp/lib/$(CONTROLLER_APPLICATION)-$(VERSION)/ebin
	@ln -sf $(PWD)/src/$(CONTROLLER_APPLICATION) temp/lib/$(CONTROLLER_APPLICATION)-$(VERSION)/src
	@ln -sf $(PWD)/include temp/lib/$(CONTROLLER_APPLICATION)-$(VERSION)/include
	@ln -sf $(PWD)/priv temp/lib/$(CONTROLLER_APPLICATION)-$(VERSION)/priv
	@ln -sf $(PWD)/builder.beam temp/lib/$(CONTROLLER_APPLICATION)-$(VERSION)/
	@(cd temp/lib/$(CONTROLLER_APPLICATION)-$(VERSION) \
	 && echo $(BUILD_OPTIONS) > $(BUILD_OPTIONS_FILE) \
	 && erl -s builder go -s init stop \
	)
	@rm -rf temp

priv/tsunami_recorder.boot: builder.beam $(RECORDER_SRC_APPFILES)
# use builder to make boot file
	@rm -rf temp
	@mkdir -p temp/lib/$(RECORDER_APPLICATION)-$(VERSION)
	@ln -sf $(PWD)/ebin temp/lib/$(RECORDER_APPLICATION)-$(VERSION)/ebin
	@ln -sf $(PWD)/src/$(RECORDER_APPLICATION) temp/lib/$(RECORDER_APPLICATION)-$(VERSION)/src
	@ln -sf $(PWD)/include temp/lib/$(RECORDER_APPLICATION)-$(VERSION)/include
	@ln -sf $(PWD)/priv temp/lib/$(RECORDER_APPLICATION)-$(VERSION)/priv
	@ln -sf $(PWD)/builder.beam temp/lib/$(RECORDER_APPLICATION)-$(VERSION)/
	@(cd temp/lib/$(RECORDER_APPLICATION)-$(VERSION) \
	 && echo $(BUILD_OPTIONS) > $(BUILD_OPTIONS_FILE) \
	 && erl -noshell -s builder go -s init stop \
	)
	@rm -rf temp

doc: 
	make -C doc

release:
	rm -fr $(PACKAGE)-$(VERSION)
	mkdir -p $(PACKAGE)-$(VERSION)
	tar zcf tmp.tgz $(SRC) $(SRC_APPFILES) $(INC_FILES) \
		$(CONTROLLER_SRC) $(CONTROLLER_SRC_APPFILES) \
		$(RECORDER_SRC) $(RECORDER_SRC_APPFILES) $(TEMPLATES) \
		 doc/*.txt doc/*.fig doc/*.png doc/Makefile doc/*.sgml \
		$(USERMANUAL) $(USERMANUAL_SRC) $(USERMANUAL_IMG) $(DTD) \
		COPYING README LISEZMOI TODO $(CONFFILE_SRC) Makefile \
		priv/builder.erl idx-tsunami.sh.in vsn.mk \
		$(DEBIAN) src/analyse_msg.pl.src CONTRIBUTORS CHANGES \
		configure configure.in config.guess config.sub include.mk.in \
		install-sh idx-tsunami.spec
	tar -C $(PACKAGE)-$(VERSION) -zxf tmp.tgz
	mkdir $(PACKAGE)-$(VERSION)/ebin
	tar zvcf  $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	rm -fr $(PACKAGE)-$(VERSION)
	rm -fr tmp.tgz


builder.beam: priv/builder.erl 
	$(CC) $(OPT) -I $(INC) $<

ebin/%.beam: src/$(APPLICATION)/%.erl $(INC_FILES)
	$(CC) $(OPT) -I $(INC) -o ebin $<

ebin/%.beam: src/$(RECORDER_APPLICATION)/%.erl  $(INC_FILES)
	$(CC) $(OPT) -I $(INC) -o ebin $<

ebin/%.beam: src/$(CONTROLLER_APPLICATION)/%.erl  $(INC_FILES)
	$(CC) $(OPT) -I $(INC) -o ebin $<

analyse_msg.pl: src/analyse_msg.pl.src Makefile
	$(SED) -e 's;%VERSION%;$(VERSION);g' < $<  > $@

idx-tsunami.sh: idx-tsunami.sh.in include.mk Makefile
	@$(SED) \
		-e 's;%INSTALL_DIR%;${raw_erlang_prefix};g' \
		-e 's;${DESTDIR};;g' \
		-e 's;CONFIG_DIR%;${CONFIG_DIR};g' \
		-e 's;%VERSION%;${VERSION};g' < $< > $@

idx-tsunami.xml: idx-tsunami.xml.in include.mk Makefile
	@$(SED) \
		-e 's;%INSTALL_DIR%;${SHARE_DIR};g' < $< > $@

%:%.sh
# Override makefile default implicit rule
