# $Id$

MANPAGE  = idx-tsunami.1.sgml

all: man

man: $(MANPAGE)
	docbook2man $< >/dev/null 2>&1

clean:
	-rm -f *~
	-rm -f idx-tsunami.1 manpage.*
