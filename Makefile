# $Id: Makefile,v 1.8 2004/11/30 21:31:53 layer Exp $

lisp=/fi/cl/7.0/bin/$(shell /fi/cl/7.0/acl/osname.sh)/mlisp
installdir=/usr/fi
installsubdir=$(installdir)/mailfilter

libfiles=emailaddr.cl lex.cl load.cl parse.cl spool.cl subs.cl

all: mailstatus/mailstatus incfilter/incfilter folderfilter/folderfilter

mailstatus/mailstatus: $(libfiles) mailstatus.cl
	rm -fr mailstatus/
	$(lisp) -L load.cl -e '(build "mailstatus")' -kill

incfilter/incfilter: $(libfiles) incfilter.cl
	rm -fr incfilter/
	$(lisp) -L load.cl -e '(build "incfilter")' -kill

folderfilter/folderfilter: $(libfiles) folderfilter.cl
	rm -fr folderfilter/
	$(lisp) -L load.cl -e '(build "folderfilter")' -kill

install: all
	mkdir -p $(installdir)
	rm -fr $(installsubdir).old
	mv $(installsubdir) $(installsubdir).old
	mkdir -p $(installsubdir)
	-rm -fr $(installsubdir)/mailstatus 
	cp -p mailstatus/* $(installsubdir)
	-rm -fr $(installsubdir)/incfilter
	cp -p incfilter/* $(installsubdir)
	-rm -fr $(installsubdir)/folderfilder
	cp -p folderfilter/* $(installsubdir)
	cd $(installdir) && ln -sf $(installsubdir)/mailstatus 
	cd $(installdir) && ln -sf $(installsubdir)/incfilter
	cd $(installdir) && ln -sf $(installsubdir)/folderfilter

uninstall:
	rm -fr $installsubdir

clean: 
	rm -fr *.fasl *~ incfilter/ mailstatus/ folderfilter/ *.out


