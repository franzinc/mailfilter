# $Id: Makefile,v 1.13 2007/05/25 00:34:53 dancy Exp $

at_franz = $(shell if test -d /fi/cl/8.1.beta/acl; then echo t; else echo nil; fi)

ARCH=$(shell uname -i)

ifeq ($(at_franz),t)
mlisp=$(shell if [ $(ARCH) = x86_64 ]; then echo mlisp-64; else echo mlisp; fi)
lisp=/fi/cl/8.1.beta/bin/$(mlisp)
installdir=$(RPM_BUILD_ROOT)/usr/fi
else
lisp=mlisp
installdir=$(RPM_BUILD_ROOT)/usr/local
endif

installsubdir=$(installdir)/mailfilter

libfiles=emailaddr.cl lex.cl load.cl parse.cl spool.cl subs.cl

default: clean all

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
	-mv $(installsubdir) $(installsubdir).old
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

realclean: clean
	rm -fr BUILD RPMS

name := mailfilter

version := $(shell grep 'mailfilter-version' version.cl | sed -e 's,.*"\(.*\)".*,\1,')
tardir := $(name)-$(version)
tarball := $(name)-$(version).tar.gz
release ?= 1

files := Makefile *.cl 

tarball:
	mkdir $(tardir)
	cp $(files) $(tardir)
	tar zcf $(tarball) $(tardir)
	rm -fr $(tardir)

rpm:    tarball
	mkdir -p BUILD RPMS/i386 SRPMS
	rpmbuild --define "_sourcedir $(CURDIR)" \
		--define "_topdir $(CURDIR)" \
		--define "version $(version)" \
		--define "release $(release)" \
		--sign \
		-ba $(name).spec
	rm $(tarball)


REPOHOST=fs1
REPODIR=/storage1/franz/$(ARCH)


install-repo:
	ssh root@$(REPOHOST) "rm -f $(REPODIR)/$(name)-*"
	scp RPMS/$(ARCH)/$(name)-$(version)-*.rpm root@$(REPOHOST):$(REPODIR)
	ssh root@$(REPOHOST) "createrepo -q $(REPODIR)"
