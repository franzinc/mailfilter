# $Id: Makefile,v 1.16 2009/02/03 21:27:42 elliott Exp $

Makefile_local = \
	$(shell if test -f Makefile.local; then echo Makefile.local; fi)

ifneq ($(Makefile_local),)
include $(Makefile_local)
endif

ARCH=$(shell uname -i)

ifeq ($(ARCH),x86_64)
lisp?=/fi/cl/8.1/bin/mlisp-64
else
lisp?=/fi/cl/8.1/bin/mlisp
endif

installdir?=$(RPM_BUILD_ROOT)/usr/fi

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
	if test -f Makefile.local; then cp Makefile.local $(tardir); fi
	tar zcf $(tarball) $(tardir)
	rm -fr $(tardir)

rpm:    tarball
	mkdir -p BUILD RPMS/$(ARCH) SRPMS
	rpmbuild --define "_sourcedir $(CURDIR)" \
		--define "_topdir $(CURDIR)" \
		--define "version $(version)" \
		--define "release $(release)" \
		--sign \
		-ba $(name).spec
	rm $(tarball)

REPOHOST                 ?= fs1
REPOBASE                 ?= /storage1/franz/common/$(ARCH)
REMOVE_PREVIOUS_VERSIONS ?= yes

REPODIR=$(REPOBASE)/$(ARCH)

install-repo:
ifeq ($(REMOVE_PREVIOUS_VERSIONS),yes)
	ssh root@$(REPOHOST) "rm -f $(REPODIR)/$(name)-*"
endif
	scp RPMS/$(ARCH)/$(name)-$(version)-*.rpm root@$(REPOHOST):$(REPODIR)
	ssh root@$(REPOHOST) "cd $(REPODIR) && createrepo --update -q ."
