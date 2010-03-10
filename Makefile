
at_franz = $(shell if test -d /fi/cl/8.2/acl; then echo t; else echo nil; fi)

Makefile_local = \
	$(shell if test -f Makefile.local; then echo Makefile.local; fi)

ifneq ($(Makefile_local),)
include $(Makefile_local)
endif

ARCH ?= $(shell uname -i)

ifeq ($(at_franz),t)
LISP ?= /fi/cl/8.2/bin/$(shell if [ $(ARCH) = x86_64 ]; then echo mlisp-64; else echo mlisp; fi)
else
endif

LISP ?= mlisp

installdir?=$(RPM_BUILD_ROOT)/usr/fi

installsubdir=$(installdir)/mailfilter

libfiles=emailaddr.cl lex.cl load.cl parse.cl spool.cl subs.cl

default: clean all

ifeq ($(at_franz),t)
ALL_EXTRA = repo_check
endif

all: $(ALL_EXTRA) mailstatus/mailstatus incfilter/incfilter \
	folderfilter/folderfilter

ifeq ($(at_franz),t)
repo_check: FORCE
	@if test ! -d fi-apps-common; then \
	    git clone git:/repo/git/fi-apps-common; \
	fi
endif

mailstatus/mailstatus: $(libfiles) mailstatus.cl
	rm -fr mailstatus/
	$(LISP) -L load.cl -e '(build "mailstatus")' -kill

incfilter/incfilter: $(libfiles) incfilter.cl
	rm -fr incfilter/
	$(LISP) -L load.cl -e '(build "incfilter")' -kill

folderfilter/folderfilter: $(libfiles) folderfilter.cl
	rm -fr folderfilter/
	$(LISP) -L load.cl -e '(build "folderfilter")' -kill

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
	rm -fr BUILD BUILDROOT SRPMS RPMS SPECS
	rm -fr *.fasl *~ incfilter/ mailstatus/ folderfilter/ *.out

realclean: clean
	rm -fr BUILD RPMS

name := mailfilter

version := $(shell grep 'mailfilter-version' version.cl | sed -e 's,.*"\(.*\)".*,\1,')
tardir := $(name)-$(version)
tarball := $(name)-$(version).tar.gz
ifeq ($(FI_APPS_COMMON),t)
release ?= $(shell . fi-apps-common/rpm-utils.sh && \
	rpm_next_release_number \
	   /net/$(REPOHOST)$(REPOBASE)/$(ARCH)/mailfilter-$(version)-*.$(ARCH).rpm)
else
release ?= 1
endif

files := Makefile *.cl 

tarball:
	mkdir $(tardir)
	cp $(files) $(tardir)
	if test -f Makefile.local; then cp Makefile.local $(tardir); fi
	tar zcf $(tarball) $(tardir)
	rm -fr $(tardir)

SIGN ?= --sign

rpm:    tarball
	mkdir -p BUILD RPMS/$(ARCH) SRPMS
	rpmbuild --define "_sourcedir $(CURDIR)" \
		--define "_topdir $(CURDIR)" \
		--define "version $(version)" \
		--define "release $(release)" \
		--target $(ARCH) $(SIGN) \
		-ba $(name).spec
	rm $(tarball)

REMOVE_PREVIOUS_VERSIONS ?= no
REPOHOST                 ?= fs1
REPOBASE                 ?= /storage1/franz/common

REPODIR=$(REPOBASE)/$(ARCH)

install-repo:
ifeq ($(REMOVE_PREVIOUS_VERSIONS),yes)
	ssh root@$(REPOHOST) "rm -f $(REPODIR)/$(name)-*"
endif
	scp RPMS/$(ARCH)/$(name)-$(version)-*.rpm root@$(REPOHOST):$(REPODIR)
	ssh root@$(REPOHOST) "cd $(REPODIR) && createrepo -s sha --update -q ."

FORCE:
