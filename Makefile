lisp=/storage1/acl/mlisp

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

clean: 
	rm -fr *.fasl *~ incfilter/ mailstatus/ folderfilter/


