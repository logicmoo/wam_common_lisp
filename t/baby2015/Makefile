ROOTNAME = Babylon
VERSION  = 2.3
PACKAGE  = $(ROOTNAME)-$(VERSION)

SUBDIRS  = kernel mac mcs samples tty

all:
	@echo "== START LISP AND LOAD THE make.cl FILE =="

install:
	@echo "== START LISP AND LOAD THE make.cl FILE =="

clean:
	rm -f $(PACKAGE).tar $(PACKAGE).tar.Z
	rm -f *.dribble
	rm -f `find . -name '*.cl~' -print`
	rm -f babylon.mem
	rm -f `find . -name '*.lib' -print`
	rm -f `find . -name '*.fas' -print`
	rm -f babylon
	rm -f `find . -name '*.fasl' -print`
	rm -f babylon.core
	rm -f `find . -name '*.cmu' -print`
	rm -f babylon.kcl
	rm -f `find . -name '*.o' -print`
	rm -f `find . -name '*.bin' -print`


