MANUAL	= user
EMACS	= emacs

all	: ecl.info

$(MANUAL).dvi	: $(MANUAL).tex clisp.sty ecl.sty
	latex $(MANUAL).dvi

ecl.info:	$(MANUAL).tex makeinfo.el
	sed -e 	"s+MANUAL+$(MANUAL)+" makeinfo.el > make.el
	$(EMACS) -batch -u -l make.el
	rm -f make.el

install:	ecl.info
	$(INSTALLDATA) ecl.info* $(INFODIR)

clean:	
	rm -f ecl.info* *~ #~ *.el
