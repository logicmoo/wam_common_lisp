# LISP=gcl
# LISP=../unixport/saved_ansi_gcl
# LISP=sbcl --noinform
# LISP=~/sbcl/src/runtime/sbcl --core ~/sbcl/output/sbcl.core --noinform
# LISP=clisp -ansi -q
# LISP=abcl
LISP=ecl
# LISP=/usr/local/lib/LispWorks/nongraphic-lispworks-4450
# LISP=acl

MAKE=make

test:
	@rm -rf sandbox/scratch
	cat doit.lsp | $(LISP) | tee test.out

test-symbols:
	(cat doit1.lsp ; echo "(load \"symbols/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-symbols.out

test-eval-and-compile:
	(cat doit1.lsp ; echo "(load \"eval-and-compile/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-eval-and-compile.out

test-data-and-control-flow:
	(cat doit1.lsp ; echo "(load \"data-and-control-flow/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-data-and-control-flow.out

test-iteration:
	(cat doit1.lsp ; echo "(load \"iteration/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-iteration.out

test-objects:
	(cat doit1.lsp ; echo "(load \"objects/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-objects.out

test-conditions:
	(cat doit1.lsp ; echo "(load \"conditions/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-conditions.out

test-cons:
	(cat doit1.lsp ; echo "(load \"cons/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-cons.out

test-arrays:
	(cat doit1.lsp ; echo "(load \"arrays/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-arrays.out

test-hash-tables:
	(cat doit1.lsp ; echo "(load \"hash-tables/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-hash-tables.out

test-packages:
	(cat doit1.lsp ; echo "(load \"packages/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-packages.out

test-numbers:
	(cat doit1.lsp ; echo "(load \"numbers/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-numbers.out

test-sequences:
	(cat doit1.lsp ; echo "(load \"sequences/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-sequences.out

test-structures:
	(cat doit1.lsp ; echo "(load \"structures/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-structures.out

test-types-and-class:
	(cat doit1.lsp ; echo "(load \"types-and-class/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-types-and-class.out

test-strings:
	(cat doit1.lsp ; echo "(load \"strings/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-strings.out

test-characters:
	(cat doit1.lsp ; echo "(load \"characters/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-characters.out

test-pathnames:
	(cat doit1.lsp ; echo "(load \"pathnames/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-pathnames.out

test-files:
	(cat doit1.lsp ; echo "(load \"files/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-files.out

test-streams:
	(cat doit1.lsp ; echo "(load \"streams/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-streams.out

test-printer:
	(cat doit1.lsp ; echo "(load \"printer/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-printer.out

test-reader:
	(cat doit1.lsp ; echo "(load \"reader/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-reader.out

test-system-construction:
	(cat doit1.lsp ; echo "(load \"system-construction/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-system-construction.out

test-environment:
	(cat doit1.lsp ; echo "(load \"environment/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-environment.out

test-misc:
	(cat doit1.lsp ; echo "(load \"misc/load.lsp\")"; cat doit2.lsp) | $(LISP) | tee test-misc.out

test-all: test-symbols test-eval-and-compile test-data-and-control-flow test-iteration test-objects \
         test-conditions test-cons test-arrays test-hash-tables test-packages test-numbers \
         test-sequences test-structures test-types-and-class test-strings test-characters test-pathnames \
         test-files test-streams test-printer test-reader test-system-construction test-environment \
         test-misc

test-compiled:
	@rm -rf scratch
	echo "(load \"compileit.lsp\")" | $(LISP) | tee test.out

test-unixport:
	echo "(load \"doit.lsp\")" | ../unixport/saved_ansi_gcl | tee test.out

random-test:
	(echo "(progn #+gcl (setq compiler::*cc* \"gcc -c -DVOL=volatile -fsigned-char -pipe \") \
		(setq *load-verbose* nil) \
		(let* ((*standard-output* (make-broadcast-stream)) \
		     (*error-output* *standard-output*)) \
		(load \"gclload1.lsp\") \
		(funcall (symbol-function 'compile-and-load) \"random-int-form.lsp\")))  \
	      (in-package :cl-test) \
	      (let ((x (cl-test::test-random-integer-forms 1000 3 1000 :random-size t :random-nvars t))) \
		(setq x (cl-test::prune-results x)) \
		(with-open-file (*standard-output* \"failures.lsp\" \
		   :direction :output \
		   :if-exists :append \
		   :if-does-not-exist :create) \
		  (mapc #'print x))) \
                #+allegro (excl::exit) \
		; extra quits added to avoid being trapped in debugger in some lisps \
	        (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit)") | $(LISP)
	rm -f gazonk*

rt_1000_8:
		echo "(load \"gclload1.lsp\") \
		(compile-and-load \"random-int-form.lsp\")  \
		(in-package :cl-test) (loop-random-int-forms 1000 8)" | $(LISP)


clean:
	@rm -f */*.{out,fas,cls,fasl,o,so,~,fn,x86f,ufsl,abcl,lib}
	@rm -f */*/*.{out,fas,cls,fasl,o,so,~,fn,x86f,ufsl,abcl,lib}
	@rm -f test*.out *.cls *.fasl *.o *.so *~ *.fn *.x86f *.fasl *.ufsl *.abcl *.fas *.lib \#*\#
	@rm -f *.dfsl *.d64fsl
	@(cd beyond-ansi; $(MAKE) clean)
	@rm -rf sandbox/scratch/ sandbox/scratch.txt
	@rm -f foo.txt foo.lsp foo.dat
	@rm -f tmp.txt tmp.dat tmp2.dat temp.dat
	@rm -f gazonk* out.class
	@rm -rf TMP/
	@rm -f "CLTEST:file-that-was-renamed.txt" file-that-was-renamed.txt
	@rm -f compile-file-test-lp.lsp compile-file-test-lp.out ldtest.lsp
