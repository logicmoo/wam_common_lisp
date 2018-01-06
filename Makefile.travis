version:=$(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
#packfile=wam-cl-$(version).tgz
#remote=www-data@packs.rlaanemets.com:/sites/packs.rlaanemets.com/public/simple-template

#package: test
#	tar cvzf $(packfile) prolog t pack.pl README.md LICENSE

test:
	swipl -s t/travis.pl -g run_tests,halt -t 'halt(1)'

run:
	swipl prolog/wam_cl/repl.pl

doc:
	swipl -q -t 'doc_save(prolog/wam_cl, [doc_root(doc),format(html),title(wam_common_lisp),if(true),recursive(true)])'

#upload: doc package
#	scp $(packfile) $(remote)/$(packfile)
#	rsync -avz -e ssh doc $(remote)

.PHONY: test build doc

