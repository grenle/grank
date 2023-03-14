orgfile := README.org
codefile := grank.el
testfile := ../test/grank-test.el
all: tangle test
tangle:
	@emacs -q --batch --eval "(require 'org)" \
		--eval '(org-babel-tangle-file "${orgfile}" "${codefile}")'
	@echo "oopah loompah dibidi doo"
test:
	@emacs -batch -l "${codefile}" -l "${testfile}" \
	-f ert-run-tests-batch-and-exit
