root     := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
pkgname  := $(lastword $(subst /, ,$(dir $(root))))
orgfile  := ${root}/src/README.org
codefile := ${root}/$(pkgname).el
testfile := ${root}/test/$(pkgname)-test.el
tanglexp := (org-babel-tangle-file "${orgfile}" "${codefile}")

all: tangle check

tangle:
	@emacs -q --batch \
	--eval "(require 'org)" \
	--eval '${tanglexp}'

check:
	@emacs -batch \
	-l "${codefile}" -l "${testfile}" \
	-f ert-run-tests-batch-and-exit
