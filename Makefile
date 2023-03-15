root     := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
pkgname  := $(lastword $(subst /, ,$(dir $(root))))
orgfile  := ${root}/src/README.org
codefile := ${root}/$(pkgname)-code.el
pkgfile  := ${root}/$(pkgname).el
testfile := ${root}/test/$(pkgname)-test.el
tanglexp := (org-babel-tangle-file "${orgfile}" "${codefile}")
summary  := Mode aware URL yanking
lexbind  := -*- lexical-binding: t; -*-

all: tangle check

tangle:
	@emacs -q --batch \
	--eval "(require 'org)" \
	--eval '${tanglexp}'
	@printf ";;; ${pkgname}.el --- ${summary} ${lexbind}\n\n" > ${pkgfile}
	@printf ";;; Commentary:\n;;; Please see src/README.org for details\n\n" >> ${pkgfile}
	@printf ";;; Code:\n\n" >> ${pkgfile}
	@cat ${codefile} >> ${pkgfile}
	@printf "\n\n(provide '${pkgname})\n;;; ${pkgname}.el ends here" >> ${pkgfile}

check:
	@emacs -batch \
	-l "${pkgfile}" -l "${testfile}" \
	-f ert-run-tests-batch-and-exit
