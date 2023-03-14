infile := README.org
outfile := grank.el
all:
	@emacs -q --batch --eval "(require 'org)" \
		--eval '(org-babel-tangle-file "${infile}" "${outfile}")'
	@echo "oopah loompah dibidi doo"
