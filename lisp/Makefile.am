# -*- Makefile -*-
SOURCES    = sdicf.el sdicf-client.el sdic-compat.el sdic-gene.el stem.el
EXTRA_DIST = sdic.el.in
DISTCLEANFILES = sdic.el
lisp_LISP = $(SOURCES) sdic.el  # sdic.el must be in this
ELCFILES  = $(lisp_LISP:.el=.elc)

$(ELCFILES): $(lisp_LISP)
	$(EMACS) -batch -q -no-site-file -l elisp-comp -f batch-byte-compile $(lisp_LISP)
