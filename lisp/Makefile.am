SOURCES    = sdicf.el sdicf-client.el sdic-compat.el sdic-gene.el stem.el
EXTRA_DIST = sdic.el.in lp.el
DISTCLEANFILES = sdic.el
lisp_LISP = $(SOURCES) sdic.el  # sdic.el must be in this
ELCFILES = $(lisp_LISP:.el=.elc)

$(ELCFILES): $(lisp_LISP)
	$(EMACS) -batch -l lp.el -f batch-byte-compile $(lisp_LISP)
