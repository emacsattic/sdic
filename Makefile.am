# -*- Makefile -*-
#
# $Id$
#
EXTRA_DIST = install.el texi/sdic.texi contrib/edict.perl contrib/eijirou.perl contrib/gene.perl contrib/jgene.perl
DISTCLEANFILES = install.el texi/sdic.info
SUBDIRS = lisp

dict: @EIWA_DICT@ @WAEI_DICT@
# info_TEXINFOS = sdic.texi     # this requires makeinfo

install-dict:
	$(mkinstalldirs) @DICTDIR@
	$(INSTALL_DATA) @EIWA_DICT@ @DICTDIR@
	$(INSTALL_DATA) @WAEI_DICT@ @DICTDIR@

install-info: texi/sdic.info
	$(INSTALL_DATA) texi/sdic.info @infodir@

info: texi/sdic.info

texi/sdic.info: texi/sdic.texi
	$(EMACS) -batch -q -l install.el -f batch-texinfo-format $?

config: lisp/sample.emacs
	@echo -n "本当に $$HOME/.emacs を書き換えますか[yes/no] " ;\
	read YN ;\
	test "$$YN" = yes
	@if [ -f "$$HOME/.emacs" ] ;\
	then \
	     echo "元の $$HOME/.emacs を $$HOME/.emacs.orig として保存します";\
	     cp -p $$HOME/.emacs $$HOME/.emacs.orig ;\
	fi
	( echo ; cat lisp/sample.emacs )>>$$HOME/.emacs


## 辞書を生成するためのルール
gene.dic: contrib/gene.perl gene.txt
	@NKF@ -S -e gene.txt | @PERL@ contrib/gene.perl --compat > $@

gene.sdic: contrib/gene.perl gene.txt
	@NKF@ -S -e gene.txt | @PERL@ contrib/gene.perl > $@

jgene.dic: contrib/jgene.perl gene.dic
	@PERL@ contrib/jgene.perl --compat < gene.dic > $@

jgene.sdic: gene.dic
	@PERL@ contrib/jgene.perl < gene.dic > $@

gene.txt:
	test -f gene95.lzh -o -f gene95.tar.gz -o -f gene95.tar.bz2
	if [ -f gene95.lzh ]; then \
		lha x gene95.lzh gene.txt; \
	elif [ -f gene95.tar.gz ]; then \
		@GZIP@ -dc gene95.tar.gz | tar xf - gene.txt; \
	else \
		@BZIP2@ -dc gene95.tar.bz2 | tar xf - gene.txt; \
	fi
	test -s gene.txt

eijirou.dic: contrib/eijirou.perl
	@NKF@ -S -e @EIJIROU_DIR@/*.txt | @PERL@ contrib/eijirou.perl --compat > $@

eijirou.sdic: contrib/eijirou.perl
	@NKF@ -S -e @EIJIROU_DIR@/*.txt | @PERL@ contrib/eijirou.perl > $@

waeijirou.dic: contrib/eijirou.perl
	@NKF@ -S -e @WAEIJIROU_DIR@/*.txt | @PERL@ contrib/eijirou.perl --waei --compat > $@

waeijirou.sdic: contrib/eijirou.perl
	@NKF@ -S -e @WAEIJIROU_DIR@/*.txt | @PERL@ contrib/eijirou.perl --waei > $@

eedict.sdic: edict contrib/edict.perl
	@PERL@ contrib/edict.perl --reverse edict > $@

edict.sdic: edict contrib/edict.perl
	@PERL@ contrib/edict.perl edict > $@

e4jwords.sdic: 4jwords contrib/edict.perl
	@PERL@ contrib/edict.perl --reverse 4jwords > $@

4jwords.sdic: 4jwords contrib/edict.perl
	@PERL@ contrib/edict.perl 4jwords > $@

ecompdic.sdic: compdic contrib/edict.perl
	@PERL@ contrib/edict.perl --reverse compdic > $@

compdic.sdic: compdic contrib/edict.perl
	@PERL@ contrib/edict.perl compdic > $@

eenamdict.sdic: enamdict contrib/edict.perl
	@PERL@ contrib/edict.perl --reverse enamdict > $@

enamdict.sdic: enamdict contrib/edict.perl
	@PERL@ contrib/edict.perl enamdict > $@


edict.dic: edict
	@NKF@ -deZ2 edict | \
	@PERL@ -pe 's!\s+/!\t!;s!/!, !g;s!, $$!!' | @SORT@ -f > $@

4jwords.dic: 4jword
	@NKF@ -deZ2 4jwords | \
	@PERL@ -pe 's!\s+/!\t!;s!/!, !g;s!, $$!!' | @SORT@ -f > $@

compdic.dic: compdic
	@NKF@ -deZ2 compdic | \
	@PERL@ -pe 's!\s+/!\t!;s!/!, !g;s!, $$!!' | @SORT@ -f > $@

enamdict.dic: enamdict
	@NKF@ -deZ2 enamdict | \
	@PERL@ -pe 's!\s+/!\t!;s!/!, !g;s!, $$!!' | @SORT@ -f > $@

edict:
	test -f $@.gz -o -f $@.bz2
	if [ -f $@.gz ]; then \
		@GZIP@ -dc $@.gz >$@ ;\
	else \
		@BZIP2@ -dc $@.bz2 >$@ ;\
	fi
	test -s $@

4jwords:
	test -f $@.gz -o -f $@.bz2
	if [ -f $@.gz ]; then \
		@GZIP@ -dc $@.gz >$@ ;\
	else \
		@BZIP2@ -dc $@.bz2 >$@ ;\
	fi
	test -s $@

compdic:
	test -f $@.gz -o -f $@.bz2
	if [ -f $@.gz ]; then \
		@GZIP@ -dc $@.gz >$@ ;\
	else \
		@BZIP2@ -dc $@.bz2 >$@ ;\
	fi
	test -s $@

enamdict:
	test -f $@.gz -o -f $@.bz2
	if [ -f $@.gz ]; then \
		@GZIP@ -dc $@.gz >$@ ;\
	else \
		@BZIP2@ -dc $@.bz2 >$@ ;\
	fi
	test -s $@

