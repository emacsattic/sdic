# -*- Makefile -*-
EXTRA_DIST = install.el texi/sdic.texi contrib/edict.perl contrib/eijirou.perl contrib/gene.perl contrib/jgene.perl
DISTCLEANFILES = install.el texi/sdic.info
SUBDIRS = lisp

dict: @EIWA_DICTIONARY@ @WAEI_DICTIONARY@
# info_TEXINFOS = sdic.texi     # this requires makeinfo

install-dict:
	$(mkinstalldirs) @DICTDIR@
	$(INSTALL_DATA) @EIWA_DICTIONARY@ @DICTDIR@
	$(INSTALL_DATA) @WAEI_DICTIONARY@ @DICTDIR@

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
	@NKF@ -S -e gene.txt | @PERL@ contrib/gene.perl --compat > gene.dic

gene.sdic: contrib/gene.perl gene.txt
	@NKF@ -S -e gene.txt | @PERL@ contrib/gene.perl > gene.sdic

jgene.dic: contrib/jgene.perl gene.dic
	@PERL@ contrib/jgene.perl --compat < gene.dic > jgene.dic

jgene.sdic: gene.dic
	@PERL@ contrib/jgene.perl < gene.dic > jgene.sdic

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
	@NKF@ -S -e *.txt | @PERL@ contrib/eijirou.perl --compat >eijirou.dic

eijirou.sdic: contrib/eijirou.perl
	@NKF@ -S -e *.txt | @PERL@ contrib/eijirou.perl >eijirou.sdic

ejdict.sdic: edict contrib/edict.perl
	@PERL@ contrib/edict.perl --reverse edict >ejdict.sdic

jedict.sdic: edict contrib/edict.perl
	@PERL@ contrib/edict.perl edict >jedict.sdic

edict:
	test -f edict.gz -o -f edict.bz2
	if [ -f edict.gz ]; then \
		@GZIP@ -dc edict.gz >edict ;\
	else \
		@BZIP2@ -dc edict.bz2 >edict ;\
	fi
	test -s edict
