#--------------------------------------------------------------------
# Step 1.
#     プログラム本体をインストールするディレクトリを LISPDIR に、辞書
#     をインストールするディレクトリを DICTDIR に指定して下さい。また、
#     Info をインストールするディレクトリを INFODIR に指定して下さい。
PREFIX  = /usr/local
LISPDIR = $(PREFIX)/lib/mule/site-lisp
INFODIR = $(PREFIX)/info
DICTDIR = $(LISPDIR)



#--------------------------------------------------------------------
# Step 2.
#     sdic を利用する Emacs の名前を指定して下さい。ここで指定された 
#     Emacs が、*.el をバイトコンパイルするために使われます。
# EMACS = emacs
# EMACS = xemacs
EMACS = mule



#--------------------------------------------------------------------
# Step 3.
#     デフォルトで使用する辞書を指定して下さい。英和辞書として利用する
#     辞書の名前を EIWA_DICTIONARY に、和英辞書として利用する辞書を 
#     WAEI_DICTIONARY に指定して下さい。
#
# (1) 英和辞書にGENE95辞書を使い、EDICT辞書を和英辞書として使う場合 (推奨)
#       gene95.lzh または gene95.tar.gz と edict.gz を、この Makefile 
#       と同じディレクトリに置いてから make install して下さい。詳細に
#       ついては INSTALL を参照。
EIWA_DICTIONARY = gene.sdic
WAEI_DICTIONARY = jedict.sdic
#
# (2) GENE95辞書を英和辞書と和英辞書の両方に使う場合
#       gene95.lzh または gene95.tar.gz を、この Makefile と同じディレ
#       クトリに置いてから、make install して下さい。
# EIWA_DICTIONARY = gene.sdic
# WAEI_DICTIONARY = jgene.sdic
#
# (3) EDICT辞書を英和辞書と和英辞書の両方に使う場合
#       edict.gz を、この Makefile と同じディレクトリに置いてから、
#       make install して下さい。
# EIWA_DICTIONARY = ejdict.sdic
# WAEI_DICTIONARY = jedict.sdic
#
# (4) GENE95辞書をCOMPAT形式に変換して、英和辞書と和英辞書の両方に利用する場合
#       gene95.lzh または gene95.tar.gz を、この Makefile と同じディレ
#       クトリに置いてから、make install して下さい。
# EIWA_DICTIONARY = gene.dic
# WAEI_DICTIONARY = jgene.dic
#
# (5) 「英辞郎」を利用する場合
#       「英辞郎」附属の全ての辞書ファイルをこの Makefile と同じディレ
#       クトリにコピーしてから、make install して下さい。詳細について
#       は INSTALL を参照。
# EIWA_DICTIONARY = eijirou.sdic
# WAEI_DICTIONARY =
#
# (6) デフォルトで使用する辞書が無い場合
#       空文字列を指定して下さい。
# EIWA_DICTIONARY =
# WAEI_DICTIONARY =



#--------------------------------------------------------------------
# Step 4. (optional)
#     各種ツールの名前を指定して下さい。
#
#     次の指定は UNIX 環境でインストールするための設定です。
NKF  = nkf -S -e
PERL = perl
GZIP = gzip -dc
BZIP = bzip2 -dc
CP   = cp -f
#
#     SunOS など、-f オプションのない cp を使っている場合は、取り除い
#     てください。
# CP = cp
#     
#     Windows 環境でインストールする場合は、同等の機能を持つコマンドの
#     名前を、拡張子を含めて指定して下さい。
# NKF  = nkf.exe -S -e
# PERL = perl.exe
# GZIP = gzip.exe -dc
# CP   = copy



#--------------------------------------------------------------------
# Step 5.
#     Makefile の編集はこれで終りです。以下のいずれかを実行して下さい。
#
#     プログラムと辞書を全てインストールする場合
#         make install
#
#     プログラムのみをインストールする場合
#         make install-lisp
#
#     Info のみをインストールする場合
#         make install-info
#
#     辞書のみをインストールする場合
#         make install-dic



#--------------------------------------------------------------------
#     本体   ( ここから下は、通常は編集する必要はありません )

SOURCES    = sdic.el sdic-sgml.el sdic-grep.el sdic-array.el sdic-compat.el \
		sdic-gene.el stem.el
TARGETS    = sdic.elc sdic-sgml.elc sdic-grep.elc sdic-array.elc sdic-compat.elc \
		sdic-gene.elc stem.elc
DICTIONARY = $(EIWA_DICTIONARY) $(WAEI_DICTIONARY)

###CUT BEGIN

# 自分の使っている Makefile のルールが邪魔になるので、CUT BEGIN から 
# CUT END までの範囲を削除した Makefile をつくって配布する。

VERSION = `perl -ne 'next unless /\(defconst sdic-version "([^"]+)"\)/;print "$$1";last;' sdic.el.in`
DIR     = sdic-$(VERSION)
WWW     = $(HOME)/usr/doc/homepage/sdic

# 配布用パッケージに添付するファイルのリスト (Makefile 以外)
SCRIPTS = gene.perl eijirou.perl jgene.perl edict.perl
FILES   = README INSTALL COPYING sdic.texi \
		sdic.el.in sdic-sgml.el sdic-grep.el sdic-array.el sdic-compat.el sdic-gene.el \
		stem.el lp.el gene.el sample.emacs.in $(SCRIPTS)

homepage: archive
	@echo -n 'ホームページを更新しますか? '
	@read YN ; test "$$YN" = y
	@file=$(DIR).tar.gz ;\
	cp -f $$file $(WWW) ;\
	cd $(WWW) ;\
	echo 更新しました。
	@touch homepage

archive: $(FILES) Makefile
	@dir=$(DIR) ;\
	echo 配布用パッケージ $$dir.tar.gz を生成 ;\
	rm -rf $$dir ;\
	mkdir $$dir ;\
	sed -e '/^###CUT BEGIN/,/^###CUT END/d' -e 's/^_clean:/clean:/' Makefile >$$dir/Makefile ;\
	cp -fp $(FILES) $$dir ;\
	tar czvf $$dir.tar.gz $$dir
	@touch archive

sdic.el.in: sdic.el.orig
	@echo 配布用ソース sdic.el.in を生成 ;\
	perl -p -e 's!"~/usr/dict/gene.sdic"!"%%EIWA_DICTIONARY%%"!;' \
		-e 's!"~/usr/dict/edict.sdic"!"%%WAEI_DICTIONARY%%"!;' sdic.el.orig >sdic.el.in

info: sdic.info sdic_toc.html
	cp -fp sdic*.html $(WWW)
	cp -fp sdic.info $(HOME)/usr/info

sdic_toc.html: texi2html sdic.texi
	perl texi2html -split_node sdic.texi

clean: _clean
	dir=$(DIR) ;\
	rm -rf $$dir $$dir.tar.gz sdic.el.in archive homepage sdic*.html

version:
	@echo $(DIR)

# 各種 Emacsen を使って正常にバイトコンパイルできるかテストするルール
build: archive
	cd $(DIR) && \
	$(MAKE) compile clean && \
	$(MAKE) EMACS=mule-19.28 compile clean && \
	$(MAKE) EMACS=emacs compile clean && \
	$(MAKE) EMACS=xemacs compile clean

###CUT END

.SUFFIXES:
.SUFFIXES: .el .elc

.el.elc:
	$(EMACS) -batch -q -l ./lp.el -f batch-byte-compile $<


## 一般に使用するルール
default:
	@echo "Edit this makefile first." ;\
	echo 'Type "make install" to install all' ;\
	echo 'Type "make install-lisp" to install lisp programs' ;\
	echo 'Type "make install-info" to install sdic.info' ;\
	echo 'Type "make install-dic" to install $(DICTIONARY)'

install: install-lisp install-info install-dic sample.emacs
	@echo 'Add configuration for sdic-mode to "~/.emacs"' ;\
	echo 'There is a sample of configuration in "sample.emacs"'

install-lisp: compile
	$(CP) $(SOURCES) $(TARGETS) $(LISPDIR)

install-info: sdic.info
	$(CP) sdic.info $(INFODIR)

install-dic: $(DICTIONARY)
	$(CP) $(DICTIONARY) $(DICTDIR)


## 辞書を生成するためのルール
gene.dic: gene.perl gene.txt
	$(NKF) gene.txt | $(PERL) gene.perl --compat > gene.dic

gene.sdic: gene.perl gene.txt
	$(NKF) gene.txt | $(PERL) gene.perl > gene.sdic

jgene.dic: jgene.perl gene.dic
	$(PERL) jgene.perl --compat < gene.dic > jgene.dic

jgene.sdic: jgene.dic
	$(PERL) jgene.perl < gene.dic > jgene.sdic

gene.txt:
	test -f gene95.lzh -o -f gene95.tar.gz -o -f gene95.tar.bz2
	if [ -f gene95.lzh ]; then \
		lha x gene95.lzh gene.txt; \
	elif [ -f gene95.tar.gz ]; then \
		$(GZIP) gene95.tar.gz | tar xf - gene.txt; \
	else \
		$(BZIP) gene95.tar.bz2 | tar xf - gene.txt; \
	fi

eijirou.dic: eijirou.perl
	$(NKF) *.txt | $(PERL) eijirou.perl --compat >eijirou.dic

eijirou.sdic: eijirou.perl
	$(NKF) *.txt | $(PERL) eijirou.perl >eijirou.sdic

ejdict.sdic: edict edict.perl
	$(PERL) edict.perl --reverse edict >ejdict.sdic

jedict.sdic: edict edict.perl
	$(PERL) edict.perl edict >jedict.sdic

edict: edict.gz
	$(GZIP) edict.gz >edict


## サンプル設定ファイルを生成するルール
sample.emacs: sample.emacs.in Makefile
	$(PERL) -p -e 's#%%LISPDIR%%#$(LISPDIR)#' sample.emacs.in >sample.emacs


## インストールディレクトリに応じて *.el を修正するルール
sdic.el: sdic.el.in Makefile
	cp sdic.el.in sdic.el
	if [ $(EIWA_DICTIONARY)z != z ]; then \
	$(PERL) -pi~ -e 's#%%EIWA_DICTIONARY%%#$(DICTDIR)/$(EIWA_DICTIONARY)#;' sdic.el ; fi
	if [ $(WAEI_DICTIONARY)z != z ]; then \
	$(PERL) -pi~ -e 's#%%WAEI_DICTIONARY%%#$(DICTDIR)/$(WAEI_DICTIONARY)#;' sdic.el ; fi


## *.el をバイトコンパイルするルール
compile: $(TARGETS)

sdic.info: sdic.texi
	$(EMACS) -batch -q -l texinfmt -f batch-texinfo-format $?

_clean:
	rm -f *~ *.elc sdic.el sdic.info sample.emacs

distclean: clean
	rm -f gene.dic gene.sdic jgene.dic jgene.sdic edict.sdic eijirou.sdic

config: sample.emacs
	@echo -n "本当に $$HOME/.emacs を書き換えますか[yes/no] " ;\
	read YN ;\
	test "$$YN" = yes
	@if [ -f "$$HOME/.emacs" ] ;\
	then \
		echo "元の $$HOME/.emacs を $$HOME/.emacs.orig として保存します" ;\
		cp -p $$HOME/.emacs $$HOME/.emacs.orig ;\
	fi
	( echo ; cat sample.emacs )>>$$HOME/.emacs
