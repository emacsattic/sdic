;;; -*- Emacs-Lisp -*-
;;; $Id$

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of sdic. Please see sdic.el for more detail.

;; SDIC 形式の辞書を grep を利用して検索するライブラリです。


;;; Install:

;; (1) 辞書を検索するために grep を利用しています。パスが通っているか
;;     確認して下さい。
;;
;; (2) 辞書を適切な形式に変換して、適当な場所( 例: /usr/dict/ )に保存
;;     して下さい。辞書変換用スクリプトとして以下の Perl スクリプトが
;;     利用できます。
;;
;;         gene.perl    - GENE95 辞書
;;         edict.perl   - EDICT 辞書
;;         eijirou.perl - 英辞郎
;;
;; (3) 使えるようにした辞書の定義情報を sdic-eiwa-dictionary-list また
;;     は sdic-waei-dictionary-list に追加して下さい。
;;
;;         (setq sdic-eiwa-dictionary-list
;;               (cons '(sdic-grep "/usr/dict/gene.dic") sdic-eiwa-dictionary-list))
;;
;;     辞書定義情報は次のような構成になっています。
;;
;;         (sdic-grep ファイル名 (オプションA 値A) (オプションB 値B) ...)
;;
;;     特別な指定が不要な場合には、オプションは省略できます。
;;
;;         (sdic-grep ファイル名)


;;; Options:

;; sdic-grep.el に対して指定できるオプションは次の通りです。
;;
;; coding-system
;;     辞書の漢字コードを指定します。省略した場合は、
;;     sdic-default-coding-system の値を使います。
;;
;; title
;;     辞書のタイトルを指定します。省略した場合は、辞書ファイルの 
;;     basename をタイトルとします。
;;
;; add-keys-to-headword
;;     全ての検索キーを含めて見出し語を構成する場合に t に設定して下さ
;;     い。和英辞書を検索する場合に、振り仮名も含めて出力する場合に利
;;     用します。
;;
;; command
;;     外部コマンドの名前を指定します。省略した場合は 
;;     sdic-grep-command の値を使います。


;;; Note;

;; sdic-sgml.el , sdic-grep.el , sdic-array.el は SDIC 形式の辞書を検
;; 索するためのライブラリです。それぞれの違いは次の通りです。
;;
;; ・sdic-sgml.el
;;     辞書データを全てメモリに読み込んでから検索を行います。外部コマ
;;     ンドを必要としませんが、大量のメモリが必要になります。
;;
;; ・sdic-grep.el
;;     grep を利用して検索を行います。
;;
;; ・sdic-array.el
;;     array を利用して検索を行います。辞書の index file を事前に生成
;;     しておいてから検索を行いますので、高速に検索が可能です。しかし、
;;     index file は辞書の3倍程度の大きさになります。
;;
;; 比較的小規模の辞書を検索する場合は sdic-grep.el が最適でしょう。し
;; かし、5MByte より大きい辞書の場合は sdic-array.el の利用を考慮すべ
;; きだと思います。
;;
;; SDIC 形式の辞書の構造については、sdic.texi を参照してください。


;;; ライブラリ定義情報
(require 'sdic)
(require 'sdic-sgml)
(provide 'sdic-grep)
(put 'sdic-grep 'version "2.0")
(put 'sdic-grep 'init-dictionary 'sdic-grep-init-dictionary)
(put 'sdic-grep 'open-dictionary 'sdic-grep-open-dictionary)
(put 'sdic-grep 'close-dictionary 'sdic-sgml-close-dictionary)
(put 'sdic-grep 'search-entry 'sdic-grep-search-entry)
(put 'sdic-grep 'get-content 'sdic-grep-get-content)


;;;----------------------------------------------------------------------
;;;		定数/変数の宣言
;;;----------------------------------------------------------------------

(defvar sdic-grep-command
  (catch 'which
    (mapcar '(lambda (file)
	       (mapcar '(lambda (path)
			  (if (file-executable-p (expand-file-name file path))
			      (throw 'which (expand-file-name file path))))
		       exec-path))
	    '("fgrep" "fgrep.exe" "grep" "grep.exe")))
  "*Executable file name of grep")

(defconst sdic-grep-buffer-name " *sdic-grep*")



;;;----------------------------------------------------------------------
;;;		本体
;;;----------------------------------------------------------------------

(defun sdic-grep-available-p ()
  (stringp sdic-grep-command))


(defun sdic-grep-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (sdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar '(lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "sdic-grep+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (or (get dic 'command)
	      (put dic 'command sdic-grep-command))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system sdic-default-coding-system))
	  dic)
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun sdic-grep-open-dictionary (dic)
  "Function to open dictionary"
  (and (or (sdic-buffer-live-p (get dic 'sdic-sgml-buffer))
	   (put dic 'sdic-sgml-buffer (generate-new-buffer sdic-grep-buffer-name)))
       dic))


(defun sdic-grep-search-entry (dic string &optional search-type) "\
Function to search word with look or grep, and write results to current buffer.
search-type の値によって次のように動作を変更する。
    nil    : 前方一致検索
    t      : 後方一致検索
    lambda : 完全一致検索
    0      : 全文検索
検索結果として見つかった見出し語をキーとし、その定義文の先頭の point を値とする
連想配列を返す。
"
  (save-excursion
    (set-buffer (get dic 'sdic-sgml-buffer))
    (let ((add-keys (get dic 'add-keys-to-headword))
	  limit ret)
      (if (get dic 'sdic-grep-erase-buffer)
	  (delete-region (point-min) (point-max)))
      (setq limit (goto-char (point-max)))
      (put dic 'sdic-grep-erase-buffer nil)
      (sdic-call-process (get dic 'command) nil t nil
			 (get dic 'coding-system)
			 (sdic-sgml-make-query-string string search-type)
			 (get dic 'file-name))
      ;; 各検索結果に ID を付与する
      (goto-char limit)
      (while (progn
	       (setq ret (cons (sdic-sgml-get-entry add-keys) ret))
	       (= 0 (forward-line 1))))
      (reverse (delq nil ret)))))


(defun sdic-grep-get-content (dic point)
  (put dic 'sdic-grep-erase-buffer t)
  (sdic-sgml-get-content dic point))
