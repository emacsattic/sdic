;;; -*- Emacs-Lisp -*-
;;; $Id$

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of xdic. Please see xdic.el for more detail.

;; XDIC 形式の辞書を grep を利用して検索するライブラリです。


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
;; (3) 使えるようにした辞書の定義情報を xdic-eiwa-dictionary-list また
;;     は xdic-waei-dictionary-list に追加して下さい。
;;
;;         (setq xdic-eiwa-dictionary-list
;;               (cons '(xdic-grep "/usr/dict/gene.dic") xdic-eiwa-dictionary-list))
;;
;;     辞書定義情報は次のような構成になっています。
;;
;;         (xdic-grep ファイル名 (オプションA 値A) (オプションB 値B) ...)
;;
;;     特別な指定が不要な場合には、オプションは省略できます。
;;
;;         (xdic-grep ファイル名)


;;; Options:

;; xdic-grep.el に対して指定できるオプションは次の通りです。
;;
;; coding-system
;;     辞書の漢字コードを指定します。省略した場合は、
;;     xdic-default-coding-system の値を使います。
;;
;; title
;;     辞書のタイトルを指定します。省略した場合は、辞書ファイルの 
;;     basename をタイトルとします。
;;
;; command
;;     外部コマンドの名前を指定します。省略した場合は 
;;     xdic-grep-command の値を使います。


;;; Note;

;; xdic-sgml.el , xdic-grep.el , xdic-array.el は XDIC 形式の辞書を検
;; 索するためのライブラリです。それぞれの違いは次の通りです。
;;
;; ・xdic-sgml.el
;;     辞書データを全てメモリに読み込んでから検索を行います。外部コマ
;;     ンドを必要としませんが、大量のメモリが必要になります。
;;
;; ・xdic-grep.el
;;     grep を利用して検索を行います。
;;
;; ・xdic-array.el
;;     array を利用して検索を行います。辞書の index file を事前に生成
;;     しておいてから検索を行いますので、高速に検索が可能です。しかし、
;;     index file は辞書の3倍程度の大きさになります。
;;
;; 比較的小規模の辞書を検索する場合は xdic-grep.el が最適でしょう。し
;; かし、5MByte より大きい辞書の場合は xdic-array.el の利用を考慮すべ
;; きだと思います。
;;
;; XDIC 形式の辞書の構造については、dictionary-format.txt を参照してく
;; ださい。


;;; ライブラリ定義情報
(require 'xdic)
(require 'xdic-sgml)
(provide 'xdic-grep)
(put 'xdic-grep 'version "1.0")
(put 'xdic-grep 'init-dictionary 'xdic-grep-init-dictionary)
(put 'xdic-grep 'open-dictionary 'xdic-grep-open-dictionary)
(put 'xdic-grep 'close-dictionary 'xdic-sgml-close-dictionary)
(put 'xdic-grep 'search-entry 'xdic-grep-search-entry)
(put 'xdic-grep 'get-content 'xdic-grep-get-content)


;;;----------------------------------------------------------------------
;;;		定数/変数の宣言
;;;----------------------------------------------------------------------

(defvar xdic-grep-command
  (catch 'which
    (mapcar '(lambda (file)
	       (mapcar '(lambda (path)
			  (if (file-executable-p (expand-file-name file path))
			      (throw 'which (expand-file-name file path))))
		       exec-path))
	    '("grep" "grep.exe")))
  "*Executable file name of grep")

(defconst xdic-grep-buffer-name "*xdic-grep*")



;;;----------------------------------------------------------------------
;;;		本体
;;;----------------------------------------------------------------------

(defun xdic-grep-available-p ()
  (stringp xdic-grep-command))


(defun xdic-grep-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (xdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar '(lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "xdic-grep+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (or (get dic 'command)
	      (put dic 'command xdic-grep-command))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system xdic-default-coding-system))
	  dic)
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun xdic-grep-open-dictionary (dic)
  "Function to open dictionary"
  (and (or (xdic-buffer-live-p (get dic 'xdic-sgml-buffer))
	   (put dic 'xdic-sgml-buffer (generate-new-buffer xdic-grep-buffer-name)))
       dic))


(defun xdic-grep-search-entry (dic string &optional search-type) "\
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
    (set-buffer (get dic 'xdic-sgml-buffer))
    (let (limit ret)
      (if (get dic 'xdic-grep-erase-buffer)
	  (delete-region (point-min) (point-max)))
      (setq limit (goto-char (point-max)))
      (put dic 'xdic-grep-erase-buffer nil)
      (xdic-call-process (get dic 'command) nil t nil
			 (get dic 'coding-system)
			 (xdic-sgml-make-query-string string search-type)
			 (get dic 'file-name))
      ;; 各検索結果に ID を付与する
      (goto-char limit)
      (while (progn
	       (if (looking-at "<K>")
		   (setq ret (cons (xdic-sgml-get-entry) ret)))
	       (= 0 (forward-line 1))))
      (reverse ret))))


(defun xdic-grep-get-content (dic point)
  (put dic 'xdic-grep-erase-buffer t)
  (xdic-sgml-get-content dic point))
