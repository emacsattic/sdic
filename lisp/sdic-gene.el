;;; -*- Emacs-Lisp -*-
;;; $Id$

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of xdic. Please see xdic.el for more detail.

;; 1行のデータの形式が
;;
;;	見出し語 TAB 定義文 RET
;;
;; となっている辞書を外部プログラムに頼らずに検索するライブラリです。


;;; Install:

;; (1) 辞書を適切な形式に変換して、適当な場所( 例: /usr/dict/ )に保存
;;     して下さい。辞書変換用スクリプトとして以下の Perl スクリプトが
;;     利用できます。
;;
;;         gene.perl    - GENE95 辞書
;;         jgene.perl   - GENE95 辞書から和英辞書を生成する
;;         eijirou.perl - 英辞郎
;;
;; (2) 使えるようにした辞書の定義情報を xdic-eiwa-dictionary-list また
;;     は xdic-waei-dictionary-list に追加して下さい。
;;
;;         (setq xdic-eiwa-dictionary-list
;;               (cons '(xdic-gene "/usr/dict/gene.dic") xdic-eiwa-dictionary-list))
;;
;;     辞書定義情報は次のような構成になっています。
;;
;;         (xdic-gene ファイル名 (オプションA 値A) (オプションB 値B) ...)
;;
;;     特別な指定が不要な場合には、オプションは省略できます。
;;
;;         (xdic-gene ファイル名)


;;; Options:

;; xdic-gene.el に対して指定できるオプションは次の通りです。
;;
;; coding-system
;;     辞書の漢字コードを指定します。省略した場合は、
;;     xdic-default-coding-system の値を使います。
;;
;; title
;;     辞書のタイトルを指定します。省略した場合は、辞書ファイルの 
;;     basename をタイトルとします。
;;
;; extract
;;     圧縮辞書を展開するための外部コマンドを指定します。省略した場合
;;     は、辞書が圧縮されていないと見なします。
;;
;; extract-option
;;     extract オプションによって指定された外部コマンドに対して、辞書
;;     を展開して標準出力に出力させるためのコマンドライン引数を指定し
;;     ます。省略した場合は xdic-gene-extract-option の値を使います。


;;; Note:

;; xdic-compat.el と xdic-gene.el は同じ機能を提供しているライブラリで
;; す。xdic-compat.el は外部コマンドを呼び出しているのに対して、
;; xdic-gene.el は Emacs の機能のみを利用しています。ただし、辞書をバッ
;; ファに読み込んでから検索を行なうので、大量のメモリが必要になります。
;;
;; Default の設定では、必要な外部コマンドが見つかった場合は 
;; xdic-compat.el を、見つからなかった場合には xdic-gene.el を使うよう
;; になっています。


;;; ライブラリ定義情報
(require 'xdic)
(provide 'xdic-gene)
(put 'xdic-gene 'version "1.2")
(put 'xdic-gene 'init-dictionary 'xdic-gene-init-dictionary)
(put 'xdic-gene 'open-dictionary 'xdic-gene-open-dictionary)
(put 'xdic-gene 'close-dictionary 'xdic-gene-close-dictionary)
(put 'xdic-gene 'search-entry 'xdic-gene-search-entry)
(put 'xdic-gene 'get-content 'xdic-gene-get-content)


;;;----------------------------------------------------------------------
;;;		定数/変数の宣言
;;;----------------------------------------------------------------------

(defvar xdic-gene-extract-option "-dc" "\
*Option for archiver.
圧縮辞書を展開するために使うオプション")

(defconst xdic-gene-search-buffer-name " *xdic-gene*")



;;;----------------------------------------------------------------------
;;;		本体
;;;----------------------------------------------------------------------

(defun xdic-gene-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (xdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar '(lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "xdic-gene+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (if (get dic 'extract)
	      (or (get dic 'extract-option)
		  (put dic 'extract-option xdic-gene-extract-option)))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system xdic-default-coding-system))
	  dic)
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun xdic-gene-open-dictionary (dic)
  "Function to open dictionary"
  (if (or (xdic-buffer-live-p (get dic 'xdic-gene-search-buffer))
	  (save-excursion
	    (set-buffer (put dic 'xdic-gene-search-buffer (generate-new-buffer xdic-gene-search-buffer-name)))
	    (insert "\n")
	    (prog1 (if (get dic 'extract)
		       (= 0 (xdic-call-process (get dic 'extract) nil t nil
					       (get dic 'coding-system)
					       (get dic 'extract-option)
					       (get dic 'file-name)))
		     (condition-case err
			 (xdic-insert-file-contents (get dic 'file-name) (get dic 'coding-system))
		       (error nil)))
	      (setq buffer-read-only t)
	      (set-buffer-modified-p nil))))
      dic))


(defun xdic-gene-close-dictionary (dic)
  "Function to close dictionary"
  (kill-buffer (get dic 'xdic-gene-search-buffer))
  (put dic 'xdic-gene-search-buffer nil))


(defsubst xdic-gene-search-internal (string)
  "通常の検索を行う内部関数"
  (let (ret (case-fold-search t))
    (while (search-forward string nil t)
      (save-excursion
	(setq ret (cons (cons (buffer-substring (progn (beginning-of-line) (point))
						(progn (skip-chars-forward "^\t") (point)))
			      (1+ (point)))
			ret))))
    (reverse ret)))


(defsubst xdic-gene-re-search-internal (string)
  "正規表現検索を行う内部関数"
  (let (ret (case-fold-search t))
    (while (re-search-forward string nil t)
      (save-excursion
	(setq ret (cons (cons (buffer-substring (progn (beginning-of-line) (point))
						(progn (skip-chars-forward "^\t") (point)))
			      (1+ (point)))
			ret))))
    (reverse ret)))


(defun xdic-gene-search-entry (dic string &optional search-type) "\
Function to search word with look or grep, and write results to current buffer.
search-type の値によって次のように動作を変更する。
    nil    : 前方一致検索
    t      : 後方一致検索
    lambda : 完全一致検索
    0      : 全文検索
    regexp : 正規表現検索
検索結果として見つかった見出し語をキーとし、その定義文の先頭の point を値とする
連想配列を返す。
"
  (save-excursion
    (set-buffer (get dic 'xdic-gene-search-buffer))
    (goto-char (point-min))
    (cond
     ;; 前方一致検索
     ((eq search-type nil)
      (xdic-gene-search-internal (concat "\n" string)))
     ;; 後方一致検索
     ((eq search-type t)
      (xdic-gene-search-internal (concat string "\t")))
     ;; 完全一致検索
     ((eq search-type 'lambda)
      (xdic-gene-search-internal (concat "\n" string "\t")))
     ;; 全文検索
     ((eq search-type 0)
      (xdic-gene-search-internal string))
     ;; 正規表現検索
     ((eq search-type 'regexp)
      (xdic-gene-re-search-internal string))
     ;; それ以外の検索形式を指定された場合
     (t (error "Not supported search type is specified. \(%s\)"
	       (prin1-to-string search-type))))))


(defun xdic-gene-get-content (dic point)
  (save-excursion
    (set-buffer (get dic 'xdic-gene-search-buffer))
    (if (<= point (point-max))
	(buffer-substring (goto-char point) (progn (end-of-line) (point)))
      (error "Can't find content. (ID=%d)" point))))
