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
;; となっている辞書を外部プログラム( look / grep )を利用して検索するラ
;; イブラリです。


;;; Install:

;; (1) look と正規表現の使える grep ( GNU grep または egrep )が必要で
;;     す。パスが通っているか確認して下さい。
;;
;; (2) 辞書を適切な形式に変換して、適当な場所( 例: /usr/dict/ )に保存
;;     して下さい。辞書変換用スクリプトとして以下の Perl スクリプトが
;;     利用できます。
;;
;;         gene.perl    - GENE95 辞書
;;         jgene.perl   - GENE95 辞書から和英辞書を生成する
;;         eijirou.perl - 英辞郎
;;
;; (3) 使えるようにした辞書の定義情報を xdic-eiwa-dictionary-list また
;;     は xdic-waei-dictionary-list に追加して下さい。
;;
;;         (setq xdic-eiwa-dictionary-list
;;               (cons '(xdic-unix "/usr/dict/gene.dic") xdic-eiwa-dictionary-list))
;;
;;     辞書定義情報は次のような構成になっています。
;;
;;         (xdic-unix ファイル名 (オプションA 値A) (オプションB 値B) ...)
;;
;;     特別な指定が不要な場合には、オプションは省略できます。
;;
;;         (xdic-unix ファイル名)


;;; Options:

;; xdic-unix.el に対して指定できるオプションは次の通りです。
;;
;; coding-system
;;     辞書の漢字コードを指定します。省略した場合は、
;;     xdic-default-coding-system の値を使います。
;;
;; title
;;     辞書のタイトルを指定します。省略した場合は、辞書ファイルの 
;;     basename をタイトルとします。
;;
;; look
;;     前方一致検索/完全一致検索の時に利用する外部コマンドの名前を指定
;;     します。省略した場合は xdic-unix-look-command の値を使います。
;;
;; look-case-option
;;     look オプションによって指定された外部コマンドに対して、英大文字
;;     /小文字を区別しないで検索するように指示するためのコマンドライン
;;     引数を指定します。省略した場合は xdic-unix-look-case-option の
;;     値を使います。
;;
;; grep
;;     後方一致検索/任意検索の時に利用する外部コマンドの名前を指定しま
;;     す。省略した場合は xdic-unix-grep-command の値を使います。
;;
;; grep-case-option
;;     grep オプションによって指定された外部コマンドに対して、英大文字
;;     /小文字を区別しないで検索するように指示するためのコマンドライン
;;     引数を指定します。省略した場合は xdic-unix-grep-case-option の
;;     値を使います。


;;; Note:

;; xdic-unix-look-command と xdic-unix-grep-command の値は自動的に設定
;; されます。例えば、xdic-unix-grep-command の場合、egrep / egrep.exe
;; / grep / grep.exe と4種のコマンドを検索して、見つかったコマンドを使
;; います。
;;
;; xdic-unix.el と xdic-gene.el は同じ機能を提供しているライブラリです。
;; xdic-unix.el は外部コマンドを呼び出しているのに対して、xdic-gene.el 
;; は Emacs の機能のみを利用しています。ただし、辞書をバッファに読み込
;; んでから検索を行なうので、大量のメモリが必要になります。
;;
;; Default の設定では、必要な外部コマンドが見つかった場合は 
;; xdic-unix.el を、見つからなかった場合には xdic-gene.el を使うように
;; なっています。


;;; ライブラリ定義情報
(require 'xdic)
(provide 'xdic-unix)
(put 'xdic-unix 'version "1.2")
(put 'xdic-unix 'init-dictionary 'xdic-unix-init-dictionary)
(put 'xdic-unix 'open-dictionary 'xdic-unix-open-dictionary)
(put 'xdic-unix 'close-dictionary 'xdic-unix-close-dictionary)
(put 'xdic-unix 'search-entry 'xdic-unix-search-entry)
(put 'xdic-unix 'get-content 'xdic-unix-get-content)


;;;----------------------------------------------------------------------
;;;		定数/変数の宣言
;;;----------------------------------------------------------------------

(defvar xdic-unix-look-command nil "*Executable file name of look")

(defvar xdic-unix-look-case-option "-f" "*Command line option for look to ignore case")

(defvar xdic-unix-grep-command nil "*Executable file name of grep")

(defvar xdic-unix-grep-case-option "-i" "*Command line option for grep to ignore case")

(defconst xdic-unix-search-buffer-name "*xdic-unix*")



;;;----------------------------------------------------------------------
;;;		本体
;;;----------------------------------------------------------------------

;; xdic-unix-*-command の初期値を設定
(mapcar '(lambda (list)
	   (or (symbol-value (car list))
	       (set (car list)
		    (catch 'which
		      (mapcar '(lambda (file)
				 (mapcar '(lambda (path)
					    (if (file-executable-p (expand-file-name file path))
						(throw 'which (expand-file-name file path))))
					 exec-path))
			      (cdr list))
		      nil))))
	'((xdic-unix-look-command "look" "look.exe")
	  (xdic-unix-grep-command "egrep" "egrep.exe" "grep" "grep.exe")))


(defun xdic-unix-available-p () "\
Function to check availability of library.
ライブラリの利用可能性を検査する関数"
  (and xdic-unix-look-command xdic-unix-grep-command t))


(defun xdic-unix-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (xdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar '(lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "xdic-unix+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (or (get dic 'look)
	      (put dic 'look xdic-unix-look-command))
	  (or (get dic 'look-case-option)
	      (put dic 'look-case-option xdic-unix-look-case-option))
	  (or (get dic 'grep)
	      (put dic 'grep xdic-unix-grep-command))
	  (or (get dic 'grep-case-option)
	      (put dic 'grep-case-option xdic-unix-grep-case-option))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system xdic-default-coding-system))
	  dic)
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun xdic-unix-open-dictionary (dic)
  "Function to open dictionary"
  (and (or (xdic-buffer-live-p (get dic 'xdic-unix-search-buffer))
	   (put dic 'xdic-unix-search-buffer (generate-new-buffer xdic-unix-search-buffer-name)))
       dic))


(defun xdic-unix-close-dictionary (dic)
  "Function to close dictionary"
  (kill-buffer (get dic 'xdic-unix-search-buffer))
  (put dic 'xdic-unix-search-buffer nil))


(defun xdic-unix-search-entry (dic string &optional search-type) "\
Function to search word with look or grep, and write results to current buffer.
search-type の値によって次のように動作を変更する。
    nil    : 前方一致検索
    t      : 後方一致検索
    lambda : 完全一致検索
    0      : 任意検索
検索結果として見つかった見出し語をキーとし、その定義文の先頭の point を値とする
連想配列を返す。
"
  (save-excursion
    (set-buffer (get dic 'xdic-unix-search-buffer))
    (save-restriction
      (if (get dic 'xdic-unix-erase-buffer)
	  (delete-region (point-min) (point-max))
	(goto-char (point-max))
	(narrow-to-region (point-max) (point-max)))
      (put dic 'xdic-unix-erase-buffer nil)
      (cond
       ;; 前方一致検索の場合 -> look を使って検索
       ((eq search-type nil)
	(if (string-match "\\Ca" string)
	    (xdic-call-process (get dic 'look) nil t nil
			       (get dic 'coding-system)
			       string (get dic 'file-name))
	  (xdic-call-process (get dic 'look) nil t nil
			     (get dic 'coding-system)
			     (get dic 'look-case-option) string (get dic 'file-name))))
       ;; 後方一致検索の場合 -> grep を使って検索
       ((eq search-type t)
	(if (string-match "\\Ca" string)
	    (xdic-call-process (get dic 'grep) nil t nil
			       (get dic 'coding-system)
			       (format "^[^\t]*%s\t" string) (get dic 'file-name))
	  (xdic-call-process (get dic 'grep) nil t nil
			     (get dic 'coding-system)
			     (get dic 'grep-case-option)
			     (format "^[^\t]*%s\t" string) (get dic 'file-name))))
       ;; 完全一致検索の場合 -> look を使って検索 / 余分なデータを消去
       ((eq search-type 'lambda)
	(if (string-match "\\Ca" string)
	    (xdic-call-process (get dic 'look) nil t nil
			       (get dic 'coding-system)
			       string (get dic 'file-name))
	  (xdic-call-process (get dic 'look) nil t nil
			     (get dic 'coding-system)
			     (get dic 'look-case-option)
			     string (get dic 'file-name)))
	(goto-char (point-min))
	(while (if (looking-at (format "%s\t" (regexp-quote string)))
		   (= 0 (forward-line 1))
		 (delete-region (point) (point-max)))))
       ;; ユーザー指定のキーによる検索の場合 -> grep を使って検索
       ((eq search-type 0)
	(xdic-call-process (get dic 'grep) nil t nil
			   (get dic 'coding-system)
			   string (get dic 'file-name)))
       ;; それ以外の検索形式を指定された場合
       (t (error "Not supported search type is specified. \(%s\)"
		 (prin1-to-string search-type))))
      ;; 各検索結果に ID を付与する
      (goto-char (point-min))
      (let (ret)
	(while (if (looking-at "\\([^\t]+\\)\t")
		   (progn
		     (setq ret (cons (cons (xdic-match-string 1) (match-end 0)) ret))
		     (= 0 (forward-line 1)))))
	(reverse ret)))))


(defun xdic-unix-get-content (dic point)
  (save-excursion
    (set-buffer (get dic 'xdic-unix-search-buffer))
    (put dic 'xdic-unix-erase-buffer t)
    (if (<= point (point-max))
	(buffer-substring (goto-char point) (progn (end-of-line) (point)))
      (error "Can't find content. (ID=%d)" point))))
