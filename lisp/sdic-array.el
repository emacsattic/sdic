;;; -*- Emacs-Lisp -*-
;;; $Id$

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of xdic. Please see xdic.el for more detail.

;; XDIC 形式の辞書を array を利用して検索するライブラリです。


;;; Install:

;; (1) array とは、SUFARY に附属している対話型検索プログラムです。
;;     SUFARY については以下の URL を参照して下さい。
;;
;;         http://cactus.aist-nara.ac.jp/lab/nlt/ss/
;;
;;     検索対象のテキストの索引を事前に作成しておくタイプの検索プログ
;;     ラムなので、grep よりも高速な検索が可能です。
;;
;;     附属文書の指示にしたがって、array と mkary をインストールして下
;;     さい。
;;
;; (2) 辞書を適切な形式に変換して、適当な場所( 例: /usr/dict/ )に保存
;;     して下さい。辞書変換用スクリプトとして以下の Perl スクリプトが
;;     利用できます。
;;
;;         gene.perl    - GENE95 辞書
;;         jgene.perl   - GENE95 辞書から和英辞書を生成する
;;         eijirou.perl - 英辞郎
;;
;; (3) 辞書の索引を生成します。/usr/dict/gene.dic の索引を生成する場合
;;     は、次のようにコマンドを入力して下さい。
;;
;;         mkary /usr/dict/gene.dic
;;
;;     すると、/usr/dict/gene.dic.ary が生成されます。
;;
;;
;; (4) 使えるようにした辞書の定義情報を xdic-eiwa-dictionary-list また
;;     は xdic-waei-dictionary-list に追加して下さい。
;;
;;         (setq xdic-eiwa-dictionary-list
;;               (cons '(xdic-array "/usr/dict/gene.dic") xdic-eiwa-dictionary-list))
;;
;;     辞書定義情報は次のような構成になっています。
;;
;;         (xdic-array ファイル名 (オプションA 値A) (オプションB 値B) ...)
;;
;;     特別な指定が不要な場合には、オプションは省略できます。
;;
;;         (xdic-array ファイル名)


;;; Options:

;; xdic-array.el に対して指定できるオプションは次の通りです。
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
;;     xdic-array-command の値を使います。


;;; ライブラリ定義情報
(require 'xdic)
(require 'xdic-sgml)
(provide 'xdic-array)
(put 'xdic-array 'version "1.0")
(put 'xdic-array 'init-dictionary 'xdic-array-init-dictionary)
(put 'xdic-array 'open-dictionary 'xdic-array-open-dictionary)
(put 'xdic-array 'close-dictionary 'xdic-array-close-dictionary)
(put 'xdic-array 'search-entry 'xdic-array-search-entry)
(put 'xdic-array 'get-content 'xdic-array-get-content)


;;;----------------------------------------------------------------------
;;;		定数/変数の宣言
;;;----------------------------------------------------------------------

(defvar xdic-array-command
  (catch 'which
    (mapcar '(lambda (file)
	       (mapcar '(lambda (path)
			  (if (file-executable-p (expand-file-name file path))
			      (throw 'which (expand-file-name file path))))
		       exec-path))
	    '("array" "array.exe")))
  "*Executable file name of array")

(defvar xdic-array-wait-prompt-flag nil)

(defconst xdic-array-buffer-name "*xdic-array*")



;;;----------------------------------------------------------------------
;;;		本体
;;;----------------------------------------------------------------------

(defun xdic-array-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (xdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar '(lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "xdic-array+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (or (get dic 'command)
	      (put dic 'command xdic-array-command))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system xdic-default-coding-system))
	  dic)
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun xdic-array-open-dictionary (dic)
  "Function to open dictionary"
  (let	((old-buffer (current-buffer))
	 (buf (or (xdic-buffer-live-p (get dic 'xdic-sgml-buffer))
		  (put dic 'xdic-sgml-buffer (generate-new-buffer xdic-array-buffer-name)))))
    (unwind-protect
	(and (or (xdic-array-process-live-p dic)
		 (let ((limit (progn (set-buffer buf) (goto-char (point-max))))
		       (proc (start-process "array" buf
					    (get dic 'command)
					    (get dic 'file-name)
					    (or (get dic 'array-file-name) ""))))
		   (accept-process-output proc 5)
		   (if (search-backward "ok\n" limit t)
		       (progn
			 (set-process-coding-system proc
						    (get dic 'coding-system)
						    (get dic 'coding-system))
			 (set-process-filter proc 'xdic-array-wait-prompt)
			 (xdic-array-send-string proc "style line")
			 t))))
	     dic)
      (set-buffer old-buffer))))


(defun xdic-array-close-dictionary (dic)
  "Function to close dictionary"
  (let ((proc (xdic-array-process-live-p dic)))
    (if proc
	(progn
	  (set-process-filter proc nil)
	  (process-send-string proc "quit\n"))))
  (kill-buffer (get dic 'xdic-sgml-buffer))
  (put dic 'xdic-sgml-buffer nil))


(defun xdic-array-search-entry (dic string &optional search-type) "\
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
    (let* ((buf (set-buffer (get dic 'xdic-sgml-buffer)))
	   (proc (get-buffer-process buf))
	   limit ret)
      (if (get dic 'xdic-array-erase-buffer)
	  (delete-region (point-min) (point-max))
	(goto-char (point-max)))
      (put dic 'xdic-array-erase-buffer nil)
      (xdic-array-send-string proc "init") ; 検索条件を初期化
      (setq limit (point))
      (xdic-array-send-string proc
			       (format "search %s"
				       (cond
					;; 前方一致検索
					((eq search-type nil) (concat "<K>" (downcase string)))
					;; 後方一致検索
					((eq search-type t) (concat (downcase string) "</K>"))
					;; 完全一致検索
					((eq search-type 'lambda) (concat "<K>" (downcase string) "</K>"))
					;; ユーザー指定のキーによる検索
					((eq search-type 0) string)
					;; それ以外の検索形式を指定された場合
					(t (error "Not supported search type is specified. \(%s\)"
						  (prin1-to-string search-type))))))
      (if (re-search-backward "^FOUND: [0-9]+$" limit t)
	  (progn
	    (setq limit (+ 3 (match-end 0)))
	    (xdic-array-send-string proc "show")
	    ;; 各検索結果に ID を付与する
	    (goto-char limit)
	    (while (progn
		     (if (looking-at "<K>")
			 (setq ret (cons (xdic-sgml-get-entry) ret)))
		     (= 0 (forward-line 1))))
	    (reverse ret))))))


(defun xdic-array-get-content (dic point)
  (put dic 'xdic-array-erase-buffer t)
  (xdic-sgml-get-content dic point))


(defun xdic-array-process-live-p (dic)
  (let ((proc (get-buffer-process (get dic 'xdic-sgml-buffer))))
    (and (processp proc)
	 (eq (process-status proc) 'run)
	 proc)))


(defun xdic-array-send-string (proc string) "\
Send STRING as command to process."
  (setq string (concat string "\n"))
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let ((xdic-array-wait-prompt-flag t))
	  (set-buffer (process-buffer proc))
	  (goto-char (point-max))
	  (insert string)
	  (set-marker (process-mark proc) (point))
	  (process-send-string proc string)
	  (while xdic-array-wait-prompt-flag (accept-process-output proc)))
      (set-buffer old-buffer))))


(defun xdic-array-wait-prompt (proc string) "\
Process filter function of Array.
プロンプトが現れたことを検知して、xdic-array-wait-prompt-flag を nil 
にする。"
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(save-match-data ; Emacs-19.34 以降は自動的に検索結果の待避/回復が行われるので不要
	  (set-buffer (process-buffer proc))
	  (let ((start (point)))
	    (goto-char (process-mark proc))
	    (insert string)
	    (set-marker (process-mark proc) (point))
	    (skip-chars-backward " \t\n")
	    (beginning-of-line)
	    (if (looking-at "ok\n")
		(progn
		  (goto-char (match-end 0))
		  (setq xdic-array-wait-prompt-flag nil))
	      (goto-char start)))))))
