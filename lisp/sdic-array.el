;;; -*- Emacs-Lisp -*-
;;; $Id$

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of sdic. Please see sdic.el for more detail.

;; SDIC 形式の辞書を array を利用して検索するライブラリです。


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
;;         edict.perl   - EDICT 辞書
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
;; (4) 使えるようにした辞書の定義情報を sdic-eiwa-dictionary-list また
;;     は sdic-waei-dictionary-list に追加して下さい。
;;
;;         (setq sdic-eiwa-dictionary-list
;;               (cons '(sdic-array "/usr/dict/gene.dic") sdic-eiwa-dictionary-list))
;;
;;     辞書定義情報は次のような構成になっています。
;;
;;         (sdic-array ファイル名 (オプションA 値A) (オプションB 値B) ...)
;;
;;     特別な指定が不要な場合には、オプションは省略できます。
;;
;;         (sdic-array ファイル名)


;;; Options:

;; sdic-array.el に対して指定できるオプションは次の通りです。
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
;;     sdic-array-command の値を使います。
;;
;; array-file-name
;;     辞書の array file の名前を指定します。省略した場合は、外部コマ
;;     ンドのデフォルト値が使われます。


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
(provide 'sdic-array)
(put 'sdic-array 'version "2.0")
(put 'sdic-array 'init-dictionary 'sdic-array-init-dictionary)
(put 'sdic-array 'open-dictionary 'sdic-array-open-dictionary)
(put 'sdic-array 'close-dictionary 'sdic-array-close-dictionary)
(put 'sdic-array 'search-entry 'sdic-array-search-entry)
(put 'sdic-array 'get-content 'sdic-array-get-content)


;;;----------------------------------------------------------------------
;;;		定数/変数の宣言
;;;----------------------------------------------------------------------

(defvar sdic-array-command
  (catch 'which
    (mapcar '(lambda (file)
	       (mapcar '(lambda (path)
			  (if (file-executable-p (expand-file-name file path))
			      (throw 'which (expand-file-name file path))))
		       exec-path))
	    '("array" "array.exe")))
  "*Executable file name of array")

(defvar sdic-array-wait-prompt-flag nil)

(defconst sdic-array-buffer-name " *sdic-array*")



;;;----------------------------------------------------------------------
;;;		本体
;;;----------------------------------------------------------------------

(defun sdic-array-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (sdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar '(lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "sdic-array+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (or (get dic 'command)
	      (put dic 'command sdic-array-command))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system sdic-default-coding-system))
	  dic)
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun sdic-array-open-dictionary (dic)
  "Function to open dictionary"
  (let	((old-buffer (current-buffer))
	 (buf (or (sdic-buffer-live-p (get dic 'sdic-sgml-buffer))
		  (put dic 'sdic-sgml-buffer (generate-new-buffer sdic-array-buffer-name)))))
    (unwind-protect
	(and (or (sdic-array-process-live-p dic)
		 (let ((limit (progn (set-buffer buf) (goto-char (point-max))))
		       (proc (sdic-start-process "array" buf
						 (get dic 'coding-system)
						 (get dic 'command)
						 (get dic 'file-name)
						 (or (get dic 'array-file-name) ""))))
		   (accept-process-output proc 5)
		   (if (search-backward "ok\n" limit t)
		       (progn
			 (set-process-filter proc 'sdic-array-wait-prompt)
			 (process-kill-without-query proc)
			 (sdic-array-send-string proc "style line")
			 t))))
	     dic)
      (set-buffer old-buffer))))


(defun sdic-array-close-dictionary (dic)
  "Function to close dictionary"
  (let ((proc (sdic-array-process-live-p dic)))
    (if proc
	(progn
	  (set-process-filter proc nil)
	  (process-send-string proc "quit\n"))))
  (kill-buffer (get dic 'sdic-sgml-buffer))
  (put dic 'sdic-sgml-buffer nil))


(defun sdic-array-search-entry (dic string &optional search-type) "\
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
    (let* ((buf (set-buffer (get dic 'sdic-sgml-buffer)))
	   (proc (get-buffer-process buf))
	   (add-keys (get dic 'add-keys-to-headword))
	   pos ret cons end lst)
      (if (get dic 'sdic-array-erase-buffer)
	  (delete-region (point-min) (point-max))
	(goto-char (point-max)))
      (put dic 'sdic-array-erase-buffer nil)
      (sdic-array-send-string proc "init") ; 検索条件を初期化
      (setq pos (point))
      (sdic-array-send-string proc (format "search %s" (sdic-sgml-make-query-string string search-type)))
      (if (re-search-backward "^FOUND: [0-9]+$" pos t)
	  (progn
	    (setq pos (+ 3 (match-end 0)))
	    (sdic-array-send-string proc "show")
	    ;; 各検索結果に ID を付与する
	    (goto-char pos)
	    (while (progn
		     (setq cons (sdic-sgml-get-entry add-keys)
			   end (progn (end-of-line) (point)))
		     (if cons
			 (or (and (setq lst (assoc (car cons) ret))
				  (= 0 (compare-buffer-substrings buf pos end
								  buf (nth 2 lst) (nth 3 lst))))
			     (setq ret (cons (list (car cons) (cdr cons) pos end) ret))))
		     (if (eobp) nil (goto-char (1+ (point)))))
	      (setq pos (point)))
	    (mapcar (function (lambda (l) (cons (car l) (nth 1 l)))) (reverse ret)))))))


(defun sdic-array-get-content (dic point)
  (put dic 'sdic-array-erase-buffer t)
  (sdic-sgml-get-content dic point))


(defun sdic-array-process-live-p (dic)
  (let ((proc (get-buffer-process (get dic 'sdic-sgml-buffer))))
    (and (processp proc)
	 (eq (process-status proc) 'run)
	 proc)))


(defun sdic-array-send-string (proc string) "\
Send STRING as command to process."
  (setq string (concat string "\n"))
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let ((sdic-array-wait-prompt-flag t))
	  (set-buffer (process-buffer proc))
	  (goto-char (point-max))
	  (insert string)
	  (set-marker (process-mark proc) (point))
	  (process-send-string proc string)
	  (while sdic-array-wait-prompt-flag (accept-process-output proc)))
      (set-buffer old-buffer))))


(defun sdic-array-wait-prompt (proc string) "\
Process filter function of Array.
プロンプトが現れたことを検知して、sdic-array-wait-prompt-flag を nil 
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
		  (setq sdic-array-wait-prompt-flag nil))
	      (goto-char start))))
      (set-buffer old-buffer))))
