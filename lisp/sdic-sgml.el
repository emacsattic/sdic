;;; -*- Emacs-Lisp -*-
;;; $Id$

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of sdic. Please see sdic.el for more detail.

;; SDIC 形式の辞書を扱うためのライブラリです。


;;; Install:

;; (1) 辞書を適切な形式に変換して、適当な場所( 例: /usr/dict/ )に保存
;;     して下さい。辞書変換用スクリプトとして以下の Perl スクリプトが
;;     利用できます。
;;
;;         gene.perl    - GENE95 辞書
;;         edict.perl   - EDICT 辞書
;;         eijirou.perl - 英辞郎
;;
;; (2) 使えるようにした辞書の定義情報を sdic-eiwa-dictionary-list また
;;     は sdic-waei-dictionary-list に追加して下さい。
;;
;;         (setq sdic-eiwa-dictionary-list
;;               (cons '(sdic-sgml "/usr/dict/gene.dic") sdic-eiwa-dictionary-list))
;;
;;     辞書定義情報は次のような構成になっています。
;;
;;         (sdic-sgml ファイル名 (オプションA 値A) (オプションB 値B) ...)
;;
;;     特別な指定が不要な場合には、オプションは省略できます。
;;
;;         (sdic-sgml ファイル名)


;;; Options:

;; sdic-sgml.el に対して指定できるオプションは次の通りです。
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
;; extract
;;     圧縮辞書を展開するための外部コマンドを指定します。省略した場合
;;     は、辞書が圧縮されていないと見なします。
;;
;; extract-option
;;     extract オプションによって指定された外部コマンドに対して、辞書
;;     を展開して標準出力に出力させるためのコマンドライン引数を指定し
;;     ます。省略した場合は sdic-sgml-extract-option の値を使います。


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
(provide 'sdic-sgml)
(put 'sdic-sgml 'version "1.1")
(put 'sdic-sgml 'init-dictionary 'sdic-sgml-init-dictionary)
(put 'sdic-sgml 'open-dictionary 'sdic-sgml-open-dictionary)
(put 'sdic-sgml 'close-dictionary 'sdic-sgml-close-dictionary)
(put 'sdic-sgml 'search-entry 'sdic-sgml-search-entry)
(put 'sdic-sgml 'get-content 'sdic-sgml-get-content)


;;;----------------------------------------------------------------------
;;;		定数/変数の宣言
;;;----------------------------------------------------------------------

(defvar sdic-sgml-extract-option "-dc" "\
*Option for archiver.
圧縮辞書を展開するために使うオプション")

(defconst sdic-sgml-buffer-name " *sdic-sgml*")



;;;----------------------------------------------------------------------
;;;		SDIC形式を扱うためのライブラリ
;;;----------------------------------------------------------------------

(defsubst sdic-sgml-replace-string (string from to) "\
文字列 STRING に含まれている文字列 FROM を全て文字列 TO に置換した文字列を返す
FROM には正規表現を含む文字列を指定できるが、TO は固定文字列しか指定で
きないので、注意して使うこと。"
  (let ((start 0) list)
    (while (string-match from string start)
      (setq list (cons to (cons (substring string start (match-beginning 0)) list)))
      (setq start (match-end 0)))
    (apply 'concat (reverse (cons (substring string start) list)))))


(defsubst sdic-sgml-recover-string (str &optional recover-lf)
  "STR に含まれているエスケープ文字列を復元する"
  (save-match-data
    (setq str (sdic-sgml-replace-string str "&lt;" "<"))
    (setq str (sdic-sgml-replace-string str "&gt;" ">"))
    (if recover-lf
	(setq str (sdic-sgml-replace-string str "&lf;" "\n")))
    (sdic-sgml-replace-string str "&amp;" "&")))


(defun sdic-sgml-recover-region (start end &optional recover-lf)
  "リージョンに含まれているエスケープ文字列を復元する"
  (save-excursion
    (save-match-data
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(while (search-forward "&lt;" nil t)
	  (replace-match "<" t t))
	(goto-char (point-min))
	(while (search-forward "&gt;" nil t)
	  (replace-match ">" t t))
	(if recover-lf
	    (progn
	      (goto-char (point-min))
	      (while (search-forward "&lf;" nil t)
		(replace-match "\n" t t))))
	(goto-char (point-min))
	(while (search-forward "&amp;" nil t)
	  (replace-match "&" t t))
	))))


(defsubst sdic-sgml-escape-string (str &optional escape-lf)
  "STR に含まれている特殊文字をエスケープする"
  (save-match-data
    (setq str (sdic-sgml-replace-string str "&" "&amp;"))
    (if escape-lf
	(setq str (sdic-sgml-replace-string str "\n" "&lf;")))
    (setq str (sdic-sgml-replace-string str "<" "&lt;"))
    (sdic-sgml-replace-string str ">" "&gt;")))


(defun sdic-sgml-escape-region (start end &optional escape-lf)
  "リージョンに含まれている特殊文字をエスケープする"
  (save-excursion
    (save-match-data
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(while (search-forward "&" nil t)
	  (replace-match "&amp;" t t))
	(goto-char (point-min))
	(while (search-forward "<" nil t)
	  (replace-match "&lt;" t t))
	(goto-char (point-min))
	(while (search-forward ">" nil t)
	  (replace-match "&gt;" t t))
	(if escape-lf
	    (progn
	      (goto-char (point-min))
	      (while (search-forward "\n" nil t)
		(replace-match "&lf;" t t))))
	))))


(defsubst sdic-sgml-make-query-string (string search-type)
  "STR から適切な検索文字列を生成する"
  (cond
   ;; 前方一致検索の場合
   ((eq search-type nil) (concat "<K>" (sdic-sgml-escape-string (downcase string))))
   ;; 後方一致検索の場合
   ((eq search-type t) (concat (sdic-sgml-escape-string (downcase string)) "</K>"))
   ;; 完全一致検索の場合
   ((eq search-type 'lambda) (concat "<K>" (sdic-sgml-escape-string (downcase string)) "</K>"))
   ;; 全文検索の場合
   ((eq search-type 0) (sdic-sgml-escape-string string))
   ;; それ以外の検索形式を指定された場合
   (t (error "Not supported search type is specified. \(%s\)"
	     (prin1-to-string search-type)))))


(defsubst sdic-sgml-get-entry (&optional add-keys-to-headword)
  "現在行から見出し語を取り出し、説明文の先頭の位置を求める。"
  (save-excursion
    (save-match-data
      (let ((start (progn (beginning-of-line) (point)))
	    (point))
	(end-of-line)
	(if (search-backward "</H>" start t)
	    (progn
	      (setq point (match-beginning 0))
	      (search-backward "<H>" start))
	  (search-backward "</K>" start)
	  (setq point (match-beginning 0))
	  (search-backward "<K>" start))
	(cons (sdic-sgml-recover-string
	       (if (and add-keys-to-headword (> (match-beginning 0) start))
		   (format "%s [%s]"
			   (buffer-substring (match-end 0) point)
			   (sdic-sgml-replace-string (buffer-substring (+ start 3)
								       (- (match-beginning 0) 4))
						     "</K><K>" "]["))
		 (buffer-substring (match-end 0) point)))
	      (+ 4 point))
	))))



;;;----------------------------------------------------------------------
;;;		本体
;;;----------------------------------------------------------------------

(defun sdic-sgml-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (sdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar '(lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "sdic-sgml+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (if (get dic 'extract)
	      (or (get dic 'extract-option)
		  (put dic 'extract-option sdic-sgml-extract-option)))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system sdic-default-coding-system))
	  dic)
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun sdic-sgml-open-dictionary (dic)
  "Function to open dictionary"
  (if (or (sdic-buffer-live-p (get dic 'sdic-sgml-buffer))
	  (save-excursion
	    (set-buffer (put dic 'sdic-sgml-buffer (generate-new-buffer sdic-sgml-buffer-name)))
	    (prog1 (if (get dic 'extract)
		       (= 0 (sdic-call-process (get dic 'extract) nil t nil
					       (get dic 'coding-system)
					       (get dic 'extract-option)
					       (get dic 'file-name)))
		     (condition-case err
			 (sdic-insert-file-contents (get dic 'file-name) (get dic 'coding-system))
		       (error nil)))
	      (setq buffer-read-only t)
	      (set-buffer-modified-p nil))))
      dic))


(defun sdic-sgml-close-dictionary (dic)
  "Function to close dictionary"
  (kill-buffer (get dic 'sdic-sgml-buffer))
  (put dic 'sdic-sgml-buffer nil))


(defun sdic-sgml-search-entry (dic string &optional search-type) "\
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
    (goto-char (point-min))
    (setq string (sdic-sgml-make-query-string string search-type))
    (let ((case-fold-search nil)
	  (add-keys (get dic 'add-keys-to-headword))
	  ret)
      (while (search-forward string nil t)
	(setq ret (cons (sdic-sgml-get-entry add-keys) ret)))
      (reverse ret))))


(defun sdic-sgml-get-content (dic point)
  (save-excursion
    (set-buffer (get dic 'sdic-sgml-buffer))
    (if (<= point (point-max))
	(sdic-sgml-recover-string (buffer-substring (goto-char point)
						    (progn (end-of-line) (point))))
      (error "Can't find content. (ID=%d)" point))))
