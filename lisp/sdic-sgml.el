;;; -*- Emacs-Lisp -*-
;;; $Id$

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of xdic. Please see xdic.el for more detail.

;; XDIC 形式の辞書を扱うためのライブラリです。


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
;;               (cons '(xdic-sgml "/usr/dict/gene.dic") xdic-eiwa-dictionary-list))
;;
;;     辞書定義情報は次のような構成になっています。
;;
;;         (xdic-sgml ファイル名 (オプションA 値A) (オプションB 値B) ...)
;;
;;     特別な指定が不要な場合には、オプションは省略できます。
;;
;;         (xdic-sgml ファイル名)


;;; Options:

;; xdic-sgml.el に対して指定できるオプションは次の通りです。
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
;;     ます。省略した場合は xdic-sgml-extract-option の値を使います。


;;; Format:

;; XDIC 形式の辞書は次のような構造になっています。
;;
;;     file     := line+
;;     line     := key* headkey content | key+ headword content
;;     headkey  := key
;;     key      := '<K>' word '</K>'
;;     headword := '<H>' word '<H>'
;;     word     := char+
;;     char     := [^<>&] | '&lt;' | '&gt;' | '&amp;'
;;     content  := string
;;     string   := char_lf+
;;     char_lf  := char | '&lf;'
;;
;; この辞書フォーマットは、grep などの行指向の検索プログラムを使って前
;; 方一致検索及び後方一致検索、完全一致検索、全文検索が容易に行なえる
;; ように設計しました。
;;
;; (1) タグは大文字小文字を区別する。アルファベットの大文字と小文字を区別
;;     しない検索( case-ignore search )が実装されていない検索プログラムを
;;     利用する可能性があるため。
;;
;; (2) 元々の辞書中に現れるメタキャラクタ <>& はそれぞれ &lt; &gt; &amp; 
;;     に置換する。従って、string には <> は現れない。辞書の説明文に含ま
;;     れている改行コードは &lf; に置換する。
;;
;; (3) headword は見出し語を保持する構文要素である。
;;
;; (4) content は説明文を保持する構文要素である。
;;
;; (5) key は辞書の見出し語検索の対象となる構文要素である。従って、
;;     headword に以下の正規化を施した文字列を代入する。
;;
;;       ・アルファベットの大文字を全て小文字に変換する
;;       ・複数の空白文字を1つのスペースに置換する
;;
;;     正規化した文字列が headword と一致する場合、その文字列を headkey 
;;     として headword を省略することが出来る。これは辞書の大きさを節約す
;;     るためである。headkey は複数の key の最後に置かれる。


;;; ライブラリ定義情報
(require 'xdic)
(provide 'xdic-sgml)
(put 'xdic-sgml 'version "1.0")
(put 'xdic-sgml 'init-dictionary 'xdic-sgml-init-dictionary)
(put 'xdic-sgml 'open-dictionary 'xdic-sgml-open-dictionary)
(put 'xdic-sgml 'close-dictionary 'xdic-sgml-close-dictionary)
(put 'xdic-sgml 'search-entry 'xdic-sgml-search-entry)
(put 'xdic-sgml 'get-content 'xdic-sgml-get-content)


;;;----------------------------------------------------------------------
;;;		定数/変数の宣言
;;;----------------------------------------------------------------------

(defvar xdic-sgml-extract-option "-dc" "\
*Option for archiver.
圧縮辞書を展開するために使うオプション")

(defconst xdic-sgml-buffer-name "*xdic-sgml*")



;;;----------------------------------------------------------------------
;;;		本体
;;;----------------------------------------------------------------------

(defun xdic-sgml-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (xdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar '(lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "xdic-sgml+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (if (get dic 'extract)
	      (or (get dic 'extract-option)
		  (put dic 'extract-option xdic-sgml-extract-option)))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system xdic-default-coding-system))
	  dic)
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun xdic-sgml-open-dictionary (dic)
  "Function to open dictionary"
  (if (or (xdic-buffer-live-p (get dic 'xdic-sgml-buffer))
	  (save-excursion
	    (set-buffer (put dic 'xdic-sgml-buffer (generate-new-buffer xdic-sgml-buffer-name)))
	    (prog1 (if (get dic 'extract)
		       (= 0 (xdic-call-process (get dic 'extract) nil t nil
					       (get dic 'coding-system)
					       (get dic 'extract-option)
					       (get dic 'file-name)))
		     (condition-case err
			 (xdic-insert-file-contents (get dic 'file-name) nil nil nil nil (get dic 'coding-system))
		       (error nil)))
	      (setq buffer-read-only t)
	      (set-buffer-modified-p nil))))
      dic))


(defun xdic-sgml-close-dictionary (dic)
  "Function to close dictionary"
  (kill-buffer (get dic 'xdic-sgml-buffer))
  (put dic 'xdic-sgml-buffer nil))


(defun xdic-sgml-search-entry (dic string &optional search-type) "\
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
    (set-buffer (get dic 'xdic-sgml-buffer))
    (goto-char (point-min))
    (setq string (cond
		  ;; 前方一致検索の場合
		  ((eq search-type nil) (concat "<K>" (regexp-quote (downcase string))))
		  ;; 後方一致検索の場合
		  ((eq search-type t) (concat (regexp-quote (downcase string)) "</K>"))
		  ;; 完全一致検索の場合
		  ((eq search-type 'lambda) (concat "<K>" (regexp-quote (downcase string)) "</K>"))
		  ;; ユーザー指定のキーによる検索の場合
		  ((eq search-type 0) string)
		  ;; それ以外の検索形式を指定された場合
		  (t (error "Not supported search type is specified. \(%s\)"
			    (prin1-to-string search-type)))))
    (let ((case-fold-search nil) ret)
      (while (re-search-forward string nil t)
	(setq ret (cons (xdic-sgml-get-entry) ret)))
      (reverse ret))))


(defun xdic-sgml-recover-string (str)
  "STR に含まれているエスケープ文字列を復元する"
  (while (string-match "&lt;" str)
    (setq str (replace-match "<" t t str)))
  (while (string-match "&gt;" str)
    (setq str (replace-match ">" t t str)))
  (while (string-match "&amp;" str)
    (setq str (replace-match "&" t t str)))
  (while (string-match "&lf;" str)
    (setq str (replace-match "\n" t t str)))
  str)


(defun xdic-sgml-get-entry ()
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
	(cons (xdic-sgml-recover-string (buffer-substring (match-end 0) point))
	      (+ 4 point))
	))))


(defun xdic-sgml-get-content (dic point)
  (save-excursion
    (set-buffer (get dic 'xdic-sgml-buffer))
    (if (<= point (point-max))
	(xdic-sgml-recover-string (buffer-substring (goto-char point)
						    (progn (end-of-line) (point))))
      (error "Can't find content. (ID=%d)" point))))
