;;; -*- Emacs-Lisp -*-
;;; $Id$

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of xdic. Please see xdic.el for more detail.

;; 1行のデータの形式が
;;	見出し語 TAB 定義文 RET
;; となっている辞書を外部プログラム( look / grep )を利用して検索する
;; ライブラリ



;;;----------------------------------------------------------------------
;;;		定数/変数の宣言
;;;----------------------------------------------------------------------

(defvar xdic-unix-look-command "look" "*Executable file name of look")

(defvar xdic-unix-look-case-option "-f" "*Command line option for look to ignore case")

(defvar xdic-unix-grep-command "egrep" "*Executable file name of grep")

(defvar xdic-unix-grep-case-option "-i" "*Command line option for grep to ignore case")


  

;;;----------------------------------------------------------------------
;;;		本体
;;;----------------------------------------------------------------------

(defun xdic-unix-open-dictionary (dic)
  "Function to open dictionary"
  (if (bufferp (get dic 'search-buffer))
      (kill-buffer (get dic 'search-buffer)))
  (put dic 'search-buffer (generate-new-buffer "*xdic-unix*")))


(defun xdic-unix-close-dictionary (dic)
  "Function to close dictionary"
  (kill-buffer (get dic 'search-buffer))
  (put dic 'search-buffer nil))


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
    (set-buffer (get dic 'search-buffer))
    (erase-buffer)
    (cond
     ;; 前方一致検索の場合 -> look を使って検索
     ((eq search-type nil)
      (if (string-match "\\Ca" string)
	  (xdic-call-process xdic-unix-look-command nil t nil
			     (get dic 'coding-system)
			     string (get dic 'file-name))
	(xdic-call-process xdic-unix-look-command nil t nil
			   (get dic 'coding-system)
			   xdic-unix-look-case-option string (get dic 'file-name))))
     ;; 後方一致検索の場合 -> grep を使って検索
     ((eq search-type t)
      (if (string-match "\\Ca" string)
	  (xdic-call-process xdic-unix-grep-command nil t nil
			     (get dic 'coding-system)
			     (format "^[^\t]*%s\t" string) (get dic 'file-name))
	(xdic-call-process xdic-unix-grep-command nil t nil xdic-unix-grep-case-option
			   (get dic 'coding-system)
			   (format "^[^\t]*%s\t" string) (get dic 'file-name))))
     ;; 完全一致検索の場合 -> look を使って検索 / 余分なデータを消去
     ((eq search-type 'lambda)
      (xdic-call-process xdic-unix-look-command nil t nil
			 (get dic 'coding-system)
			 string (get dic 'file-name))
      (goto-char (point-min))
      (while (if (looking-at (format "%s\t" (regexp-quote string)))
		 (= 0 (forward-line 1))
	       (delete-region (point) (point-max)))))
     ;; ユーザー指定のキーによる検索の場合 -> grep を使って検索
     ((eq search-type 0)
      (xdic-call-process xdic-unix-grep-command nil t nil
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
      (reverse ret))))


(defun xdic-unix-get-content (dic point)
  (save-excursion
    (set-buffer (get dic 'search-buffer))
    (if (<= point (point-max))
	(buffer-substring (goto-char point) (progn (end-of-line) (point)))
      (error "Can't find content. (ID=%d)" point))))




;;;----------------------------------------------------------------------
;;;		定義情報
;;;----------------------------------------------------------------------

(provide 'xdic-unix)
(put 'xdic-unix 'version "1.1")
(put 'xdic-unix 'open-dictionary 'xdic-unix-open-dictionary)
(put 'xdic-unix 'close-dictionary 'xdic-unix-close-dictionary)
(put 'xdic-unix 'search-entry 'xdic-unix-search-entry)
(put 'xdic-unix 'get-content 'xdic-unix-get-content)
