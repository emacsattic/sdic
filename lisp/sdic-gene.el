;;; -*- Emacs-Lisp -*-
;;; $Id$

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of xdic. Please see xdic.el for more detail.

;; 1行のデータの形式が
;;	見出し語 TAB 定義文 RET
;; となっている辞書を外部プログラムに頼らずに検索するライブラリ

(require 'xdic)
(provide 'xdic-gene)
(put 'xdic-gene 'version "1.2")
(put 'xdic-gene 'init-dictionary 'xdic-gene-init-dictionary)
(put 'xdic-gene 'open-dictionary 'xdic-gene-open-dictionary)
(put 'xdic-gene 'close-dictionary 'xdic-gene-close-dictionary)
(put 'xdic-gene 'search-entry 'xdic-gene-search-entry)
(put 'xdic-gene 'get-content 'xdic-gene-get-content)

(defconst xdic-gene-search-buffer-name "*xdic-gene*")



;;;----------------------------------------------------------------------
;;;		本体
;;;----------------------------------------------------------------------

(defun xdic-gene-init-dictionary (alist)
  "Function to initialize dictionary"
  (let ((dic (xdic-make-dictionary-symbol)))
    (if (file-readable-p (put dic 'file-name (or (nth 1 (assoc 'file-name alist))
						 (error "%s" "File name is not specified."))))
	(progn
	  (put dic 'coding-system (or (nth 1 (assoc 'coding-system alist))
				      (error "%s" "Coding-system is not specified.")))
	  dic)
      (error "Can't read dictionary: %s" (prin1-to-string (get dic 'file-name))))))


(defun xdic-gene-open-dictionary (dic)
  "Function to open dictionary"
  (save-excursion
    (set-buffer (or (xdic-buffer-live-p (get dic 'xdic-gene-search-buffer))
		    (put dic 'xdic-gene-search-buffer (generate-new-buffer xdic-gene-search-buffer-name))))
    (xdic-insert-file-contents (get dic 'file-name) nil nil nil nil (get dic 'coding-system))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    t))


(defun xdic-gene-close-dictionary (dic)
  "Function to close dictionary"
  (kill-buffer (get dic 'xdic-gene-search-buffer))
  (put dic 'xdic-gene-search-buffer nil))


(defun xdic-gene-search-entry (dic string &optional search-type) "\
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
    (set-buffer (get dic 'xdic-gene-search-buffer))
    (goto-char (point-min))
    (setq string (cond
		  ;; 前方一致検索の場合
		  ((eq search-type nil) (concat "^" (regexp-quote string)))
		  ;; 後方一致検索の場合
		  ((eq search-type t) (format "^\\([^\t]*%s\\)\t" (regexp-quote string)))
		  ;; 完全一致検索の場合
		  ((eq search-type 'lambda) (format "^%s\t" (regexp-quote string)))
		  ;; ユーザー指定のキーによる検索の場合
		  ((eq search-type 0) string)
		  ;; それ以外の検索形式を指定された場合
		  (t (error "Not supported search type is specified. \(%s\)"
			    (prin1-to-string search-type)))))
    (let (ret (case-fold-search t))
      (while (re-search-forward string nil t)
	(save-excursion
	  (setq ret (cons (cons (buffer-substring (progn (beginning-of-line) (point))
						  (progn (skip-chars-forward "^\t") (point)))
				(1+ (point)))
			  ret))))
      (reverse ret))))


(defun xdic-gene-get-content (dic point)
  (save-excursion
    (set-buffer (get dic 'xdic-gene-search-buffer))
    (if (<= point (point-max))
	(buffer-substring (goto-char point) (progn (end-of-line) (point)))
      (error "Can't find content. (ID=%d)" point))))
