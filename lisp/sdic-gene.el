;;; -*- Emacs-Lisp -*-
;;; $Id$

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of xdic. Please see xdic.el for more detail.

;; 1行のデータの形式が
;;	見出し語 TAB 定義文 RET
;; となっている辞書を外部プログラムに頼らずに検索するライブラリ



;;;----------------------------------------------------------------------
;;;		本体
;;;----------------------------------------------------------------------

(defun xdic-gene-open-dictionary (dic)
  "Function to open dictionary"
  (if (bufferp (get dic 'search-buffer))
      (kill-buffer (get dic 'search-buffer)))
  (save-excursion
    (set-buffer (put dic 'search-buffer (generate-new-buffer "*xdic-gene*")))
    (insert-file-contents (get dic 'file-name))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    t))


(defun xdic-gene-close-dictionary (dic)
  "Function to close dictionary"
  (kill-buffer (get dic 'search-buffer))
  (put dic 'search-buffer nil))


(defun xdic-gene-search-entry (dic string &optional search-type) "\
Function to search word with look or grep, and write results to current buffer.
search-type の値によって次のように動作を変更する。
    nil    : 前方一致検索
    t      : 後方一致検索
    lambda : 完全一致検索
検索結果として見つかった見出し語をキーとし、その定義文の先頭の point を値とする
連想配列を返す。
"
  (save-excursion
    (set-buffer (get dic 'search-buffer))
    (goto-char (point-min))
    (setq string (cond
		  ;; 前方一致検索の場合
		  ((eq search-type nil)
		   (format "^\\(%s[^\t]*\\)\t" (regexp-quote string)))
		  ;; 後方一致検索の場合
		  ((eq search-type t)
		   (format "^\\([^\t]*%s\\)\t" (regexp-quote string)))
		  ;; 完全一致検索の場合
		  ((eq search-type 'lambda)
		   (format "^\\(%s\\)\t" (regexp-quote string)))
		  ;; それ以外の検索形式を指定された場合
		  (t (error "Not supported search type is specified. \(%s\)"
			    (prin1-to-string search-type)))))
    (let (ret)
      (while (re-search-forward string nil t)
	(setq ret (cons (cons (match-string 1) (match-end 0)) ret)))
      (reverse ret))))


(defun xdic-gene-get-content (dic point)
  (save-excursion
    (set-buffer (get dic 'search-buffer))
    (if (<= point (point-max))
	(buffer-substring (goto-char point) (progn (end-of-line) (point)))
      (error "Can't find content. (ID=%d)" point))))




;;;----------------------------------------------------------------------
;;;		定義情報
;;;----------------------------------------------------------------------

(provide 'xdic-gene)
(put 'xdic-gene 'version "1.1")
(put 'xdic-gene 'open-dictionary 'xdic-gene-open-dictionary)
(put 'xdic-gene 'close-dictionary 'xdic-gene-close-dictionary)
(put 'xdic-gene 'search-entry 'xdic-gene-search-entry)
(put 'xdic-gene 'get-content 'xdic-gene-get-content)
