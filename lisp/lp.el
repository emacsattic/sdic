;;; -*- Emacs-Lisp -*-
;;; $Id$

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: dictionary

;;; Commentary:

;; This file is a part of sdic. Please see sdic.el for more detail.

;; このファイルは、GENE辞書を英和辞書として使い、EDICT辞書を和英辞書と
;; して使うための設定を行なう Emacs-Lisp プログラムです。主に Windows 
;; 環境で利用することを想定しています。UNIX 環境でインストールを行なう
;; 場合は、普通に Makefile を使って下さい。
;;
;; ここでは、以下のディレクトリにファイルを置く場合のインストール手順
;; について説明します。この手順では、Meadow または Mule for windows 以
;; 外のコマンドを必要としません。
;;
;;     ・辞書をインストールするディレクトリ -> C:\Dict
;;     ・Emacs-Lisp プログラムをインストールするディレクトリ -> C:\Meadow\site-lisp
;;     ・Info をインストールするディレクトリ -> C:\Meadow\info
;;
;; (1) GENE辞書のデータ gene95.lzh または gene95.tar.gz を展開して、
;;     gene.txt をこのファイルと同じディレクトリに置いて下さい。
;;
;; (2) EDICT辞書のデータ edict.gz を展開して、edict をこのファイルと同
;;     じディレクトリに置いて下さい。
;;
;; (3) 以下のコマンドを実行して下さい。
;;
;;         mule -batch -q -l ./lp.el -f make-sdic C:\Meadow\site-lisp C:\Meadow\info C:\Dict
;;
;;     適当に引数を変更することによって、インストール先を変えられます。
;;
;; (4) 以下の設定を .emacs に付け加えて下さい。
;;
;;         (setq load-path (cons "C:/Meadow/site-lisp" load-path))
;;         (autoload 'sdic "sdic" "英単語の意味を調べる" t nil)
;;         (global-set-key "\C-cw" 'sdic)
;;
;;     キーバインドは適当に変更して下さい。


(setq load-path (cons (expand-file-name default-directory) load-path))
(and (boundp 'emacs-major-version)
     (>= emacs-major-version 20)
     (set-language-environment "Japanese"))



(defvar make-sdic-lisp-directory "C:/Meadow/site-lisp"
  "Lisp program をインストールするディレクトリ")

(defvar make-sdic-info-directory "C:/Meadow/info"
  "Info をインストールするディレクトリ")

(defvar make-sdic-dict-directory make-sdic-lisp-directory
  "辞書をインストールするディレクトリ")

(defvar make-sdic-gene-coding-system (if (>= emacs-major-version 20) 'sjis-dos *autoconv*)
  "配布されているGENE辞書の漢字コード")

(defvar make-sdic-edict-coding-system (if (>= emacs-major-version 20) 'euc-japan-unix *autoconv*)
  "配布されているEDICT辞書の漢字コード")

(defvar make-sdic-default-directory default-directory)



(defun make-sdic ()
  (defvar command-line-args-left)	;Avoid 'free variable' warning
  (if (not noninteractive)
      (error "`make-sdic' is to be used only with -batch"))
  (load "sdic.el.in")
  (require 'sdic-sgml)
  (or (= 3 (length command-line-args-left))
      (error "%s" "Illegal arguments"))
  (or (string= "NONE" (car command-line-args-left))
      (setq make-sdic-lisp-directory (car command-line-args-left)))
  (or (string= "NONE" (nth 1 command-line-args-left))
      (setq make-sdic-info-directory (nth 1 command-line-args-left)))
  (or (string= "NONE" (nth 2 command-line-args-left))
      (setq make-sdic-dict-directory (nth 2 command-line-args-left)))
  (or (file-directory-p make-sdic-lisp-directory)
      (error "Can't find directory : %s" make-sdic-lisp-directory))
  (or (file-directory-p make-sdic-info-directory)
      (error "Can't find directory : %s" make-sdic-info-directory))
  (or (file-directory-p make-sdic-dict-directory)
      (error "Can't find directory : %s" make-sdic-dict-directory))
  (make-sdic-sdic_el)
  (make-sdic-install-lisp)
  (make-sdic-install-info)
  (make-sdic-install-dictionary))


(defun make-sdic-install-lisp ()
  (mapcar '(lambda (basename)
	     (let ((in-file (expand-file-name basename make-sdic-default-directory))
		   (out-file (expand-file-name basename make-sdic-lisp-directory)))
	       (message in-file)
	       (byte-compile-file in-file)
	       (copy-file in-file out-file t t)
	       (copy-file (concat in-file "c") (concat out-file "c") t t)))
	  (directory-files make-sdic-default-directory nil "^\\(sdic.*\\|stem\\)\\.el$")))


(defun make-sdic-install-info ()
  (require 'texinfmt)
  (let ((in-file (expand-file-name "sdic.texi" make-sdic-default-directory))
	(out-file (expand-file-name "sdic.info" make-sdic-default-directory))
	(copy-file (expand-file-name "sdic.info" make-sdic-info-directory)))
    (or (file-readable-p in-file) (error "Can't find file : %s" in-file))
    (let ((buf (generate-new-buffer "*sdic.texi*")))
      (unwind-protect
	  (progn
	    (set-buffer buf)
	    (insert-file-contents in-file)
	    (texinfo-format-buffer)
	    (save-buffer))
	(kill-buffer buf)))
    (copy-file in-file copy-file t t)))


(defun make-sdic-install-dictionary ()
  (let* ((in-file (expand-file-name "gene.txt" make-sdic-default-directory))
	 (out-file (expand-file-name "gene.sdic" make-sdic-default-directory))
	 (copy-file (expand-file-name "gene.sdic" make-sdic-dict-directory)))
    (message out-file)
    (or (file-readable-p in-file) (error "Can't find file : %s" in-file))
    (make-sdic-gene in-file out-file)
    (copy-file out-file copy-file t t))
  (let* ((in-file (expand-file-name "edict" make-sdic-default-directory))
	 (out-file (expand-file-name "jedict.sdic" make-sdic-default-directory))
	 (copy-file (expand-file-name "jedict.sdic" make-sdic-dict-directory)))
    (message out-file)
    (or (file-readable-p in-file) (error "Can't find file : %s" in-file))
    (make-sdic-edict in-file out-file)
    (copy-file out-file copy-file t t)))

	   
(defun make-sdic-sdic_el ()
  (let ((in-file (expand-file-name "sdic.el.in" make-sdic-default-directory))
	(out-file (expand-file-name "sdic.el" make-sdic-default-directory)))
    (message out-file)
    (or (file-readable-p in-file) (error "Can't find file : %s" in-file))
    (let ((buf (generate-new-buffer "*sdic.el*")))
      (unwind-protect
	  (progn
	    (set-buffer buf)
	    (sdic-insert-file-contents in-file sdic-default-coding-system)
	    (goto-char (point-min))
	    (search-forward "%%EIWA_DICTIONARY%%")
	    (delete-region (goto-char (match-beginning 0)) (match-end 0))
	    (insert (expand-file-name "gene.sdic" make-sdic-dict-directory))
	    (goto-char (point-min))
	    (search-forward "%%WAEI_DICTIONARY%%")
	    (delete-region (goto-char (match-beginning 0)) (match-end 0))
	    (insert (expand-file-name "jedict.sdic" make-sdic-dict-directory))
	    (make-sdic-write-file "sdic.el"))
	(kill-buffer buf)))))


(defun make-sdic-write-file (output-file)
  (let ((buffer-file-coding-system sdic-default-coding-system)
	(file-coding-system sdic-default-coding-system))
    (message "Writing %s..." output-file)
    (write-region (point-min) (point-max) output-file)))


(defun make-sdic-gene (input-file &optional output-file)
  "GENE辞書をSDIC形式に変換する"
  (interactive "fInput dictionary file name: ")
  (or output-file
      (setq output-file (concat (if (string-match "\\.[^\\.]+$" input-file)
				    (substring input-file 0 (match-beginning 0))
				  input-file)
				".sdic")))
  (let ((buf (generate-new-buffer "*gene*")))
    (unwind-protect
	(save-excursion
	  (set-buffer buf)
	  (message "Reading %s..." input-file)
	  (sdic-insert-file-contents input-file make-sdic-gene-coding-system)
	  (message "Converting %s..." input-file)
	  ;; 最初の2行はコメントだから、行頭に # を挿入する
	  (goto-char (point-min))
	  (insert "# ")
	  (forward-line)
	  (beginning-of-line)
	  (insert "# ")
	  (forward-line)
	  (beginning-of-line)
	  (save-restriction
	    (narrow-to-region (point) (point-max))
	    (sdic-sgml-escape-region (point-min) (point-max))
	    (let (head list key top)
	      (while (progn
		       (setq top (point))
		       (end-of-line)
		       (delete-region (point) (progn (skip-chars-backward "[ \t\f\r]") (point)))
		       (setq head (buffer-substring top (point))
			     key (sdic-sgml-replace-string (downcase head) "\\s-+" " "))
		       (if (string-match " +\\+[0-9]+$" key)
			   (setq key (substring key 0 (match-beginning 0))))
		       (beginning-of-line)
		       (if (string= head key)
			   (progn
			     (insert "<K>")
			     (end-of-line)
			     (insert "</K>"))
			 (insert "<H>")
			 (end-of-line)
			 (insert "</H><K>" key "</K>"))
		       (delete-char 1)
		       (end-of-line)
		       (forward-char)
		       (not (eobp)))))
	    (message "Sorting %s..." input-file)
	    (sort-lines nil (point-min) (point-max)))
	  (make-sdic-write-file output-file))
      (kill-buffer buf))))


(defun make-sdic-edict (input-file &optional output-file)
  "EDICT辞書をSDIC形式に変換する"
  (interactive "fInput dictionary file name: ")
  (or output-file
      (setq output-file (concat (if (string-match "\\.[^\\.]+$" input-file)
				    (substring input-file 0 (match-beginning 0))
				  input-file)
				".sdic")))
  (let ((buf (generate-new-buffer "*jedict*")))
    (unwind-protect
	(save-excursion
	  (set-buffer buf)
	  (message "Reading %s..." input-file)
	  (sdic-insert-file-contents input-file make-sdic-edict-coding-system)
	  (message "Converting %s..." input-file)
	  ;; 最初の1行はコメントだから、行頭に # を挿入する
	  (delete-region (goto-char (point-min)) (progn (forward-char 4) (point)))
	  (insert "# ")
	  (forward-line)
	  (beginning-of-line)
	  (save-restriction
	    (narrow-to-region (point) (point-max))
	    (sdic-sgml-escape-region (point-min) (point-max))
	    (while (progn
		     (insert "<K>")
		     (looking-at "\\cj+")
		     (goto-char (match-end 0))
		     (insert "</K>")
		     (delete-char 1)
		     (if (looking-at "\\[\\(\\cj+\\)\\] +")
			 (let ((key (buffer-substring (match-beginning 1) (match-end 1))))
			   (delete-region (match-beginning 0) (match-end 0))
			   (insert "<K>" key "<K>")))
		     (delete-char 1)
		     (end-of-line)
		     (backward-char)
		     (delete-char 1)
		     (forward-char)
		     (not (eobp))))
	    (message "Sorting %s..." input-file)
	    (sort-lines nil (point-min) (point-max)))
	  (make-sdic-write-file output-file))
      (kill-buffer buf))))
