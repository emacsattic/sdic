;;; sdicf.el --- Search library for SDIC format dictionary

;; Copyright (C) 1999 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Author: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;	   NISHIDA Keisuke <knishida@ring.aist.go.jp>
;; Maintainer: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; Created: $Date$
;; Version: $Revision$
;; Keywords: dictionary

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; SDIC 形式辞書からの検索用ライブラリ。次の関数から成る。

;;     sdicf-open           - SDIC 辞書のオープン
;;     sdicf-close          - SDIC 辞書のクローズ
;;     sdicf-search         - SDIC 辞書から検索
;;     sdicf-entry-headword - エントリの見出し語を得る
;;     sdicf-entry-keywords - エントリの検索キーのリストを得る
;;     sdicf-entry-text     - エントリの本文を得る

;; 関数 `sdicf-open' により辞書をオープンする。引数として辞書ファイルを
;; 指定する。オプション引数 STRATEGY によって検索の方式を指定出来る。
;; 省略した場合は、次の手順によって自動的に決定する。
;; 
;; 1) 辞書ファイルに ".ary" を付加したファイルが存在するなら `array' 方式。
;; 2) fgrep もしくは grep コマンドが実行パスに存在するなら `grep' 方式。
;; 3) どちらでもなければ `direct' 方式。
;; 
;; オープンに成功すれば辞書オブジェクト(sdic)が返される。失敗すればエラー。

;; 関数 `sdicf-search' により辞書から検索を行なう。全文検索を指定した場合、
;; `array' 方式以外では検索語の大文字小文字を無視して検索する。見付かった
;; エントリ・オブジェクト(entry)のリストを返す。
;; 
;; エントリの情報は関数 `sdicf-entry-headword' と `sdicf-entry-text' により
;; 得られる。それぞれ見出し語と本文を返す。このとき変数 
;; `sdicf-headword-with-keyword' が non-nil であれば、見出し語にキーワードを
;; 追加する。

;; 関数 `sdicf-close' により辞書をクローズする。

;; 各種変数:
;; 
;; sdicf-version       - sdicf.el のバージョン
;; sdicf-grep-command  - grep コマンド
;; sdicf-array-command - array コマンド

;; 注意事項:
;; 
;; * 関数の引数チェックは完全でない。期待された値を渡すこと。
;; 
;; * 文字コードの設定は一切行なっていない。必要に応じて
;; `coding-system-for-write' や `process-coding-system-alist' などを
;; 設定するか、あるいはユーザに設定を任せること。
;; 
;; * GNU Emacs 19.30 以降であれば、`auto-compression-mode' を有効にする
;; ことで、`direct' 方式で圧縮した辞書を用いることが出来る。展開は自動で
;; 行なわれるため、特別な設定は必要でない。
;; 
;; * 速度重視のため `save-match-data' は一切用いていない。影響があるよう
;; であれば注意すること。
;; 
;; * 辞書をオープンすると、作業用バッファが生成されるが、バッファを kill
;; しても検索時には回復される。`array' 方式では、プロセスが kill されても
;; 回復する。これらの問題でエラーが起こることはない(はず)。

;;; Code:

(provide 'sdicf)
(defconst sdicf-version "0.8")

;;;
;;; Customizable variables
;;;

(defvar sdicf-grep-command
  (catch 'which
    (mapcar (lambda (file)
	      (mapcar (lambda (path)
			(if (file-executable-p (expand-file-name file path))
			    (throw 'which (expand-file-name file path))))
		      exec-path))
	    '("fgrep" "fgrep.exe" "grep" "grep.exe")))
  "*Executable file name of grep")

(defvar sdicf-array-command
  (catch 'which
    (mapcar (lambda (file)
	      (mapcar (lambda (path)
			(if (file-executable-p (expand-file-name file path))
			    (throw 'which (expand-file-name file path))))
		      exec-path))
	    '("array" "array.exe")))
  "*Executable file name of array")

;;;
;;; Internal variables
;;;

(defconst sdicf-strategy-alist
  '((direct sdicf-direct-init sdicf-direct-quit sdicf-direct-search)
    (grep sdicf-grep-init sdicf-grep-quit sdicf-grep-search)
    (array sdicf-array-init sdicf-array-quit sdicf-array-search)))

;;;
;;; Internal functions
;;;

(defsubst sdicf-object-p (sdic)
  "辞書オブジェクトかどうか検査する"
  (and (vectorp sdic) (eq 'SDIC (aref sdic 0))))

(defsubst sdicf-entry-p (entry)
  (and (stringp entry) (string-match "^<.>\\([^<]+\\)</.>" entry)))

(defsubst sdicf-get-filename (sdic)
  "辞書オブジェクトからファイル名を得る"
  (aref sdic 1))

(defsubst sdicf-get-strategy (sdic)
  "辞書オブジェクトから strategy を得る"
  (aref sdic 2))

(defsubst sdicf-get-buffer (sdic)
  "辞書オブジェクトから検索用バッファを得る"
  (aref sdic 3))

(defun sdicf-common-init (sdic)
  "共通の辞書初期化ルーチン
作業用バッファが存在することを確認し、なければ新しく生成する。"
  (or (buffer-live-p (sdicf-get-buffer sdic))
      (aset sdic 3 (generate-new-buffer (format " *sdic %s*" (sdicf-get-filename sdic))))))

(defun sdicf-search-internal () "\
行をチェックし、エントリならばリストに加える
この関数を呼び出す前に、(let (entries) ... とした上で、ポイントを行の
先頭に移動しておかなければならない。関数の実行後、ポイントは次の行頭に
移動する。"
  (if (eq (following-char) ?<)
      (setq entries (cons (buffer-substring (point) (progn (end-of-line) (point))) entries)))
  (forward-char))

(defun sdicf-encode-string (string)
  "STRING をエンコードする"
  (let ((start 0) ch list)
    (while (string-match "[&<>\n]" string start)
      (setq ch (aref string (match-beginning 0)))
      (setq list (cons (if (eq ch ?&) "&amp;"
			 (if (eq ch ?<) "&lt;"
			   (if (eq ch ?>) "&gt;" "&lf;")))
		       (cons (substring string start (match-beginning 0))
			     list)))
      (setq start (match-end 0)))
    (apply 'concat (nreverse (cons (substring string start) list)))))

(defun sdicf-decode-string (string)
  "STRING をデコードする"
  (let ((start 0) list)
    (while (string-match "&\\(\\(lt\\)\\|\\(gt\\)\\|\\(lf\\)\\|\\(amp\\)\\);"
			 string start)
      (setq list (cons (if (match-beginning 2) "<"
			 (if (match-beginning 3) ">"
			   (if (match-beginning 4) "\n" "&")))
		       (cons (substring string start (match-beginning 0))
			     list)))
      (setq start (match-end 0)))
    (apply 'concat (nreverse (cons (substring string start) list)))))


;;;
;;; Strategy `direct'
;;;

(defun sdicf-direct-init (sdic)
  (or (buffer-live-p (sdicf-get-buffer sdic))
      (if (sdicf-common-init sdic)
	  (save-excursion
	    (set-buffer (sdicf-get-buffer sdic))
	    (delete-region (point-min) (point-max))
	    (insert-file-contents (sdicf-get-filename sdic))
	    (while (re-search-forward "^#" nil t)
	      (delete-region (1- (point)) (progn (end-of-line) (min (1+ (point)) (point-max)))))
	    t))))

(defun sdicf-direct-quit (sdic) nil)

(defun sdicf-direct-search (sdic case pattern)
  (sdicf-direct-init sdic)
  (save-excursion
    (set-buffer (sdicf-get-buffer sdic))
    (goto-char (point-min))
    (let ((case-fold-search case) entries)
      (while (search-forward pattern nil t)
	(beginning-of-line)
	(sdicf-search-internal))
      (nreverse entries))))

;;;
;;; Strategy `grep'
;;;

(defalias 'sdicf-grep-init 'sdicf-common-init)

(defun sdicf-grep-quit (sdic) nil)

(defun sdicf-grep-search (sdic case pattern)
  (sdicf-grep-init sdic)
  (save-excursion
    (set-buffer (sdicf-get-buffer sdic))
    (delete-region (point-min) (point-max))
    (call-process sdicf-grep-command nil t nil (if case "-i" "-e") pattern
		  (sdicf-get-filename sdic))
    (goto-char (point-min))
    (let (entries)
      (while (not (eobp))
	(sdicf-search-internal))
      (nreverse entries))))

;;;
;;; Strategy `array'
;;;

(defun sdicf-array-init (sdic)
  (sdicf-common-init sdic)
  (let ((process (get-buffer-process (sdicf-get-buffer sdic))))
    (or (and process (eq (process-status process) 'run))
	(progn
	  (setq process (start-process "array"
				       (sdicf-get-buffer sdic)
				       sdicf-array-command
				       (sdicf-get-filename sdic)))
	  (accept-process-output process)
	  (process-kill-without-query process)
	  (process-send-string process "style line\n")
	  (accept-process-output process)
	  (process-send-string process "order index\n")
	  (accept-process-output process)
	  (set-process-filter process 'sdicf-array-wait-prompt)
	  t))))

(defun sdicf-array-quit (sdic)
  (if (buffer-live-p (sdicf-get-buffer sdic))
      (let ((process (get-buffer-process (sdicf-get-buffer sdic))))
	(and process
	     (eq (process-status process) 'run)
	     (process-send-string process "quit\n")))))

(defun sdicf-array-send-string (proc string) "\
Send STRING as command to process."
  (save-excursion
    (let ((sdicf-array-wait-prompt-flag t))
      (set-buffer (process-buffer proc))
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (process-send-string proc (concat string "\n"))
      (while sdicf-array-wait-prompt-flag (accept-process-output proc))
      )))

(defun sdicf-array-wait-prompt (proc string) "\
Process filter function of Array.
プロンプトが現れたことを検知して、sdicf-array-wait-prompt-flag を nil 
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
		  (setq sdicf-array-wait-prompt-flag nil))
	      (goto-char start))))
      (set-buffer old-buffer))))

(defun sdicf-array-search (sdic case pattern)
  (sdicf-array-init sdic)
  (save-excursion
    (let ((case-fold-search nil)		; array cannot search case sensitively
	  (process (get-buffer-process (set-buffer (sdicf-get-buffer sdic)))))
      (sdicf-array-send-string process "init")
      (delete-region (point-min) (point-max))
      (sdicf-array-send-string process (concat "search " pattern))
      (forward-line -1)
      (beginning-of-line)
      (if (looking-at "FOUND:")
	  (progn
	    (delete-region (point-min) (point-max))
	    (sdicf-array-send-string process "show")
	    (goto-char (point-min))
	    (let (entries (prev ""))
	      (while (not (eobp))
		(sdicf-search-internal))
	      (delq nil (mapcar (function (lambda (s) (if (string= prev s) nil (setq prev s))))
				(sort entries 'string<))))
	    )))))


;;;
;;; Interface functions
;;;

(defun sdicf-open (filename &optional strategy) "\
SDIC形式の辞書をオープンする
FILENAME は辞書のファイル名。STRATEGY は省略可能で、検索を行なう方式を
指定する。

 `direct' - 辞書をバッファに読んで直接検索。
 `grep'   - grep コマンドを用いて検索。
 `array'  - SUFARY を用いた高速検索。

STRATEGY が省略された場合、いずれかを自動的に判定する。

辞書オブジェクトは CAR が `SDIC' のベクタで、要素としてファイル名と 
strategy と作業用バッファを持つ。"
  (let ((sdic (vector 'SDIC
		      (if (file-readable-p (setq filename (expand-file-name filename)))
			  filename
			(error "Cannot open file: %s" filename))
		      (if strategy
			  (if (memq strategy (mapcar 'car sdicf-strategy-alist))
			      strategy
			    (error "Invalid search strategy: %S" strategy))
			(or (and (stringp sdicf-array-command)
				 (file-exists-p (concat filename ".ary"))
				 'array)
			    (and (stringp sdicf-grep-command)
				 'grep)
			    'direct))
		      nil)))
    (and (funcall (nth 1 (assq (sdicf-get-strategy sdic) sdicf-strategy-alist)) sdic)
	 sdic)))

(defun sdicf-close (sdic)
  "SDIC 形式の辞書をクローズする"
  (or (sdicf-object-p sdic)
      (signal 'wrong-type-argument (list 'sdicf-object-p sdic)))
  (funcall (nth 2 (assq (sdicf-get-strategy sdic) sdicf-strategy-alist)) sdic)
  (if (sdicf-get-buffer sdic) (kill-buffer (sdicf-get-buffer sdic))))

(defun sdicf-search (sdic method word)
  "SDIC 形式の辞書から検索を行なう
METHOD は検索法で、次のいずれかとする。
 `prefix' - 前方一致検索
 `suffix' - 後方一致検索
 `exact'  - 完全一致検索
 `text'   - 全文検索
WORD は検索語。見付かったエントリのリストを返す。"
  (or (sdicf-object-p sdic)
      (signal 'wrong-type-argument (list 'sdicf-object-p sdic)))
  (or (stringp word)
      (signal 'wrong-type-argument (list 'stringp word)))
  (funcall (nth 3 (assq (sdicf-get-strategy sdic) sdicf-strategy-alist))
	   sdic
	   (eq method 'text)
	   (cond
	    ((eq method 'prefix) (concat "<K>" (sdicf-encode-string (downcase word))))
	    ((eq method 'suffix) (concat (sdicf-encode-string (downcase word)) "</K>"))
	    ((eq method 'exact) (concat "<K>" (sdicf-encode-string (downcase word)) "</K>"))
	    ((eq method 'text) word)
	    (t (error "Invalid search method: %S" method)))))

(defun sdicf-entry-headword (entry &optional add-keys-to-headword)
  "エントリ ENTRY の見出し語を返す。"
  (or (sdicf-entry-p entry)
      (signal 'wrong-type-argument (list 'sdicf-entry-p entry)))
  (let ((head (substring entry (match-beginning 1) (match-end 1))))
    (if add-keys-to-headword
	(let ((start (match-end 0)) keys)
	  (while (string-match "<.>\\([^<]+\\)</.>" entry start)
	    (setq keys (cons (format "[%s]" (substring entry (match-beginning 1) (match-end 1)))
			     keys)
		  start (match-end 0)))
	  (setq head (apply 'concat head " " (nreverse keys)))))
    (sdicf-decode-string head)))

(defun sdicf-entry-text (entry)
  "エントリ ENTRY の本文を返す。"
  (or (stringp entry)
      (signal 'wrong-type-argument (list 'stringp entry)))
  (sdicf-decode-string (substring entry (string-match "[^>]*$" entry))))


;;; sdicf.el ends here
