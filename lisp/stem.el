;;; stem.el ---- routines for stemming
;;; $Id$

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: stemming

;;; Commentary:

;; 論文『An algorithm for suffix stripping (M.F.Porter)』に記述されて
;; いるアルゴリズムに基づいて、英単語の語尾を取り除くためのライブラリ。
;; 利用及び再配布の際は、GNU 一般公用許諾書の適当なバージョンにしたがっ
;; て下さい。

;; 一次配布元
;;    http://www-nagao.kuee.kyoto-u.ac.jp/member/tsuchiya/elisp/xdic.html


;; -*- Emacs-Lisp -*-

(provide 'stem)

(defvar stem:minimum-word-length 4 "Porter のアルゴリズムが適用できる最小語長")


;;;============================================================
;;;	非公開関数
;;;============================================================

;; 動作速度を向上させるために、関数内部で外部変数をいじっている
;; 関数があり、予期しない副作用が発生する可能性が高い。従って、
;; 非公開関数を直接呼び出すことは避けること。

;;------------------------------------------------------------
;;	stemming-rule の条件節を記述する関数群
;;------------------------------------------------------------

(defsubst stem:match (arg) "\
変数 str を検査する非公開関数 (語幹の部分を変数 stem に代入する)"
  (and
   (string-match arg str)
   (setq stem (substring str 0 (match-beginning 0)))))

(defsubst stem:m () "\
変数 stem に含まれている VC の数を求める非公開関数"
  (save-match-data
    (let ((pos 0)(m 0))
      (while (string-match "\\(a\\|e\\|i\\|o\\|u\\|[^aeiou]y+\\)[aeiou]*" stem pos)
	(setq m (1+ m))
	(setq pos (match-end 0)))
      (if (= pos (length stem)) (1- m) m))))

(defsubst stem:m> (i) "\
変数 stem に含まれている VC の数の条件を記述する非公開関数"
  (< i (stem:m)))

(defsubst stem:m= (i) "\
変数 stem に含まれている VC の数の条件を記述する非公開関数"
  (= i (stem:m)))

(defsubst stem:*v* () "\
変数 stem が母音を含んでいるか検査する関数"
  (save-match-data
    (if (string-match "\\(a\\|e\\|i\\|o\\|u\\|[^aeiou]y\\)" stem) t)))

(defsubst stem:*o () "\
変数 stem が cvc の形で終っているか検査する関数"
  (save-match-data
    (if (string-match "[^aeiou][aeiouy][^aeiouwxy]$" stem) t)))



;;------------------------------------------------------------
;;	stemming-rule を記述した関数群
;;------------------------------------------------------------

(defun stem:step1a (str) "第1a段階の stemming rule (非公開関数)"
  (let ((s)(stem))
    (if (setq s (cond
		 ((stem:match "sses$") "ss")
		 ((stem:match "ies$")  "i")
		 ((stem:match "ss$")   "ss")
		 ((stem:match "s$")    "")))
	(concat stem s)
      str)))


(defun stem:step1b (str) "第1b段階の stemming rule (非公開関数)"
  (let ((s)(stem))
    (cond
     ((and (stem:match "eed$") (stem:m> 0))
      (concat stem "ee"))
     ((or (and (not stem) (stem:match "ed$") (stem:*v*))
	  (and (stem:match "ing$") (stem:*v*)))
      (if (and (stem:m= 1) (stem:*o))
	  (concat stem "e")
	(setq str stem)
	(if (setq s (cond
		     ((stem:match "at$") "ate")
		     ((stem:match "bl$") "ble")
		     ((stem:match "iz$") "ize")
		     ((stem:match "\\([^lsz]\\)\\1$")
		      (substring str (match-beginning 1) (match-end 1)))))
	    (concat stem s)
	  str)))
     (t str))))


(defun stem:step1c (str) "第1c段階の stemming rule (非公開関数)"
  (let ((stem))
    (if (and (stem:match "y$")
	     (stem:*v*))
	(concat stem "i")
      str)))


(defun stem:step1 (str) "第1段階の stemming rule (非公開関数)"
  (stem:step1c
   (stem:step1b
    (stem:step1a str))))


(defun stem:step2 (str) "第2段階の stemming rule (非公開関数)"
  (let ((s)(stem))
    (if (and
	 (setq s (cond
		  ((stem:match "ational$") "ate")
		  ((stem:match "tional$")  "tion")
		  ((stem:match "enci$")    "ence")
		  ((stem:match "anci$")    "ance")
		  ((stem:match "izer$")    "ize")
		  ((stem:match "abli$")    "able")
		  ((stem:match "alli$")    "al")
		  ((stem:match "entli$")   "ent")
		  ((stem:match "eli$")     "e")
		  ((stem:match "ousli$")   "ous")
		  ((stem:match "ization$") "ize")
		  ((stem:match "ation$")   "ate")
		  ((stem:match "ator$")    "ate")
		  ((stem:match "alism$")   "al")
		  ((stem:match "iveness$") "ive")
		  ((stem:match "fulness$") "ful")
		  ((stem:match "ousness$") "ous")
		  ((stem:match "aliti$")   "al")
		  ((stem:match "iviti$")   "ive")
		  ((stem:match "biliti$")  "ble")))
	 (stem:m> 0))
	(concat stem s)
      str)))


(defun stem:step3 (str) "第3段階の stemming rule (非公開関数)"
  (let ((s)(stem))
    (if (and
	 (setq s (cond
		  ((stem:match "icate$") "ic")
		  ((stem:match "ative$") "")
		  ((stem:match "alize$") "al")
		  ((stem:match "iciti$") "ic")
		  ((stem:match "ical$")  "ic")
		  ((stem:match "ful$")   "")
		  ((stem:match "ness$")  "")))
	 (stem:m> 0))
	(concat stem s)
      str)))


(defun stem:step4 (str) "第4段階の stemming rule (非公開関数)"
  (let ((stem))
    (if (and (or
	      (stem:match "al$")
	      (stem:match "ance$")
	      (stem:match "ence$")
	      (stem:match "er$")
	      (stem:match "ic$")
	      (stem:match "able$")
	      (stem:match "ible$")
	      (stem:match "ant$")
	      (stem:match "ement$")
	      (stem:match "ment$")
	      (stem:match "ent$")
	      (and (string-match "[st]\\(ion\\)$" str)
		   (setq stem (substring str 0 (match-beginning 1))))
	      (stem:match "ou$")
	      (stem:match "ism$")
	      (stem:match "ate$")
	      (stem:match "iti$")
	      (stem:match "ous$")
	      (stem:match "ive$")
	      (stem:match "ize$"))
	     (stem:m> 1))
	stem str)))


(defun stem:step5 (str) "第5段階の stemming rule (非公開関数)"
  (let ((stem))
    (if (or
	 (and (stem:match "e$")
	      (or (stem:m> 1)
		  (and (stem:m= 1)
		       (not (stem:*o)))))
	 (and (stem:match "ll$")
	      (setq stem (concat stem "l"))
	      (stem:m> 1)))
	stem str)))


(defun stem:extra (str) "\
動詞/形容詞の規則的活用形と名詞の複数形の活用語尾を取り除く非公開関数
与えられた語の原形として可能性のある語のリストを返す"
  (let ((l)(stem))
    (setq l (cond
	     ;; 比較級/最上級
	     ((stem:match "\\([^aeiou]\\)\\1e\\(r\\|st\\)$")
	      (list (substring str (match-beginning 1) (match-end 1))
		    (substring str (match-beginning 0) (match-beginning 2))))
	     ((stem:match "\\([^aeiou]\\)ie\\(r\\|st\\)$")
	      (setq c (substring str (match-beginning 1) (match-end 1)))
	      (list c (concat c "y") (concat c "ie")))
	     ((stem:match "e\\(r\\|st\\)$") '("" "e"))
	     ;; 3単現/複数形
	     ((stem:match "ches$") '("ch" "che"))
	     ((stem:match "shes$") '("sh" "che"))
	     ((stem:match "ses$") '("s" "se"))
	     ((stem:match "xes$") '("x" "xe"))
	     ((stem:match "zes$") '("z" "ze"))
	     ((stem:match "ves$") '("f" "fe"))
	     ((stem:match "\\([^aeiou]\\)oes$")
	      (setq c (substring str -4 -3))
	      (list c (concat c "o") (concat c "oe")))
	     ((stem:match "\\([^aeiou]\\)ies$")
	      (setq c (substring str -4 -3))
	      (list c (concat c "y") (concat c "ie")))
	     ((stem:match "es$") '("" "e"))
	     ((stem:match "s$") '(""))
	     ;; 過去形/過去分詞
	     ((stem:match "\\([^aeiou]\\)ied$")
	      (setq c (substring str -4 -3))
	      (list c (concat c "y") (concat c "ie")))
	     ((stem:match "\\([^aeiou]\\)\\1ed$")
	      (list (substring str -4 -3)
		    (substring str -4 -1)))
	     ((stem:match "cked$") '("c" "cke"))
	     ((stem:match "ed$") '("" "e"))
	     ;; 現在分詞
	     ((stem:match "\\([^aeiou]\\)\\1ing$")
	      (list (substring str -5 -4)))
	     ((stem:match "ing$") '("" "e"))
	     ))
    (append (mapcar '(lambda (s) (concat stem s)) l)
	    (list str))
    ))




;;;============================================================
;;;	公開関数
;;;============================================================

(defun stem:stripping-suffix (str) "\
活用語尾を取り除く関数
与えられた語の元の語として可能性のある語のリストを返す"
  (save-match-data
    (let (l w)
      (setq l (sort
	       (append
		;; 大文字を小文字に変換
		(list (prog1 str (setq str (downcase str))))
		;; 独自のヒューリスティックスを適用
		(stem:extra str)
		(if (> (length str) stem:minimum-word-length)
		    ;; 単語長が条件を満たせば、Porter のアルゴリズムを適用
		    (mapcar
		     '(lambda (func)
			(setq str (funcall func str)))
		     '(stem:step1 stem:step2 stem:step3 stem:step4 stem:step5))))
	       'string<))
      ;; 最長共通部分列を求める
      (let* ((w1 (car l))
	     (w2 (car (reverse l)))
	     (i (min (length w1) (length w2))))
	(while (not (string= (substring w1 0 i)
			     (substring w2 0 i)))
	  (setq i (1- i)))
	(setq l (cons (substring w1 0 i) l)))
      ;; 重複している要素を取り除く
      (mapcar '(lambda (c) (or (string= c (car w)) (setq w (cons c w)))) l)
      ;; 文字列の長さ順に並べかえる
      (sort (reverse w)
	    '(lambda (a b) (< (length a) (length b))))
      )))


;;; 主関数の別名
(defalias 'stemming 'stem:stripping-suffix)


;;; Porter のアルゴリズムを適用する関数
(defun stem:stripping-inflection (word) "\
Porter のアルゴリズムに基づいて派生語を処理する関数"
  (save-match-data
    (stem:step5
     (stem:step4
      (stem:step3
       (stem:step2
	(stem:step1 word)))))))
