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


;;;============================================================
;;;	stemming-rule の条件節を記述する関数群
;;;============================================================

;;; 変数 str を検査する関数 (語幹の部分を変数 stem に代入する)
(defsubst stem:match (arg)
  (and
   (string-match arg str)
   (setq stem (substring str 0 (match-beginning 0)))))


;;; 変数 stem に含まれている VC の数を求める関数
(defsubst stem:m ()
  (save-match-data
    (let ((pos 0)(m 0))
      (while (string-match "\\(a\\|e\\|i\\|o\\|u\\|[^aeiou]y+\\)[aeiou]*" stem pos)
	(setq m (1+ m))
	(setq pos (match-end 0)))
      (if (= pos (length stem)) (1- m) m))))

(defsubst stem:m> (i)
  (< i (stem:m)))

(defsubst stem:m= (i)
  (= i (stem:m)))


;;; 変数 stem が母音を含んでいるか検査する関数
(defsubst stem:*v* ()
  (save-match-data
    (if (string-match "\\(a\\|e\\|i\\|o\\|u\\|[^aeiou]y\\)" stem) t)))


;;; 変数 stem が cvc の形で終っているか検査する関数
(defsubst stem:*o ()
  (save-match-data
    (if (string-match "[^aeiou][aeiouy][^aeiouwxy]$" stem) t)))




;;;============================================================
;;;	stemming-rule を記述した関数群
;;;============================================================

;;; 第1a段階の stemming rule
(defun stem:step1a (str)
  (let ((s)(stem))
    (if (setq s (cond
		 ((stem:match "sses$") "ss")
		 ((stem:match "ies$")  "i")
		 ((stem:match "ss$")   "ss")
		 ((stem:match "s$")    "")))
	(concat stem s)
      str)))


;;; 第1b段階の stemming rule
(defun stem:step1b (str)
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


;;; 第1c段階の stemming rule
(defun stem:step1c (str)
  (let ((stem))
    (if (and (stem:match "y$")
	     (stem:*v*))
	(concat stem "i")
      str)))


;;; 第1段階の stemming rule
(defun stem:step1 (str)
  (stem:step1c
   (stem:step1b
    (stem:step1a str))))



;;; 第2段階の stemming rule
(defun stem:step2 (str)
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



;;; 第3段階の stemming rule
(defun stem:step3 (str)
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



;;; 第4段階の stemming rule
(defun stem:step4 (str)
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



;;; 第5段階の stemming rule
(defun stem:step5 (str)
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




;;; ここまで非公開関数

;;;============================================================
;;;	公開関数
;;;============================================================

;;; Porter のアルゴリズムに基づいて派生語を処理する関数
(defun stem:stripping-inflection (str) "\
Porter のアルゴリズムに基づいて派生語を処理する関数
-tion や -ize などの語尾を取り除く
与えられた語の元の語として可能性のある語のリストを返す"
  (let ((w str)(l (list str)))
    (mapcar
     '(lambda (func)
	(setq str (funcall func str))
	(or (string= str (car l))
	    (setq l (cons str l))))
     '(stem:step1 stem:step2 stem:step3 stem:step4 stem:step5))
    (setq w (stem:string-and (car l) w))
    (if (string= w (car l)) l (cons w l))))



;;; Porter のアルゴリズムを適用する関数
(defun stem:stripping-suffix (word)
  (save-match-data
    (stem:step5
     (stem:step4
      (stem:step3
       (stem:step2
	(stem:step1 word)))))))



(defun stem:string-and (w1 w2) "\
2つの文字列の一致している部分を返す関数"
  (let ((i))
    (if (> (length w1) (length w2))	; w1 の方が長い場合
	(setq i  w1			; w1 と w2 を交換する
	      w1 w2
	      w2 i))
    (setq i (length w1))
    (while (not (string= w1 (substring w2 0 i)))
      (setq i (1- i))
      (setq w1 (substring w1 0 i)))
    w1))



(defun stem:word-at-point () "\
カーソル位置の英単語を stemming して返す関数"
  (let ((str))
    (setq str (save-excursion
		(if (not (looking-at "\\<"))
		    (forward-word -1))
		(if (looking-at "[A-Za-z]+")
		    (downcase (match-string 0)))))
    (stem:string-and (stem:stripping-suffix str) str)))




;;; 独自のヒューリスティックスによって
;;; 動詞/形容詞の規則的活用形と名詞の複数形を処理する関数
(defun stem:stripping-conjugation (str) "\
動詞/形容詞の規則的活用形と名詞の複数形の活用語尾を取り除く関数
与えられた語の原形として可能性のある語のリストを返す
マッチするヒューリスティックスがなかった場合、nil を返す"
  (let ((l)(stem))
    (save-match-data
      (if (setq l (cond
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
		  (list str)))
      )))
