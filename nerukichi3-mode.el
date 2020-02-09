;;; nerukichi3-mode.el
;;; ねるきちモードIII
;;;
;;; aを押したときに、「ねるきち A ナリ」が存在している場合は、「なるきち A2 ナリ」というふうに
;;; 数値をふやしたものに置き換えるようにせよ。
;;;

;; ローカルマップを設定
;; a...z のいずれかを押下すると、指定の関数を実行する
(setq my-local-map (make-sparse-keymap))
(let ( (key ?a) skey x)
  (while (<= key ?z)
    (setq skey (char-to-string key))
    (setq x (char-to-string (atari)))
    (message (concat "x = " x))
    (if (string= skey x)
        (define-key my-local-map skey 'disp-jibaku)
      (define-key my-local-map skey 'disp-boku))
    (setq key (+ 1 key))))

;; ねるきちモードというメジャーモードを設定
;; my-local-map を使うことを設定
(defun nerukichi3-mode ()
    "ねるきちモードだよ"
  (interactive)
  (setq major-mode 'nerukichi-mode
        mode-name "ねるきちもーど")
  (use-local-map my-local-map)
  )

;; 僕ねるきち a ナリという文字列を調べて
;; もしなければ、「僕ねるきち a ナリ」という文字列を
;; 挿入する。
(defun disp-boku ()
  (interactive)
  (let ((x (this-command-keys)))    ; x -- a...z のどれかの1文字
    (if (neru-search-word x)
        (progn
          (neru-replace-word x)
          )
      (goto-char (point-max))
      (insert (concat "僕ねるきち " x " ナリ\n")))
    )
  )

;; 自爆
(defun disp-jibaku ()
  (interactive)
  (message "自爆")
  (erase-buffer)
  )

;; a...z のどれを当たりにするかを設定
;; @return -- あたりの文字
(defun atari ()
  (interactive)
  (random t)
  (let (x)
    (setq x (+ 97 (random 26)))
    ; (message (char-to-string x))
    x))

;; 「僕ねるきち x ナリ」という文字列を検索する
;; もしあれば、t を
;; もしなければ nil を返す
(defun neru-search-word (x)
  (interactive)
  (goto-char (point-min))
  (let (str)
    (setq str (concat "僕ねるきち\\s ?" x "\\([0-9]*\\)\\s ?ナリ"))
    (if (re-search-forward str nil t)
        (progn
          (goto-char (match-beginning 0))
          t )
      nil)))

;; a...zのどれかを引数に指定して、
;; 「a」という文字列があれば「僕ねるきち a1 ナリ」と
;; 「僕ねるきち a1 ナリ」という文字列ならば「僕ねるきち a2 ナリ」と
;; いう文字列に置き換える
(defun neru-replace-word (x)
  (interactive)
  (let (str)
                                        ;(setq str (concat "僕ねるきち\\s ?" x "\\([0-9]*\\)\\s ?ナリ"))
    (setq str (concat x "\\([0-9]*\\)"))
    (message str)
    (re-search-forward str nil t)
    (goto-char (match-beginning 0))
    (neru-replace
     (match-string 0)
     (concat x (neru-plus (match-string 1))))
    )
  )

;; 数字を表す文字列を受け取って、
;; それより1大きい数字の文字列を返す
(defun neru-plus (numStr)
  (interactive)
  (let (newNum)
    (setq newNum (+ 1 (string-to-number numStr)))
    (number-to-string newNum)))

(defun neru-replace (str newStr)
  (interactive)
  (delete-char (length str))
  (insert newStr)
  (forward-line 1))
  
