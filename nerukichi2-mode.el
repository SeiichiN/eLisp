;;; ねるきちモードII
;;; Copyright 2020 Seiichi Nukayama

(setq my-local-map (make-sparse-keymap))
(let ( (key ?a))
  (while (<= key ?z)
    (let ((skey (char-to-string key))
          (x (char-to-string (atari))))
      (message (concat "x = " x))
      ;(ata (char-to-string ?c)))
      (if (string= skey x)
          (define-key my-local-map skey 'disp-jibaku)
        (define-key my-local-map skey 'disp-boku)))
    (setq key (+ 1 key))))


(defun nerukichi2-mode ()
    "ねるきちモードだよ"
  (interactive)
  (setq major-mode 'nerukichi-mode
        mode-name "ねるきちもーど")
  (use-local-map my-local-map)
  )

(defun disp-boku ()
  (interactive)
  (let ((x (this-command-keys)))
    (insert (concat "僕ねるきち " x " ナリ\n")))
  )

(defun disp-jibaku ()
  (interactive)
  (message "自爆")
  (erase-buffer)
  )

(defun atari ()
  (interactive)
  (random t)
  (let ((x (+ 97 (random 26))))
    x))






