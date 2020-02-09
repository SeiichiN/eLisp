;;; mymode.el
;;; Emacs lisp 最初の一歩
;;; Copyright 2020 Seiichi Nukayama

(defun my-mode ()
  (interactive)
  (setq my-name "Seiichi")
  (setq major-mode 'my-mode
        mode-name "私のもーど")
  (setq my-local-map (make-sparse-keymap))
  (define-key my-local-map "h" 'backward-char)
  (define-key my-local-map "j" 'previous-line)
  (define-key my-local-map "k" 'next-line)
  (define-key my-local-map "l" 'forward-char)
  (define-key my-local-map "\C-ch" 'hello-world)
  (use-local-map my-local-map)
  )

(defun hello-world ()
  (interactive)
  (message my-name)
  (insert "Hello, world!\n"))

