;;; nerukichi-mode.el --- ねるきちモード
;;; Copyright (c) 2020 by Seiichi Nukayama

(defun nerukichi-mode ()
  (interactive)
  (setq major-mode 'nerukichi-mode
        mode-name "ねるきちもーど")
  (setq my-local-map (make-sparse-keymap))
  (define-key my-local-map "a" 'disp-boku)
  (use-local-map my-local-map)
  )

(defun disp-boku ()
  (interactive)
  (insert "僕ねるきちナリ\n")
  )

(provide 'nerukichi-mode)
;;; nerukichi-mode.el end here
