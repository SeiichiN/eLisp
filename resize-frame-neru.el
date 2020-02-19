;;; resize-frame-neru.el --- 対話的にフレームサイズを変更する
;; 『改訂版・やさしいEmacs-Lisp講座』p172
;; Copyright (c) 2020 Yuhji Hirose
;;
;; Code:

(defun resize-frame-neru ()
  "本に載ってたやり方"
  (interactive)
  (let (key (width (frame-width)) (height (frame-height)))
    (catch 'quit
      (while t
        (setq key (read-char))
        (cond
         ((eq key ?n) (setq height (1+ height)))
         ((eq key ?p) (setq height (1- height)))
         ((eq key ?f) (setq width (1+ width)))
         ((eq key ?b) (setq width (1- width)))
         (t (throw 'quit t)))
        (modify-frame-parameters
         nil
         (list (cons 'width width) (cons 'height height)))))
    (message "おしまい")))


;(provide 'resize-frame-neru')
;;; resize-frame-neru.el end here
;;; ---------------------------------
;;; 修正時刻： Mon Feb 17 19:21:31 2020
