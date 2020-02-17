;;; resize-frame-interactively.el -- 現在のフレームのサイズをキーボードで対話的に変更する
;; 機能：
;;   関数起動後、n, p, f, b を押すことにより、フレームサイズがそれぞれ、1行拡大、1行縮小、
;;   1桁拡大、1桁縮小するものとし、それ以外のキーを押すまで繰り返される。
;;
;; Author: Seiichi Nukayama <billie175@gmail.com>
;; Version: 0.01
;; Copyright (c) 2020 by Seiichi Nukayama
;;
;;; Code:

(defvar my-org-map my-local-map)

(defun change-height (n)
  (let* ((now-frame-alist (frame-parameters))
         (height (cdr (assq 'height now-frame-alist))))
    (modify-frame-parameters
     nil
     (list (cons 'height (+ height n))))))

(defun change-width (n)
  (let* ((now-frame-alist (frame-parameters))
         (width (cdr (assq 'width now-frame-alist))))
    (modify-frame-parameters
     nil
     (list (cons 'width (+ width n))))))

(defun change-background-color (color)
  (let* ((now-frame-alist (frame-parameters))
         (background-color (cdr (assq 'background-color now-frame-alist))))
    (modify-frame-parameters
     nil
     (list (cons 'background-color color)))))



(defun line-minus ()
  (interactive)
  (change-height -1))

(defun line-plus ()
  (interactive)
  (change-height 1))

(defun col-minus ()
  (interactive)
  (change-width -1))

(defun col-plus ()
  (interactive)
  (change-width 1))

(defun resize-frame-end ()
  (interactive)
  (use-local-map my-org-map)
  (change-background-color "black"))

(defun nothing-do ()
  "何もしないよ"
  (message "なにもしないよ"))

;; キーマップ
(defun resize-frame-keymap ()
  (interactive)
  (setq my-local-map (make-sparse-keymap))
  (setq my-org-map (copy-keymap my-local-map))
  (save-excursion
    (narrow-to-region (point-min) (point-max))
    (define-key my-local-map "n" 'line-plus)
    (define-key my-local-map "p" 'line-minus)
    (define-key my-local-map "f" 'col-plus)
    (define-key my-local-map "b" 'col-minus)
    (define-key my-local-map "q" 'resize-frame-end)
    (use-local-map my-local-map)))

(defun resize-frame-interactively ()
  (interactive)
  (change-background-color "blue")
  (resize-frame-keymap))


(provide 'resize-frame-interactively)
;;; resize-frame-interactively.el end here
;;;--------------------------------------
;;; 修正時刻： Mon Feb 17 13:05:04 2020
