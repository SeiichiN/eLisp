;;; my-checksheet.el --- [[]]で囲まれた文字列にいろいろ細工する
;;
;; [[文字列]]のような部分を一時的に隠し、読めない状態にする。そののち、TABキーを押すごとに
;; 次のような動作をする。
;; 1. ポイント位置が隠された単語部分に該当するなら、見えるように戻す。
;; 2. 次に見つかる隠された単語部分にポイントを移動する。
;; 3. 見つからなければ終了。
;; このような動きを M-x checksheet として行えるようにするプログラムを作成せよ。
;;
;; 『改訂版・やさしいEmacs-Lisp講座』p.226
;;
;; Copyright (c) 2020 Seiichi Nukayama
;;
;;; Code:

(defface checksheet-hide-simple
  '(
    (((class color) (type tty) (background dark)) (:background "cyan" :foreground "red"))
    (((class color) (type x) (background dark)) (:background "glay" :foreground "red"))
    (t (:underline t)))
  "単純な色の設定")

;; (defface checksheet-hide
;;   '(
;;     (((class color) (type tty) (background dark)) (:background "glay" :foreground "red"))
;;     (((class color) (type x) (background light)) (:background "glay" :foreground "red"))
;;     (t (:underline t)))
;;     "-- hide face --")

;; バッファの最初から最後まで、[[]]で囲まれた部分にオーバーレイを設定して、
;; 非表示にする
(defun set-to-invisible ()
  (interactive)
  (goto-char (point-min))
  (defun set-to-invisible-in ()
    (let ((p (re-search-forward "\\[\\[" nil t)) ov)
      (if (numberp p)
          (progn
            (add-text-properties
                      (progn      ; 始点
                        (goto-char p)
                        (- (point) 2))
                      (progn      ; 終点
                        (goto-char (re-search-forward "\\]\\]" nil t))
                        (point))
                                        ; "]]"の分だけポイントをずらす
                      '(face checksheet-hide-simple display " ??? "))     
            (set-to-invisible-in))
        (goto-char (point-min))
        nil)))
  (set-to-invisible-in))

(defun get-start-end ()
  (interactive)
  (let (start end p)
    (setq p (point))
    (setq start (next-property-change p))
    ;(setq start (text-property-any p (+ p 10) 'display " ??? "))
    (setq end (re-search-forward "\\]\\]" nil t))
    (list start end)))

(defun set-visible-off ()
  (interactive)
  (let (pos start end)
    (setq pos (get-start-end))
    (setq start (car pos))
    (setq end (car (cdr pos)))
    (add-text-properties start end '(display " ??? "))))




(defun set-visible-on ()
  (interactive)
  (let (pos start end)
    (setq pos (get-start-end))
    (setq start (car pos))
    (setq end (car (cdr pos)))
    (remove-text-properties start end '(display nil))))

(defun my-change-visible ()
  (interactive)
  (let (p)
    (setq p (next-property-change (point)))
    (if (numberp p) (set-visible-on)
      (message "もうないよ")
      nil)))

  ;; (if (eq (get-text-property (point) 'display) " ??? ")
  ;;     (set-visible-on)
  ;;   (set-visible-off)))

(defun my-keymap ()
  (interactive)
  (setq my-check-map (make-sparse-keymap))
  (define-key my-check-map "\t" 'my-change-visible)
  (define-key my-check-map "t" 'my-change-visible)
  (define-key my-check-map "h" 'backward-char)
  (define-key my-check-map "j" 'previous-line)
  (define-key my-check-map "k" 'next-line)
  (define-key my-check-map "l" 'forward-char)
  (use-local-map my-check-map))

(defun my-checksheet ()
  (interactive)
  (save-restriction
    (narrow-to-region (point-min) (point-max))
    (set-to-invisible)
    (my-keymap)))

;; (let (my-local-map my-org-map)
    ;;   (setq my-local-map (make-sparse-keymap))
    ;;   (setq my-org-map (copy-keymap my-local-map))
    ;;   (set-to-invisible)
    ;;   (define-key my-local-map "\C-i" 'my-change-visible))))
  

;;-----------------------------------------------------
;; https://www.emacswiki.org/emacs/EmacsOverlays
;;
(defun list-overlays-at (&optional pos)
  "Describe overlays at POS or point."
  (interactive)
  (setq pos (or pos (point)))
  (let ((overlays (overlays-at pos))
        (obuf (current-buffer))
        (buf (get-buffer-create "*Overlays*"))
        (props '(priority window category face mouse-face display
                          help-echo modification-hooks insert-in-front-hooks
                          insert-behind-hooks invisible intangible
                          isearch-open-invisible isearch-open-invisible-temporary
                          before-string after-string evaporate local-map keymap
                          field))
        start end text)
    (if (not overlays)
        (message "None.")
      (set-buffer buf)
      (erase-buffer)
      (dolist (o overlays)
        (setq start (overlay-start o)
              end (overlay-end o)
              text (with-current-buffer obuf
                     (buffer-substring start end)))
        (when (> (- end start) 13)
          (setq text (concat (substring text 1 10) "...")))
        (insert (format "From %d to %d: \"%s\":\n" start end text))
        (dolist (p props)
          (when (overlay-get o p)
            (insert (format " %15S: %S\n" p (overlay-get o p))))))
      (pop-to-buffer buf))))


(provide 'my-checksheet)
;;; my-checksheet.el ends here
;;;-------------------------------------------
;;; 修正時刻： Thu Feb 20 07:32:22 2020



;; バッファの最初から最後まで、[[]]で囲まれた部分にオーバーレイを設定して、
;; 非表示にする
;; (defun set-to-invisible ()
;;   (interactive)
;;   (goto-char (point-min))
;;   (defun set-to-invisible-in ()
;;     (let ((p (re-search-forward "\\[\\[" nil t)) ov)
;;       (if (numberp p)
;;           (progn
;;             (setq ov (make-overlay
;;                       (progn      ; 始点
;;                         (goto-char p)
;;                         (point))
;;                       (progn      ; 終点
;;                         (goto-char (re-search-forward "\\]\\]" nil t))
;;                         (- (point) 2))))     ; "]]"の分だけポイントをずらす
;;             (overlay-put ov 'invisible t)
;;             (set-to-invisible-in))
;;         (goto-char (point-min))
;;         nil)))
;;   (set-to-invisible-in))


;; (defun my-change-visible ()
;;   (interactive)
;;   (let (overlay-list)
;;     (setq overlay-list (overlays-at (point)))
;;     (message overlay-list)
;;     (overlay-put overlay-list 'invisible nil)))
