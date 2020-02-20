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

;; バッファの最初から最後まで、[[]]で囲まれた部分にオーバーレイを設定して、
;; 非表示にする
(defun set-to-invisible ()
  (interactive)
  (goto-char (point-min))
  (defun set-to-invisible-in ()
    (let ((p (re-search-forward "\\[\\[" nil t)) ov)
      (if (numberp p)
          (progn
            (setq ov (make-overlay
                      (progn      ; 始点
                        (goto-char p)
                        (point))
                      (progn      ; 終点
                        (goto-char (re-search-forward "\\]\\]" nil t))
                        (- (point) 2))))     ; "]]"の分だけポイントをずらす
            (overlay-put ov 'invisible t)
            (set-to-invisible-in))
        (goto-char (point-min))
        nil)))
  (set-to-invisible-in))
    
(defun my-change-visible ()
  (interactive)
  (let (overlay-list)
    (setq overlay-list (overlays-at (point)))
    (message overlay-list)
    (overlay-put overlay-list 'invisible nil)))

(defun my-checksheet ()
  (interactive)
  (save-restriction
    (let (my-local-map my-org-map)
      (narrow-to-region (point-min) (point-max))
      (setq my-local-map (make-sparse-keymap))
      (setq my-org-map (copy-keymap my-local-map))
      (define-key my-local-map "\C-i" 'my-change-visible)
      (set-to-invisible))))
  

(provide 'my-checksheet)
;;; my-checksheet.el ends here
;;;-------------------------------------------
;;; 修正時刻： Thu Feb 20 07:32:22 2020
