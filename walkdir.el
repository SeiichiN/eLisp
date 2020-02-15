;;; walkdir.el --- 簡易ファイルブラウザ
;; 簡易ファイルブラウザ walkdir p154
;; 機能：
;;   指定したディレクトリの中のファイル一覧をバッファの1行に1つずつ表示する。
;;   行の後半にそのファイルの見出し行を表示する
;;   バッファ内は、n と p でカーソルを上下に移動できる
;;   移動のたびに隣のウィンドウにカーソル位置のファイルの先頭10行程度が表示される。
;;   f を押すと、そのファイルが find-file によってオープンされる。
;;
;; Author  : Seiichi Nukayama <billie175@gmail.com>
;; Version : 0.01
;;
;; Copyright (c) 2020 by Seiichi Nukayama
;;
;;; Code:


;; ポイントがあるところのファイル名を取得する
;; @Return -- filename
(defun get-filename ()
  (interactive)
  (save-excursion
    (let ((p (point)) start filename)
      (beginning-of-line)
      (setq start (point))
      (skip-chars-forward "^\n\t ")
      (setq filename (buffer-substring start (point)))
      (message filename)
      filename)))

;; ウィンドウが1つなら、もう1つウィンドウを作成する
;; @Param: buf -- バッファオブジェクト
;; 
;; (defun set-window (buf)
;;   (interactive)
;;   (if (one-window-p)
;;       (split-window))
;;   (pop-to-buffer buf))


(defun show-file ()
  (interactive)
  (save-excursion
    (let ((my-filename (get-filename))
          (current-buf (buffer-name))
          (current-win (selected-window))
          (tmpbuf (pop-to-buffer " *temp*")))
      ;(set-window tmpbuf)              ; 別のウィンドウを作成する
      (set-buffer tmpbuf)
      (erase-buffer)
      (progn
        (insert-file-contents my-filename))
      (select-window current-win)
      (set-buffer current-buf)
      )))

;; 終了処理
;; 別のウィンドウを開いていたら、それを閉じて、現在のバッファも閉じる
(defun walkdir-end ()
  (interactive)
  (if (get-buffer " *temp*") (kill-buffer (get-buffer " *temp*")))
  (if (get-buffer " *walkdir*") (kill-buffer (get-buffer " *walkdir*")))
  (if (not (one-window-p)) (delete-other-windows)))

(defun move-up ()
  (interactive)
  (previous-line)
  (show-file))

(defun move-down ()
  (interactive)
  (next-line)
  (show-file))

(defun walkdir-keymap ()
  (interactive)
  (setq my-local-map (make-sparse-keymap))
  (save-restriction
    (narrow-to-region (point-min) (point-max))
    (define-key my-local-map "n" 'move-down)
    (define-key my-local-map "p" 'move-up)
    (define-key my-local-map "f" 'show-file)
    (define-key my-local-map "q" 'walkdir-end)
    (use-local-map my-local-map)))


(defun walkdir ()
  "簡易ファイルブラウザ"
  (interactive)
  (save-excursion
    (let ((work-buffer (get-buffer-create " *walkdir*"))
          (filename-list (directory-files "./")))
      (progn
        (switch-to-buffer " *walkdir*")
        (message "Current buffer is %s" (buffer-name))
        (setq major-mode 'walkdir-mode
              mode-name "Walkdirモード")
        (erase-buffer)
        (goto-char (point-min))
        (walkdir-keymap)
;        (setq filename-list (directory-files "./"))
        (mapcar
         '(lambda (x) (insert (format "%-30s \n" x)))
                                        ;(shell-command-to-string "head" ))
         filename-list)
      ))))


(provide 'walkdir)
;;; walkdir.el end here
