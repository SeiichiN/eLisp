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


;; ファイルの先頭 n行を取得する
;; @param: filename -- string ファイル名
;;         n        -- string 1...10...
;; @return: ファイルの内容 n行分
(defun show-head (filename n)
  (interactive)
  (let (opt str)
    (setq opt (concat "-n" (number-to-string n)))
    (setq str (shell-command-to-string
     (mapconcat #'shell-quote-argument
                (list "head" opt filename)
                " ")))
    (setq str (replace-regexp-in-string "\n$" "" str ))
    str))

;; ポイントのところのファイルを別ウィンドウで開く
;; 参照で開く
(defun show-file ()
  (interactive)
  (save-excursion
    (let ((my-filename (get-filename))
          (current-buf (buffer-name))
          (current-win (selected-window))
          (tmpbuf (pop-to-buffer " *temp*")))
      (set-buffer tmpbuf)
      (erase-buffer)
      (progn
        (linum-mode 0)
        (insert (show-head my-filename 10)))
      (select-window current-win)
      (set-buffer current-buf)
      )))

;; ポイントのところのファイルを別ウィンドウで開く
;; 編集可能で開く
(defun edit-file ()
  (interactive)
  (save-excursion
    (let ((my-filename (get-filename))
          (current-buf (buffer-name))
          (current-win (selected-window))
          (tmpbuf (pop-to-buffer (get-filename))))
      (set-buffer tmpbuf)
      (erase-buffer)
      (progn
        (linum-mode 1)
        (insert-file-contents my-filename t))
      )))

;; 終了処理
;; 別のウィンドウを開いていたら、それを閉じて、現在のバッファも閉じる
(defun walkdir-end ()
  (interactive)
  (if (get-buffer " *temp*") (kill-buffer (get-buffer " *temp*")))
  (if (get-buffer " *walkdir*") (kill-buffer (get-buffer " *walkdir*")))
  (if (not (one-window-p)) (delete-other-windows)))

;; p -- 上へ移動
(defun move-up ()
  (interactive)
  (previous-line)
  (show-file))

;; n -- 下へ移動
(defun move-down ()
  (interactive)
  (next-line)
  (show-file))

;; キーマップ
(defun walkdir-keymap ()
  (interactive)
  (setq my-local-map (make-sparse-keymap))
  (save-restriction
    (narrow-to-region (point-min) (point-max))
    (define-key my-local-map "n" 'move-down)
    (define-key my-local-map "p" 'move-up)
    (define-key my-local-map "f" 'edit-file)
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
        (walkdir-keymap)
        (mapcar
         '(lambda (x)
            (insert (format "%-30s " x))
            (insert (format "%-20s\n" (show-head x 1))))
            ;; (insert (format "%-30s " (substring x 1 (length x))))
            ;; (insert (format "%-20s" (substring (show-head x 1) 1 (length x)))))
         filename-list)
        (goto-char (point-min))
      ))))


(provide 'walkdir)
;;; walkdir.el end here
