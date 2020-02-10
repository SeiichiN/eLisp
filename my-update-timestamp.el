;;; my-update-timestamp.el
;;; 修正時刻を更新する
;;; Copyright 2022 Seiichi Nukayama

(defun my-update-timestamp ()
  (interactive)
  (if (my-search-stamp)
      (progn
        (delete-char 24)
        (my-insert-current-time)
        (message "修正時刻を更新しました"))
    (goto-char (point-max))
    (insert (concat "\n修正時刻： " (current-time-string) "\n"))
    (message "修正時刻がなかったので、作りました")
    nil))

(define-key global-map "\C-cd" 'my-update-timestamp)

(defun my-insert-current-time ()
  (interactive)
  (insert (current-time-string)))

(setq search-word "修正時刻")
(setq search-weekday "Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat")

;; 「修正時刻： Sun Feb 2020 ...」という言葉を探して、
;; 見つかったら、その曜日の先頭に移動する関数
;; 補助関数
;;   my-goto-top       -- バッファの先頭に移動
;;   my-search-word    -- 「修正時刻」を探す
;;   my-search-weekday -- 「Sun」などの曜日を探す
(defun my-search-stamp ()
  (interactive)
  (let ((p (my-goto-top)))
    (if (my-search-word)
        (progn
          (setq p (my-search-word))
          (if (my-search-weekday)
              t
            (message "曜日がないで")
            (goto-char p)
            nil))
      (progn
        (goto-char p)
        (message "ないで")
        nil))))

;; Sun...Sat を探す関数
;; もし見つけたら、そのワードの先頭のポイントに移動
(defun my-search-weekday ()
  (interactive)
  (if (re-search-forward search-weekday nil t)
      (goto-char (match-beginning 0))
    nil))

;; バッファの先頭に移動する関数
;; 移動する前のポイントを p に格納している
;; 移動のあとは、p の値を返す
(defun my-goto-top ()
  (interactive)
  (let (p)
    (setq p (point))
    (goto-char (point-min))
    p)
  )

;; 「修正時刻」というワードを探す関数
;; もし探したら、そのワードの先頭のポイントを返す
(defun my-search-word ()
  (interactive)
  (my-goto-top)
  (if (search-forward search-word nil t)
      (match-beginning 0)
    nil))
