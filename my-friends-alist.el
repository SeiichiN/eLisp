;;; my-friends-alist.ml
;;; 友人のメールアドレス
;;; p132

(setq my-friends-alist
      '(("yuuji@ae.keio.ac.jp")
        ("pcs39334@asciinet.or.jp")
        ("kondara@yzfrl.inc")
        ("yuuji@koeki-u.ac.jp")
        ("yuuji@yatex.org")
        ("harano@bellydandy.org")))


(let (word (p (point)) start result)
  (skip-chars-backward "^\n\t ")
  (setq start (point))
  (setq word (buffer-substring start p))
  (setq result (try-completion word my-friends-alist))
  (cond
   ((eq result t) (message "唯一の補完結果です"))
   ((eq result nil) (error "そのようなアドレスは登録されていません"))
   ((string= result word)
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list
       (all-completions word my-friends-alist)))
    )
   (t (delete-region (point) p))
   (insert result)
   ;; 別の候補の可能性あるか調べる
   (if (eq) t)
  )

