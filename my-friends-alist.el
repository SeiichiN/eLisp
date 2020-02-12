;;; my-friends-alist.ml
;;; 友人のメールアドレス
;;; p132

(defvar my-friends-alist
      '(("yuuji@ae.keio.ac.jp")
        ("pcs39334@asciinet.or.jp")
        ("kondara@yzfrl.inc")
        ("yuuji@koeki-u.ac.jp")
        ("yuuji@yatex.org")
        ("harano@bellydandy.org"))
      "よく差し出す相手のメールアドレスの補完リスト")

(defun my-complete-friends ()
  "ポイント位置の単語からメールアドレスを補完する"
  (interactive)
  (let (word (p (point)) start result)   ; 変数の宣言 word p start result (p = 現在位置)
    (save-excursion
    (skip-chars-backward "^\n\t ")       ; 空白改行まで戻る
    (setq start (point))                 ; その戻った位置をスタートとする
    (setq word (buffer-substring start p))     ; start から p（元の位置）までを word とする
    (setq result (try-completion word my-friends-alist))   ; word を my-friend-alist と比較しその結果を result に入れる
    (cond
     ((eq result t) (message "唯一の補完結果です"))        ; t ... 一致している(補完の必要なし)
     ((eq result nil) (error "そのようなアドレスは登録されていません"))   ; nil ... 一致するものがない
     ((string= result word)                                ; word そのものが結果として帰ってきた場合（部分一致）
      (with-output-to-temp-buffer "*Completions*"          ; 標準出力をバッファに切り替えて
        (display-completion-list                           ; 候補文字列リストを一覧表示する
         (all-completions word my-friends-alist)))         ; 候補文字列の一覧リストを作成する
      )
     (t (delete-region (point) p))                         ; 上記以外であれば、上で設定した範囲を削除する
     (insert result)                                       ; try-completion の結果を挿入する
     ;; 別の候補の可能性あるか調べる
     (if (eq t (try-completion result my-friends-alist))   ; result を元に再度補完してみて、t ならば
         nil  ; 唯一の補完結果なので何もしない             ; 他の補完候補はない
       (message "複数の候補が存在します")                  ; 他の補完候補がある
       )
     ))))

(define-key global-map "\C-cm" 'my-complete-friends)














