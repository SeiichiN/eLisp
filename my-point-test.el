TEST!!1
TEST!!2
TEST!!3
TEST!!4
TEST!!5
--END

(defun my-point-test ()
  (interactive)
  (let (end)                               ; ローカル変数 end
    (goto-char (point-min))                ; バッファ先頭に移動
    (save-excursion
      (search-forward "--END")             ; --END を探す
      (goto-char (match-beginning 0))      ; 検索語の先頭に移動
      (setq end (point))                   ; そのポイントを end にセット
      (message "end is %d" (point))        ; 表示
      (sit-for 3))                         ; 3秒待機
    (while (< (point) end)                 ; point が end より小さい間は繰り返す
      (insert "TEST!!")                    ; TEST!! を挿入
      (forward-line 1))                    ; 次の行に移動
    (goto-char 8))                       ; end すなわち --END の先頭に移動
  )


(defun my-marker-test ()
  (interactive)
  (let (end)                               ; ローカル変数 end
    (goto-char (point-min))                ; バッファ先頭に移動
    (save-excursion
      (search-forward "--END")             ; --END を探す
      (goto-char (match-beginning 0))      ; 検索語の先頭に移動
      (setq end (point-marker))            ; そのポイントをマーカーとして end にセット
      (message "end is %d" (point))        ; 表示
      (sit-for 3))                         ; 3秒待機
    (while (< (point) end)                 ; point が end より小さい間は繰り返す
      (insert "TEST!!")                    ; TEST!! を挿入
      (forward-line 1))                    ; 次の行に移動
    (goto-char end))                       ; end すなわち --END の先頭に移動
  )


(defun my-narrowing-test ()
  (interactive)
  (goto-char (point-min))                  ; バッファ先頭に移動
  (save-restriction                        ; 現在の範囲制限を保存
    (save-excursion                        ; 現在位置を保存
      (search-forward "--END")             ; --END を探す
      (goto-char (match-beginning 0))      ; 検索語の先頭に移動
      (narrow-to-region                    ; 編集可能領域を設定する
       (point-min) (point))                ; 今回は、バッファ先頭から --END の先頭まで
      (message "end is %d" (point))        ; 表示
      (sit-for 3))                         ; 3秒待機
    (while (not (eobp))                    ; point が バッファ終端でなければ
      (insert "TEST!!")                    ; TEST!! を挿入
      (forward-line 1))                    ; 次の行に移動
    ))

;;; 注
;;; 本来は、以下のかたちで使う
;;; (save-excursion
;;;    (save-restriction
;;;    .....
;;;    .....))

;;; 修正時刻： Sun Feb  9 09:21:14 2020
