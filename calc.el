;;; calc.el --- 簡単な小遣い帳
;; Copyright (c) 2020 by Seiichi Nukayama
;;
;; Author : Seiichi Nukayama <billie175@gmail.com>
;; URL    : http://www.billies-works.com/
;; Version: 0.01
;;;
;; --BEGIN
;; ガソリン代 390
;; USBメモリ   1990
;; TACKブレンド     525
;; 樹上完熟 735
;; --END

(defun my-calculator ()
  "--BEGINから--ENDまでの行末の数字をすべて足した結果を--END行に挿入"
  (interactive)
  (save-excursion
    (let ((sum 0) n)                 ; sum と n の２つのローカル変数を用意。 sum は 0 で初期化
      (goto-char (point-min))                ; 文書の先頭に移動
      (re-search-forward "^--BEGIN$")        ; --BEGIN 文字列を探して、その次のポイントを返す
      (while (re-search-forward "\\(^--END\\)\\|\\([0-9]+$\\)" nil t)   ; 検索　グループ1 グループ2
        (cond
         ((match-beginning 2)                ; グループ2が見つかったら、その文字列の先頭にポイント
          (setq n (match-string 2))          ; グループ2の文字列を n にセット
          (setq n (string-to-number n))      ; n を数値に変換
          (goto-char (match-beginning 2))    ; グループ2の文字列の先頭に移動
          (skip-chars-backward " \t")        ; 左方向に スペースやタブをスキップし、それ以外の
                                             ; キャラクタの次の位置にポイントをおく
          (delete-region                     ; 指定範囲を削除する
           (point)                           ; 今いるポイントの値。ここから削除する。
           (progn (end-of-line) (point)))    ; 行末に移動、そのポイントの値を得る。ここまで削除。
          (move-to-column 70 t)              ; 70桁めに強制移動。桁がなければ、空白で埋める。
          (insert (format "%5d" n))          ; グループ2の数値を5桁の整数で挿入
          (setq sum (+ sum n)))              ; 合計 sum に その数値を追加する
         ((match-beginning 1)                 ; グループ1が見つかったら、
          (delete-region                      ; 次の範囲を削除
           (point)                            ; グループ1が見つかったその文字列の次のポイント
           (progn (end-of-line) (point)))     ; 行末に移動。そのポイントまで。
          (move-to-column 70 t)               ; 70桁めに移動。桁がない場合は空白で埋める。
          (insert (format "%5d" sum))))))     ; 合計を挿入。
    ))

(provide 'calc)
;;; calc.el ends here
