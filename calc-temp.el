;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(while (re-search-forward
        "\\([12][0-9][0-9][0-9]\\)/\\([ 012]?[0-9]\\)/\\([ 0123]?[0-9]\\)"
        nil t)
  (replace-match "西暦\\1年\\2月\\3日"))



nil

西暦2011年5月5日nil

西暦2020年2月8日nil

西暦2020年9月12日nil


(goto-char (point-min))


(re-search-forward "^--BEGIN$")


(defun calc ()
  (let ((sum 0) n) ; sum と n の２つのローカル変数を用意。 sum は 0 で初期化
    (while (re-search-forward "\\(^--END\\)\\|\\([0-9]+$\\)" nil t) ; 検    2索　グループ1 グループ
      (cond
       ((match-beginning 2) ; グループ2が見つかったら、その文字列の先頭にポイント
        (setq n (match-string 2))     ; グループ2の文字列を n にセット
        (setq n (string-to-number n)) ; n を数値に変換
        (goto-char (match-beginning 2)) ; グループ2の文字列の先頭に移動
        (skip-chars-backward " \t") ; 左方向に スペースやタブをスキップし、それ以外の
                                        ; キャラクタの次の位置にポイントをおく
        (delete-region                  ; 指定範囲を削除する
         (point)              ; 今いるポイントの値。ここから削除する。
         (progn (end-of-line) (point))) ; 行末に移動、そのポイントの値を得る。ここまで削除。
        (move-to-column 70 t) ; 70桁めに強制移動。桁がなければ、空白で埋める。
        (insert (format "%5d" n))   ; グループ2の数値を5桁の整数で挿入
        (setq sum (+ sum n)))))     ; 合計 sum に その数値を追加する
    ))




;;; calc.el 
;;; 小遣い帳
;;;
--BEGIN
ガソリン代                                                              390
USBメモリ                                                              1990
TACKブレンド                                                            525
樹上完熟                                                                735
--ENDnil




(setq debug-on-error t)
t
