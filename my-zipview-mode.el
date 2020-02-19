;;; my-zipview-mode.el --- zipファイル閲覧モード
;; zip圧縮ファイルのリストをバッファ中に表示し、ファイル名部分にカーソルを合わせて Returnキーを押すと
;; 当該ファイルを選んで内容表示する圧縮ファイル閲覧モードを作成する。
;; zipファイルの中のリストを表示するには、「unzip l 圧縮ファイル名」、その中に含まれるひとつのファイル
;; だけを内容表示するには、「unzip -p 圧縮ファイル名 フォルダ名/ファイル名」と指定する。
;;
;; Author: Seiichi Nukayama <billie175@gmail.com>
;; Version: 0.01
;;
;; Copyright (c) 2020 by Seiichi Nukayama
;;
;;; Code:

;; 圧縮ファイルの中のファイルのリストを出力する
;; *zipview*というバッファを作成して、そこに一覧を出力
;; この処理は非同期なので、my-proc-state-sentinel に処理を渡している。
(defun my-zipview (zipfile)
  "zipファイル閲覧モード"
  (interactive "sInput string zipfile:")
  (save-excursion
    (let ((work-buffer (pop-to-buffer "*zipview*")) proc)
      (progn
        (switch-to-buffer "*zipview*")
        (message "Current buffer is %s" (buffer-name))
        (setq major-mode 'zipview-mode
              mode-name "zipviewモード")
        (erase-buffer)
        (setq proc (start-process "zipview" work-buffer "unzip" "-l" zipfile))
        (set-process-sentinel proc 'my-proc-state-sentinel)
        (goto-char (point-min))
        (my-zipview-menu)))))

(defun my-updown (n)
  (interactive)
  (let ((p (point)) l)
    (beginning-of-line)
    (setq l (- p (point)))
    (forward-line n)
    (goto-char (+ (point) l))))

(defun up-key ()
  (interactive)
  (my-updown -1))
  
(defun down-key ()
  (interactive)
  (my-updown 1))

(defun left-key ()
  (interactive)
  (backward-char))

(defun right-key ()
  (interactive)
  (forward-char))

(defun my-zipview-end ()
  (interactive)
  (use-local-map my-org-map))


(defun my-zipview-menu ()
  (interactive)
  (setq my-local-map (make-sparse-keymap))
  (setq my-org-map (copy-keymap my-local-map))
  (save-excursion
    (narrow-to-region (point-min) (point-max))
    (define-key my-local-map "\C-m" 'zip-to-show)
    (define-key my-local-map "n" 'down-key)
    (define-key my-local-map "p" 'up-key)
    (define-key my-local-map "f" 'right-key)
    (define-key my-local-map "b" 'left-key)
    (define-key my-local-map "q" 'my-zipview-end)
    (use-local-map my-local-map)))

;; filename -- ディレクトリ部分も含んだ文字列
;;             (ex. "work/samplezip/content.html")
;; @return: dirname -- ディレクトリ部分だけの文字列
;;                    (ex. "work/samplezip/")
(defun get-dirname (filename)
  "パス文字列からディレクトリ部分を抜き出す関数"
  (interactive "sInput string file:")
  (defun get-dirname-in (str dirname)
    (let ((c 0))
      (if (string-match "/" str)
          (progn
            (setq c (string-match "/" str))
            (setq dirname (concat dirname (substring str 0 (+ c 1))))
            (get-dirname-in (substring str (+ c 1)) dirname))
        (if (< 0 (length dirname))
            (substring dirname 0 (- (length dirname) 1))  ; 末尾の"/"を削除
          str))))
  (get-dirname-in filename ""))


(defun zip-to-show ()
  "ひとつの展開ファイルの内容を表示する関数"
  (interactive)
  (let (dirname filename)
    (setq filename (get-filename))
    (setq dirname (get-dirname filename))
    (show-file dirname filename)))


;; ポイントのある箇所のファイル名を取得する
;; @return: filename
(defun get-filename ()
  (interactive)
  (let ((p (point)) line-head start filename)        ; p -- 現在位置
    (beginning-of-line)                              ; 行頭へ移動
    (setq line-head (point))                         ; line-head -- 行頭位置のポイント
    (goto-char p)                                    ; 現在位置へ移動
                                        ; 現在位置が30以下なら31へ移動
    (if (< (- p line-head) 30) (goto-char (+ line-head 31)))
    (skip-chars-backward "^\n\t ")                   ; ファイル名の最初へ
    (setq start (point))                             ; start -- ファイル名の先頭
    (skip-chars-forward "^\n\t ")                    ; 英字とかをスキップ
    (setq filename (buffer-substring start (point))) ; start...point -- ファイル名
    (goto-char p)                                    ; 元の位置へ
    (message filename)
    filename))

;; 圧縮ファイルの中の1つのファイルの内容を表示する
;; *zipview*バッファに表示する
;; 非同期処理なので、処理が終わると、my-proc-state-sentinelを呼ぶ
(defun show-file (dirname filename)
  (interactive "sInput string dir:\nsInput string file:")
  (let (proc)
    (setq proc (start-process "fileview" "*zipcontents*" "unzip" "-p" dirname filename))
    (set-buffer "*zipcontents*")
    (erase-buffer)
    (set-process-sentinel proc 'my-proc-state-sentinel)
    ))

;; 処理が終わると呼び出される関数
;; 「exited abnormally」という文字列が出力されたら失敗と判断
(defun my-proc-state-sentinel (proc state)
  "show-file のあとに呼ばれる関数"
  (let ((ps (process-status proc))
        (curwin (selected-window))
        (result-buffer (process-buffer proc)))
    (cond
     ((eq ps 'exit)                     ; 処理が終了したら
      (progn
        (pop-to-buffer result-buffer)
        (goto-char (point-min))
        (cond
         ((re-search-forward "exited abnormally" nil t) (message "失敗しました"))
         ((re-search-forward "cannot find" nil t) (message "失敗しました"))
         (t (message "成功しました")))
        (select-window curwin)))
     (t nil))))                         ; 処理がそれ以外

(provide 'my-zipview-mode)
;;; my-zipview-mode.el ends here
;;;--------------------------------
;;; 修正時刻： Wed Feb 19 16:30:02 2020

;; 以下は、もう使わんやろけど、一応おいておく
;;---------------------------------------------------------------------------
;; my-zipview の処理が終わると呼び出される関数
;; ただ、一番下の「my-proc-state-sentinel」を使えたほうがいい。
;; 問題は、成功失敗の判断をどのようにするかである。
;; (defun my-zipview-sentinel (proc state)
;;   "my-zipviewの終了時に呼ばれる関数"
;;   (let ((ps (process-status proc))
;;         (curwin (selected-window))
;;         (result-buffer (process-buffer proc)))
;;     (cond
;;      ((eq ps 'exit)
;;       (pop-to-buffer result-buffer)
;;       (goto-char (point-min))
;;       (if (re-search-forward "Archive:" nil t)
;;           (message "一覧取得成功")
;;         (message "一覧取得に失敗しました"))
;;       (select-window curwin))
;;      (t nil))))
