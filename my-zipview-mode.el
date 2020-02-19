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
        ;; (set-process-sentinel proc 'my-zipview-sentinel)))))

(defun my-zipview-menu ()
  (interactive)
  (let (key)
    (catch 'quit
      (while t
        (setq key (read-char))
        (cond
         ((eq key 13) (show-file (get-filename)))
         ((eq key ?n) (forward-line 1))
         ((eq key ?p) (forward-line -1))
         ((eq key ?f) (goto-char (+ (point) 1)))
         ((eq key ?b) (goto-char (- (point) 1)))
         (t (throw 'quit t)))))))

;; ポイントのある箇所のファイル名を取得する
;; @return: filename
(defun get-filename ()
  (interactive)
  (let ((p (point)) line-head start filename)
    (beginning-of-line)
    (setq line-head (point))
    (if (< (- p line-head) 30) (goto-char (+ line-head 31)))
    (skip-chars-backward "^ ")
    (setq start (point))
    (skip-chars-forward "^\n\t ")
    (setq filename (buffer-substring start (point)))
    (message filename)
    filename))

;; 圧縮ファイルの中の1つのファイルの内容を表示する
;; *zipview*バッファに表示する
;; 非同期処理なので、処理が終わると、my-proc-state-sentinelを呼ぶ
(defun show-file (dirname filename)
  (interactive "sInput string dir:\nsInput string file:")
  (let (proc)
    (setq proc (start-process "fileview" "*zipview*" "unzip" "-p" dirname filename))
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
;;; 修正時刻： Tue Feb 18 07:30:51 2020

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
