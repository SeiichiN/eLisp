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
        (set-process-sentinel proc 'my-proc-state-sentinel)))))
        ;; (set-process-sentinel proc 'my-zipview-sentinel)))))

(defun my-zipview-sentinel (proc state)
  "my-zipviewの終了時に呼ばれる関数"
  (let ((ps (process-status proc))
        (curwin (selected-window))
        (result-buffer (process-buffer proc)))
    (cond
     ((eq ps 'exit)
      (pop-to-buffer result-buffer)
      (goto-char (point-min))
      (if (re-search-forward "Archive:" nil t)
          (message "一覧取得成功")
        (message "一覧取得に失敗しました"))
      (select-window curwin))
     (t nil))))

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

(defun show-file (dirname filename)
  (interactive "sInput string dir:\nsInput string file:")
  (let (proc)
    (setq proc (start-process "fileview" "*zipview*" "unzip" "-p" dirname filename))
    (set-process-sentinel proc 'my-proc-state-sentinel)
    ))

(defun my-proc-state-sentinel (proc state)
  "show-file のあとに呼ばれる関数")
(let ((ps (process-status proc))
      (curwin (selected-window))
      (result-buffer (process-buffer proc)))
  (cond
   ((eq ps 'exit)
    (pop-to-buffer result-buffer)
    (goto-char (point-min))
    (if (re-search-forward "exited abnormally" nil t)
        (insert "失敗しました")
      (message "成功しました"))
    (select-window curwin))
   (t nil)))

(provide 'my-zipview-mode)
;;; my-zipview-mode.el ends here
;;;--------------------------------
;;; 修正時刻： Tue Feb 18 07:30:51 2020

