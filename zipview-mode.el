;;; zipview-mode.el --- zipファイル閲覧モード
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
  (interactive)
  (save-excursion
    (let ((work-buffer (get-buffer-create "*zipview*")) proc)
      (progn
        (switch-to-buffer "*zipview*")
        (message "Current buffer is %s" (buffer-name))
        (setq major-mode 'zipview-mode
              mode-name "zipviewモード")
        (erase-buffer)
        (setq proc (start-process "zipview" work-buffer "unzip" "-l" zipfile))
        (set-process-sentinel proc 'my-zipview-sentinel)))))

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


(provide 'zipview-mode)
;;; zipview-mode.el ends here
;;;--------------------------------
;;; 修正時刻： Tue Feb 18 06:50:55 2020
