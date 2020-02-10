;;; my-completing-buffer-word
;;; バッファ中にある単語をすべて記憶し、補完入力できるようにする
;;; p112

(defun my-completing-buffer-word ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (word word-alist)
      (while (not (eobp))
        (re-search-forward "^[A-Za-z]+" nil t)
        (setq word (match-string 0))
        (setq word-alist (cons (list word) word-alist))
        (forward-line 1))
      ;(message word-alist)
      (completing-read "単語?:" word-alist nil 1)
      )))














