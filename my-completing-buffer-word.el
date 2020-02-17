;;; my-completing-buffer-word.el ---  バッファ中にある単語をすべて記憶し、補完入力できるようにする
;; p112
;; Copyright (c) 2020 by Seiichi Nukayama
;;
;;; Code:

(defun my-completing-buffer-word ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (word word-alist)
      (while (not (eobp))
        (re-search-forward "^[A-Za-z]+" nil t)
        (setq word (match-string 0))
        (if (not (my-check-word word word-alist))
          (setq word-alist (cons (list word) word-alist)))
        (forward-line 1))
      ;(message word-alist)
      (completing-read "単語?:" word-alist nil 1)
      )))

(defun my-check-word (word word-alist)
  (interactive)
  (cond
   ((null word-alist) nil)
   ((equal word (car (car word-alist))) word)
   (t
    (my-check-word word (cdr word-alist)))
   ))

(provide 'my-completing-buffer-word)
;;; my-completing-buffer-word.el end here
;;; ------------------------------------
;;; 修正時刻： Mon Feb 17 23:14:04 2020
