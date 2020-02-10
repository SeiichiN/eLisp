;;; err.el
;;; p99

(defun fold-within5 ()
  "5桁折り返す"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (move-to-column 5)
      (forward-line 1)
      (forward-line 1)
      )
    )
  )
