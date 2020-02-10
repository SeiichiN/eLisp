;;; wday.el
;;; p105
;;;

(defun my-select-start-s (wday)
  (string-match "^S" (car wday))
  (let ((wday-alist '(("Sunday") ("Monday") ("Tuesday") ("Wednesday")
                      ("Thrusday") ("Friday") ("Saturday")))
        (completion-ignore-case t))
    (completing-read "What day?: " wday-alist 'my-select-start-s t))
  )


;;; 曜日を保管入力する
(let ((wday-alist '(("Sunday") ("Monday") ("Tuesday") ("Wednesday")
                    ("Thrusday") ("Friday") ("Saturday")))
      (completion-ignore-case t))
  (completing-read "What day?: " wday-alist nil t))








