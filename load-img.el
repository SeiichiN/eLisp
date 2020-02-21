;;; load-img.el --- 画像を表示する
;;
;; Copyright (c) 2020 Seiichi Nukayama
;;
;;; Code:

(defvar my-simple-image (create-image (expand-file-name "img.png"))
  "Simple Image")

(defun my-insert-image-at (image pos)
  "Insert image object IMAGE at position POS."
  (save-excursion
    (goto-char pos)
    (insert-image image)))

;; バッファの末尾に画像を表示する
;;(my-insert-image-at my-simple-image (point-max))

(defun my-display-image-region (beg end image)
  "Set 'display of text from BEG to END to image object IMAGE."
  (add-text-properties beg end (list 'display image)))

;; 領域を指定して、画像を表示する
;;(my-display-image-region (point-min) (point-max) my-simple-image)

(defun my-fetch-databytes (url)
  (let ((cb (generate-new-buffer " *fetched stream*")))
    (unwind-protect
        (save-excursion
          (set-buffer cb)
          (set-buffer-multibyte nil)
          (call-process
           shell-file-name nil t nil shell-command-switch
           (format "wget -g -O - %s" url))
          (buffer-string))
      (kill-buffer cb))))

(defvar my-mascot (create-image
                   (my-fetch-databytes
                    "http://www.yatex.org/elisp/img/img.png")
                   nil t))

(insert-image my-mascot)

(provide 'load-img)
;;; load-img.el ends here
;;; -----------------------------
;;; 修正時刻： Fri Feb 21 10:24:24 2020
 
