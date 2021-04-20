;;; rotate-text-smartparens.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021
;;
;; Author:  <https://github.com/JakDar>
;; Created: April 20, 2021
;; Modified: April 20, 2021
;; Version: 0.0.1
;; Homepage: https://github.com/JakDar/rotate-text.el
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(defun rotate-text-paren-smartparens-get-data()
  "Adapter fro `sp-get-thing' to match api of `rotate-text-paren-get-paren-data-function'."
  (let ((p  (sp-get-thing)))
    (list :start (sp-get p :beg) :end (- (sp-get p :end) 1) :left (sp-get p :op))
    )
  )


(provide 'rotate-text-smartparens)
;;; rotate-text-smartparens.el ends here
