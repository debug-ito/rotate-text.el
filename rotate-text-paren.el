;;; rotate-text-paren.el --- Description -*- lexical-binding: t; -*-
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

(defvar rotate-text-paren-pairs  '(("()" "{}" "[]"))
  "*List of paren pair groups to rotate.")

(defvar rotate-text-paren-get-paren-data-function #'rotate-text-paren-smartparens-get-data
  "Function to find paren position for current point.
Return (list :start <pos-before-start-char> :end <pos-before-end-char> :left <left-paren-char>)."
  )

(defun rotate-text-paren--symbols ()
  "."
  (mapcar

   (lambda (pairs)
     (mapcan
      (lambda (pair)
        (list
         (string-to-char (substring pair 0 1))
         (string-to-char (substring pair 1 2))
         ))
      pairs)) rotate-text-paren-pairs)
  )

(defun rotate-text-paren--left->pair (paren-group) ;TODO: cleanup
  "."
  (let
      (
       (startchars  (mapcar (lambda (pair) (substring pair 0 1) ) paren-group ))
       (rotated-pairs (cons (car (last paren-group)) (butlast paren-group)))
       )

    (require 'dash)
    (-zip startchars rotated-pairs)
    )
  )

(defun rotate-text-paren-rotate-parens ()
  "Toggle parens at cursor.
Utilizes modified `rotate-text-paren-get-paren-data-function'
with smartparens or show-paren backend to find and replace them
with the other pair of brackets. This function can be easily
modified and expanded to replace other brackets. Currently,
mismatch information is ignored and mismatched parens are changed
based on the left one."
  (interactive)
  (save-excursion
    (let* ((parens (funcall rotate-text-paren-get-paren-data-function))
           (start (plist-get parens :start))
           (end (plist-get parens :end))
           (startchar (plist-get parens :left))
           )
      (when parens
        (let
            ((pair (cl-find-if (lambda (p) (s-equals? (car p) startchar )) (rotate-text-paren--left->pair (car rotate-text-paren-pairs)))))
          (if pair
              (rotate-text-paren--replace (cdr pair) start end)
            (message "toggle-parens : missing paren")
            ))))))

(defun rotate-text-paren--replace (pair start end)
  "Replace parens with a new PAIR at START and END in current buffer.

A helper function for `rotate-text-paren-rotate-parens'."
  (goto-char start)
  (delete-char 1)
  (insert (substring pair 0 1))
  (goto-char end)
  (delete-char 1)
  (insert (substring pair 1 2)))


(defun rotate-text-paren-or-text(arg &optional default-string com-symbols com-words com-patterns)
  "."
  (interactive (list (if (consp current-prefix-arg)
                         -1
                       (prefix-numeric-value current-prefix-arg))))

  (if (cl-find (char-after) (flatten-list (rotate-text-paren--symbols)))
      (rotate-text-paren-rotate-parens)
    (rotate-text arg default-string com-symbols com-words com-patterns)
    )
  )



(provide 'rotate-text-paren)
;;; rotate-text-paren.el ends here
