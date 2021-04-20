;;; rotate-text-show-paren.el --- Description -*- lexical-binding: t; -*-
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

(defun rotate-text-paren-show-paren--locate-after()
  "Locate an unescaped paren after point to show.
If one is found, return the cons (DIR . OUTSIDE), where DIR is 1
for an open paren, -1 for a close paren."
  (let* ((ind-pos (save-excursion (back-to-indentation) (point)))
	 (eol-pos
	  (save-excursion
	    (end-of-line) (skip-chars-backward " \t" ind-pos) (point)))
	 (after (show-paren--categorize-paren (point))))
    after
    ))

;; HACK: Function compied  from paren.el as original one is compiled and doesnt cooperate with cl-flet.
(defun rotate-text-paren-show-paren--default ()
  "Find the opener/closer near point and its match.

It is the default value of `show-paren-data-function'."
  (let* ((temp (rotate-text-paren-show-paren--locate-after))
	 (dir (car temp))
	 (outside (cdr temp))
	 pos mismatch here-beg here-end)
    ;;
    ;; Find the other end of the sexp.
    (when dir
      (setq here-beg (if (eq dir 1) outside (1- outside))
	    here-end (if (eq dir 1) (1+ outside) outside))
      (save-restriction
	;; Determine the range within which to look for a match.
	(when blink-matching-paren-distance
	  (narrow-to-region
	   (max (point-min) (- (point) blink-matching-paren-distance))
	   (min (point-max) (+ (point) blink-matching-paren-distance))))
	;; Scan across one sexp within that range.
	;; Errors or nil mean there is a mismatch.
	(condition-case ()
	    (setq pos (scan-sexps outside dir))
	  (error (setq pos t mismatch t)))
	;; Move back the other way and verify we get back to the
	;; starting point.  If not, these two parens don't really match.
	;; Maybe the one at point is escaped and doesn't really count,
	;; or one is inside a comment.
	(when (integerp pos)
	  (unless (condition-case ()
		      (eq outside (scan-sexps pos (- dir)))
		    (error nil))
	    (setq pos nil)))
	;; If found a "matching" paren, see if it is the right
	;; kind of paren to match the one we started at.
	(if (not (integerp pos))
	    (if mismatch (list here-beg here-end nil nil t))
	  (let ((beg (min pos outside)) (end (max pos outside)))
	    (unless (eq (syntax-class (syntax-after beg)) 8)
	      (setq mismatch
		    (not (or (eq (char-before end)
				 ;; This can give nil.
				 (cdr (syntax-after beg)))
			     (eq (char-after beg)
				 ;; This can give nil.
				 (cdr (syntax-after (1- end))))
			     ;; The cdr might hold a new paren-class
			     ;; info rather than a matching-char info,
			     ;; in which case the two CDRs should match.
			     (eq (cdr (syntax-after (1- end)))
				 (cdr (syntax-after beg)))))))
	    (list here-beg here-end
		  (if (= dir 1) (1- pos) pos)
		  (if (= dir 1) pos (1+ pos))
		  mismatch)))))))

(defun rotate-text-paren-show-paren-get-data()
  (let*
      ((parens (rotate-text-paren-show-paren--default))
       (start (if (< (nth 0 parens) (nth 2 parens))
                  (nth 0 parens) (nth 2 parens)))
       (end (if (< (nth 0 parens) (nth 2 parens))
                (nth 2 parens) (nth 0 parens)))
       (startchar (buffer-substring-no-properties start (1+ start)))

       )
    (list :start start :end end :left startchar)
    )
  )

(provide 'rotate-text-show-paren)
;;; rotate-text-show-paren.el ends here
