;;; $Id: cf-banner.el,v 24c412839626 2003/07/04 18:23:03 simon $
;;; This GNU Emacs lisp file is part of ColdFrame.
;;;
;;; It removes banner comment at the top of Ada source files,
;;; replacing it (in this version) with a heading appropriate for PVCS
;;; revision control.

(defun cf-banner ()
  "Replace ColdFrame's initial 'edit this' banner by a customized one.
This version is for use with PVCS."
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
	(widen)
	; get rid of ColdFrame's banner (fail if it isn't ColdFrame's)
	(goto-char (point-min))
	(re-search-forward "^--\\(-\\)+$"
			   (save-excursion (end-of-line) (point)))
	(forward-char)
	(re-search-forward "^--  Automatically generated: \\(\\(edit this!\\)\\|\\(should not need editing\\)\\)  --$"
			(save-excursion (end-of-line) (point)))
	(forward-char)
	(re-search-forward "^--\\(-\\)+$"
			   (save-excursion (end-of-line) (point)))
	(forward-char)
	(delete-region (point-min) (point))
	; banner start
	(insert
	 "------------------------------------------------------------------------------")
	(newline)
	; --  <last two directories on path>
	(insert "--  ")
	(cf--make-dir-spec)
	(newline)
	; PM is the filename subsitution
	(insert "--  %PM%")
	(newline)
	; PR is the revision, PRT is the date
	(insert "--  %PR%  %PRT%")
	(newline)
	; banner end
	(insert
	 "------------------------------------------------------------------------------")
	(newline)))))

(defun cf--make-dir-spec ()
  "Insert a representation of the current file's location in the buffer.
Uses the last two directory name components."
  (let ((d (char-to-string directory-sep-char)))
    (narrow-to-region (point) (point))
    (insert (buffer-file-name))
    (goto-char (point-min))
    (while (search-forward d nil t)
      (replace-match "/" nil t))
    (re-search-backward "/")
    (delete-region (point-max) (+ (point) 1))
    (re-search-backward "/")
    (re-search-backward "/")
    (forward-char)
    (delete-region (point-min) (point))
    (end-of-line)
    (widen)))

(provide 'cf-banner)

