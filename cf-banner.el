;;; $Id: cf-banner.el,v 97a59a968357 2003/07/03 21:42:16 simon $
;;; This GNU Emacs lisp file is part of ColdFrame.
;;;
;;; It removes banner comment at the top of Ada source files,
;;; replacing it (in this version) with a heading appropriate for PVCS
;;; revision control.

(defun cf-banner ()
  "Replace ColdFrame's initial "edit this" banner by a customized one.
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
	(search-forward "--  Automatically generated: edit this!  --"
			(save-excursion (end-of-line) (point)))
	(forward-char)
	(re-search-forward "^--\\(-\\)+$"
			   (save-excursion (end-of-line) (point)))
	(forward-char)
	(delete-region (point-min) (point))
	; banner start
	(insert
	 "------------------------------------------------------------------------")
	(newline)
	; --  <last two directories on path>
	(insert "--  ")
	(let ((d (char-to-string directory-sep-char))
	      (start (point))
	      (end))
	  (insert (buffer-file-name))
	  (setq end (point))
	  (re-search-backward d)
	  (delete-region end (point))
	  (re-search-backward d)
	  (re-search-backward d)
	  (forward-char)
	  (delete-region start (point))
	  (end-of-line)
	  (insert d))
	(newline)
	; PM is the filename subsitution
	(insert "--  %PM%")
	(newline)
	; PR is the revision, PRT is the date
	(insert "--  %PR%  %PRT%")
	(newline)
	; banner end
	(insert
	 "------------------------------------------------------------------------")
	(newline)))))

(provide 'cf-banner)

