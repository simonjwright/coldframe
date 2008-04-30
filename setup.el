;;; $Id: setup.el,v c59ee9194671 2008/04/30 05:00:46 simonjwright $
;;;
;;; Sets up environment variables for ColdFrame development

(progn
  (let* ((home (getenv "HOME"))
	 (cf (concat home "/cf")))
    (setenv "ADA_PROJECT_PATH" cf)
    (setenv "AUNIT" "AUnit-1.03p")
    (setenv "BC" "bc")
    (setenv "BUILD_BASE" (concat cf "/.build"))
    (setenv "CASE_EXCEPTIONS"
	    (concat home
		    "/.emacs_case_exceptions"
		    ":"
		    cf
		    "/emacs_case_exceptions"))
    (setenv "COLDFRAME" "cf")
    (cond
     ;; see notes in setup.sh
     ((or (memq system-type '(gnu/linux)) (memq system-type '(darwin)))
      (setenv "ADA_PROJECT_PATH"
	      (concat 
	       (getenv "ADA_PROJECT_PATH")
	       ":"
	       home "/tcladashell"))))
    (setenv "TOP" home)
    ))
