;;; $Id: setup.el,v cbb0273e2b14 2004/01/16 07:11:42 simon $
;;;
;;; Sets up environment variables for ColdFrame development

(progn
  (let ((home (getenv "HOME")))
    (setenv "ADA_PROJECT_PATH" (concat home "/cf"))
    (setenv "BC" "bc")
    (setenv "BUILD_BASE" (concat home "/cf/.build"))
    (setenv "CASE_EXCEPTIONS"
	    (concat home
		    "/.emacs_case_exceptions"
		    ":"
		    home
		    "/cf/emacs_case_exceptions"))
    (setenv "DEVEL" "YES")
    (setenv "TASH" (concat home "/tash832a"))
    (setenv "TCL" "/usr/lib")
    (setenv "TCL_VERSION" "8.3")
    (setenv "TOP" home)
    ))
