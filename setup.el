;;; $Id: setup.el,v 38960f8e0d9a 2004/02/27 06:32:50 simon $
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
