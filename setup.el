;;; $Id: setup.el,v 9ae8f34fc402 2003/12/13 06:15:12 simon $
;;;
;;; Sets up environment variables for ColdFrame development

(progn
  (let ((home (getenv "HOME")))
    (setenv "ADA_PROJECT_PATH" (concat home "/cf"))
    (setenv "BC" "bc")
    (setenv "BUILD_DIR" (concat home "/cf/.build"))
    (setenv "CASE_EXCEPTIONS"
	    (concat home
		    "/.emacs_case_exceptions"
		    ":"
		    home
		    "/cf/emacs_case_exceptions"))
    (setenv "DEVEL" "YES")
    (setenv "TASH" "tash832a")
    (setenv "TCL" "/usr/lib")
    (setenv "TCL_VERSION" "8.3")
    (setenv "TOP" home)
    ))