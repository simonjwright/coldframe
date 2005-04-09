;;; $Id: setup.el,v 284cc4ed467d 2005/04/09 07:48:55 simon $
;;;
;;; Sets up environment variables for ColdFrame development

(progn
  (let* ((home (getenv "HOME"))
	 (cf (concat home "/local/cf")))
    (setenv "ADA_PROJECT_PATH" cf)
    (setenv "BC" "local/bc")
    (setenv "BUILD_BASE" (concat cf "/.build"))
    (setenv "CASE_EXCEPTIONS"
	    (concat home
		    "/.emacs_case_exceptions"
		    ":"
		    cf
		    "/emacs_case_exceptions"))
    (setenv "DEVEL" "YES")
    (setenv "TASH" (concat home "/tash832a"))
    (setenv "TCL" "/usr/lib")
    (setenv "TCL_VERSION" "8.3")
    (setenv "TOP" home)
    ))
