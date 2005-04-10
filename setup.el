;;; $Id: setup.el,v b28ada3f0750 2005/04/10 17:41:07 simon $
;;;
;;; Sets up environment variables for ColdFrame development

(progn
  (let* ((home (getenv "HOME"))
	 (cf (concat home "/local/cf")))
    (setenv "ADA_PROJECT_PATH" cf)
    (setenv "AUNIT" "AUnit-1.03p")
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
