;;; $Id: setup.el,v 499c4d3b554e 2005/05/14 16:36:11 simonjwright $
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
    (cond
     ;; see notes in setup.el
     ((memq system-type '(gnu/linux))
      (progn 
	(setenv "TASH" (concat home "/tash832a"))
	(setenv "TCL" "/usr/lib")
	(setenv "TCL_VERSION" "8.3")
	))
     ((memq system-type '(darwin))
      (progn 
	(setenv "TASH" (concat home "/tash841a"))
	(setenv "TCL" "/usr/local")
	(setenv "TCL_VERSION" "8.4")
	)))
    (setenv "TOP" home)
    ))
