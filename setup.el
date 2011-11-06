;;; Sets up environment variables for ColdFrame development (very
;;; specific to SJW config!)

(progn
  (let* ((home (getenv "HOME"))
	 (cf (concat home "/coldframe")))
    (setenv "ADA_PROJECT_PATH" cf)
    (setenv "BUILD_BASE" (concat cf "/.build"))
    (setenv "CASE_EXCEPTIONS"
	    (concat home
		    "/.emacs_case_exceptions"
		    ":"
		    cf
		    "/emacs_case_exceptions"))
    (setenv "COLDFRAME" "cf")
    ))
