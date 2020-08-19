(in-package :cl-threadpool-make-doc)

;;
;; Helper functions
;;

(defun make-function-string (f)
  (concatenate
   'string
   "<p>"
   (cl-readme:sbcl-make-function-decl f)
   "</p>"
   "<p>"
   (documentation f 'function)
   "</p>"))

(defun make-variable-string (v)
  (concatenate
   'string
   "<b>"
   (string-downcase (package-name (symbol-package v))) ":"
   (string-downcase  (symbol-name v))
   "</b>"
   "<p>"
   (documentation v 'variable)
   "</p>"))

(defun make-condition-string (c)
  (concatenate
   'string
   "<b>"
   (string-downcase (package-name (symbol-package c))) ":"
   (string-downcase  (symbol-name c))
   "</b>"
   "<p>"
   (documentation c 'type)
   "</p>"))

(defun make-method-string (c)
  (concatenate
   'string
   "<b>"
   (string-downcase (package-name (symbol-package c))) ":"
   (string-downcase  (symbol-name c))
   "</b>"
   "<p>"
   (documentation c 'function)
   "</p>"))

;;
;; Readme
;;

(defun get-readme ()
  `("<html><body>"
    (semantic (:name "header")
	      (heading (:name "cl-threadpool"))
	      ,(cl-readme:read-verbatim "make-doc/introduction.html"))
    (semantic (:name "nav")
	      (heading (:name "Table of contents")
		       (toc)))
    (semantic (:name "section")
	      (heading (:name "Example" :toc t)
		       ,(cl-readme:read-code "examples/example-1.lisp"))
	      (heading (:name "Change-Log" :toc t)
		       ,(cl-readme:read-verbatim "make-doc/changelog.html"))
	      (heading (:name "Installation" :toc t)
		       ,(cl-readme:read-verbatim "make-doc/installation.html"))
	      (heading (:name "API" :toc t)
		       ,(make-function-string 'cl-threadpool:make-threadpool)
		       ,(make-function-string 'cl-threadpool:run-jobs)
		       ,(make-function-string 'cl-threadpool:add-job)
		       ,(make-function-string 'cl-threadpool:stop)
		       ,(make-function-string 'cl-threadpool:threadpoolp)
		       ,(make-function-string 'cl-threadpool:queue-size)
		       ,(make-function-string 'cl-threadpool:pool-name)
		       ,(make-function-string 'cl-threadpool:pool-stopped-p)
		       ,(make-function-string 'cl-threadpool:worker-thread-p)
		       ,(make-function-string 'cl-threadpool:job-value)
		       ,(make-function-string 'cl-threadpool:job-done-p)
		       ,(make-function-string 'cl-threadpool:cancel-job)
		       ,(make-function-string 'cl-threadpool:job-cancelled-p)
		       ,(make-condition-string 'cl-threadpool:job-cancellation-error)
		       ,(make-condition-string 'cl-threadpool:job-execution-error)
		       ,(make-method-string 'cl-threadpool:job-execution-error-pool-name)
		       ,(make-method-string 'cl-threadpool:job-execution-error-thread-id)
		       ,(make-method-string 'cl-threadpool:job-execution-error-message)
		       ;; TODO Improve documentation of *logger*
		       ;; ,(make-variable-string 'cl-threadpool:*logger*)
		       )
	      (heading (:name "Supported Lisp implementations and operating systems" :toc t)
		       ,(cl-readme:read-verbatim "make-doc/supported.html")))
    (semantic (:name "footer")
	      "<hr/><p><small>Generated " ,(cl-readme:current-date) "</small></p>")
    "</body></html>"))

;;
;; Generate HTML file
;;

(defun make-readme ()
  (let ((cl-readme:*home-directory* "/Users/olli/src/lisp/cl-threadpool/")
	(cl-readme:*tab-width* 8))
    (with-open-file (fh (cl-readme:make-path "docs/index.html")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf-8)
      (cl-readme:doc-to-html fh (get-readme))))
  "DONE")

;;(make-readme)
