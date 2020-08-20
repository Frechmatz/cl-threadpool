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

(defun make-package-string (p)
  (documentation (find-package p) t))

;;
;; Readme
;;

(defun get-readme ()
  `("<html><body>"
    (semantic (:name "header")
	      (heading (:name "cl-threadpool"))
	      ,(cl-readme:read-verbatim "make-doc/introduction.html")
	      "<p>The source code is available <a href=\"https://github.com/Frechmatz/cl-threadpool\">here</a>.</p>")
    (semantic (:name "nav")
	      (heading (:name "Table of contents")
		       (toc)))
    (semantic (:name "section")
	      (heading (:name "Examples" :toc t)
		       (heading (:name ,(make-package-string 'cl-threadpool-example-1))
				,(cl-readme:read-code "examples/example-1.lisp"))
		       (heading (:name ,(make-package-string 'cl-threadpool-example-2))
				,(cl-readme:read-code "examples/example-2.lisp")))
	      ;; Yes, the change-log looks messy.
	      ;; But this approach generates proper headings and
	      ;; gives us the possibility to add them to the TOC.
	      (heading (:name "Change-Log" :toc t)
		       (heading (:name "Version 1.0.0")
				"<p>Initial release of cl-threadpool.</p>")
		       (heading (:name "Version 2.0.0")
				"<p><b>This version is the current quicklisp release.</b></p>"
				"<p>Version 2 is a major rework of the thread pool with bugfixes, new features and removal of features that have been identified as not being useful.</p>"
				(heading (:name "Breaking changes")
					 "<ul>"
					 "<li>Removed add-job. Has been replaced with run-jobs.</li>"
					 "<li>Removed :resignal-job-conditions argument from make-threadpool. The pool no longer handles conditions signalled by a job.</li>"
					 "<li>Removed :max-queue-size argument from make-threadpool. The size of the job queue is now unlimited.</li>"
					 " <li>Removed condition threadpool-error-queue-capacity-exceeded.</li>"
					 "<li>Removed dependency 'verbose'. The library no longer depends on a logging framework.</li>"
					 "</ul>")
				(heading (:name "New features")
					 "<ul>"
					 "<li>Added run-jobs for synchronous execution of jobs.</li>"
					 "<li>Added queue-size to get the number of jobs waiting for execution.</li>"
					 "<li>Added pool-name to get the name of a thread pool.</li>"
					 "</ul>"))
		       (heading (:name "Version 3.0.0")
				"<p>This version introduces futures and shall also be the last version coming with breaking changes.</p>"
				(heading (:name "Breaking changes")
					 "<ul>"
					 "<li>Removed cl-threadpool:start. A threadpool is now ready to use immediately after its instantiation.</li>"
					 "<li>Removed condition threadpool-error. Pool usage errors and internal errors are now	represented by simple-error.</li>"
					 "<li>cl-threadpool:stop no longer returns an indicator if the pool has successfully been stopped. The status can be checked via cl-threadpool:pool-stopped-p.</li>"
					 "<li>Error handling of jobs has been reworked. The threadpool now catches all unhandled conditions of jobs. If a job has signalled a condition and its result is requested, a job-execution-error is signalled by the threadpool.</li>"
					 "</ul>")
				(heading (:name "New features")
					 "<ul>"
					 "<li>Added add-job Adds a job to the queue and returns a future. The future represents the result of the asynchronously running job.</li>"
					 "<li>Added job-value, cancel-job, job-done-p, job-cancelled-p.</li>"
					 "<li>Added conditions job-execution-error and job-cancellation-error.</li>"
					 "<li>Added pool-stopped-p.</li>"
					 "</ul>")))
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
		       ,(make-function-string 'cl-threadpool:job-execution-error-pool-name)
		       ,(make-function-string 'cl-threadpool:job-execution-error-thread-id)
		       ,(make-function-string 'cl-threadpool:job-execution-error-message)
		       ,(make-variable-string 'cl-threadpool:*logger*))
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
