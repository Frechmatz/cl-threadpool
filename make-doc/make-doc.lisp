(in-package :cl-threadpool-make-doc)

;;
;; Helper functions
;;

(defun make-index (system)
  (docparser:parse system))

(defun get-index-node (index package-name symbol-name)
  (aref (docparser:query
	 index
	 :package-name (string-upcase package-name)
	 :symbol-name (string-upcase symbol-name))
	0))

(defun make-function-string (index package-name symbol-name)
  "Returns HTML representation of a function"
  (let* ((node (get-index-node index package-name symbol-name))
	 (lambda-list (docparser:operator-lambda-list node)))
    (concatenate
     'string
     "<b>" (string-downcase symbol-name) "</b>&nbsp;"
     (string-downcase (format nil "~a" (if lambda-list lambda-list "()")))
     "<p>" (docparser:node-docstring node) "</p>")))

(defun make-variable-string (index package-name symbol-name)
  "Returns HTML representation of a variable"
  (let ((node (get-index-node index package-name symbol-name)))
    (concatenate
     'string
     "<b>" (string-downcase symbol-name) "</b>"
     "<p>" (docparser:node-docstring node) "</p>")))

(defun make-condition-string (index package-name symbol-name)
  "Returns HTML representation of a condition."
  (let* ((node (get-index-node index package-name symbol-name)))
    (concatenate
     'string
     "<b>" (string-downcase symbol-name) "</b>"
     "<p>"  (docparser:node-docstring node) "</p>")))

(defun get-package-docstring (index package-name)
  (let ((docstring nil))
    ;; Did not found a better way yet :(
    (docparser:do-packages (package index)
      (if (string= (string-upcase package-name) (docparser:package-index-name package))
	  (setf docstring (docparser:package-index-docstring package))))
    (if (not docstring)
	(error "Package ~a not found" package-name))
    docstring))

(defun make-package-string (index package-name)
  (concatenate
   'string
   "<p>" (get-package-docstring index package-name) "</p>"))

(defun make-code-string (path)
  "Returns HTML representation of a source code file"
  (concatenate
   'string
   "<p><pre><code>"
   (cl-html-readme:read-file path :replace-tabs t :escape t)
   "</code></pre></p>"))

(defun now ()
  "Returns a string representing the current date and time."
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (get-decoded-time)
    (declare (ignore dow dst-p tz))
    ;; 2018-09-19 21:28:16
    (let ((str (format nil "~4,'0d-~2,'0d-~2,'0d  ~2,'0d:~2,'0d:~2,'0d" yr mon day hr min sec)))
      str)))

;;
;; Readme
;;

(defun get-readme (index doc-index)
  `("<html>"
    "<head><link href=\"styles.css\" rel=\"stylesheet\" type=\"text/css\"/></head>"
    "<body>"
    (semantic
     (:name "header")
     (heading
      (:name "cl-threadpool"))
     ,(cl-html-readme:read-file "make-doc/introduction.html")
     "<p>The source code is available <a href=\"https://github.com/Frechmatz/cl-threadpool\">here</a>.</p>")
    (semantic
     (:name "nav")
     (heading
      (:name "Table of contents")
      (toc)))
    (semantic
     (:name "section")
     (heading
      (:name "Examples" :toc t)
      (heading
       (:name ,(make-package-string doc-index "cl-threadpool-example-1"))
       ,(make-code-string "examples/example-1.lisp"))
      (heading
       (:name ,(make-package-string doc-index "cl-threadpool-example-2"))
       ,(make-code-string "examples/example-2.lisp")))
     (heading
      (:name "Change-Log" :toc t)
      (heading (:name "Version 1.0.0")
	       "<p>Initial release of cl-threadpool.</p>")
      (heading
       (:name "Version 2.0.0")
       "<p>Version 2 is a major rework of the thread pool with bugfixes, new features and removal of features that have been identified as not being useful.</p>"
       (heading
	(:name "Breaking changes")
	"<ul>"
	"<li>Removed add-job. Has been replaced with run-jobs.</li>"
	"<li>Removed :resignal-job-conditions argument from make-threadpool. The pool no longer handles conditions signalled by a job.</li>"
	"<li>Removed :max-queue-size argument from make-threadpool. The size of the job queue is now unlimited.</li>"
	" <li>Removed condition threadpool-error-queue-capacity-exceeded.</li>"
	"<li>Removed dependency 'verbose'. The library no longer depends on a logging framework.</li>"
	"</ul>")
       (heading
	(:name "New features")
	"<ul>"
	"<li>Added run-jobs for synchronous execution of jobs.</li>"
	"<li>Added queue-size to get the number of jobs waiting for execution.</li>"
	"<li>Added pool-name to get the name of a thread pool.</li>"
	"</ul>"))
      (heading
       (:name "Version 3.0.0")
       "<p>This version introduces futures and shall also be the last version coming with breaking changes.</p>"
       (heading
	(:name "Breaking changes")
	"<ul>"
	"<li>Removed cl-threadpool:start. A threadpool is now ready to use immediately after its instantiation.</li>"
	"<li>Removed condition threadpool-error. Pool usage errors and internal errors are now	represented by simple-error.</li>"
	"<li>cl-threadpool:stop no longer returns an indicator if the pool has successfully been stopped. The status can be checked via cl-threadpool:pool-stopped-p.</li>"
	"<li>Error handling of jobs has been reworked. The threadpool now catches all unhandled conditions of jobs. If a job has signalled a condition and its result is requested, a job-execution-error is signalled by the threadpool.</li>"
	"</ul>")
       (heading
	(:name "New features")
	"<ul>"
	"<li>Added add-job Adds a job to the queue and returns a future. The future represents the result of the asynchronously running job.</li>"
	"<li>Added job-result, cancel-job, job-done-p, job-cancelled-p.</li>"
	"<li>Added conditions job-execution-error and job-cancellation-error.</li>"
	"<li>Added pool-stopped-p.</li>"
	"</ul>"))
      (heading
       (:name "Version 3.0.1")
       "<p><b>This version is the current quicklisp release.</b></p>"
       "<p>Adapted to change in verbose library.</p>"))
     (heading
      (:name "Installation" :toc t)
      ,(cl-html-readme:read-file "make-doc/installation.html"))
     (heading
      (:name "API" :toc t)
      ,(make-function-string index "cl-threadpool" "make-threadpool")
      ,(make-function-string index "cl-threadpool" "run-jobs")
      ,(make-function-string index "cl-threadpool" "add-job")
      ,(make-function-string index "cl-threadpool" "stop")
      ,(make-function-string index "cl-threadpool" "threadpoolp")
      ,(make-function-string index "cl-threadpool" "queue-size")
      ,(make-function-string index "cl-threadpool" "pool-name")
      ,(make-function-string index "cl-threadpool" "pool-stopped-p")
      ,(make-function-string index "cl-threadpool" "worker-thread-p")
      ,(make-function-string index "cl-threadpool" "job-result")
      ,(make-function-string index "cl-threadpool" "job-done-p")
      ,(make-function-string index "cl-threadpool" "cancel-job")
      ,(make-function-string index "cl-threadpool" "job-cancelled-p")
      ,(make-condition-string index "cl-threadpool" "job-cancellation-error")
      ,(make-condition-string index "cl-threadpool" "job-execution-error")
      ,(make-function-string index "cl-threadpool" "job-execution-error-pool-name")
      ,(make-function-string index "cl-threadpool" "job-execution-error-message")
      ,(make-variable-string index "cl-threadpool" "*logger*"))
     (heading
      (:name "Run tests" :toc t)
      "<pre><code>(asdf:test-system :cl-threadpool)</code></pre>"
      (heading
       (:name "Tested Lisp implementations and operating systems")
       ,(cl-html-readme:read-file "make-doc/supported.html")))
     (heading
      (:name "Generate documentation" :toc t)
      ,(make-code-string "make-doc/generate-doc.lisp")))
    (semantic
     (:name "footer")
     "<hr/><p><small>Generated " ,(now) "</small></p>")
    "</body></html>"))

;;
;; Generate HTML file
;;

(defun make-doc ()
  (let ((cl-html-readme:*home-directory* (asdf:system-source-directory :cl-threadpool/doc))
	(cl-html-readme:*tab-width* 4)
	(index (make-index :cl-threadpool))
	(doc-index (make-index :cl-threadpool/doc)))
    (with-open-file
	(fh (cl-html-readme:make-path "docs/index.html")
	    :direction :output
	    :if-exists :supersede
	    :if-does-not-exist :create
	    :external-format :utf-8)
      (cl-html-readme:doc-to-html fh (get-readme index doc-index))))
  "DONE")

;;(make-doc)
