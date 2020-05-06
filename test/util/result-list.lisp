(in-package :cl-threadpool-test)

(defun make-result-list ()
  "Collect results of jobs. A job result must be a string."
  (let ((lock (bt:make-lock))
	(results nil))
    (list
     :add (lambda (value)
	    "Add a result string."
	    (if (not (stringp value))
		(error "result-list::add Value must be a string"))
	    (bt:with-lock-held (lock)
	      (setf results (push value results))))
     :get (lambda()
	    "Return a clone of the current results."
	    (let ((r nil))
	      (bt:with-lock-held (lock)
		(setf r (copy-list results)))
	      r)))))
	       
	       
