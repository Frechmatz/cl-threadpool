(in-package :cl-threadpool-test)

(defparameter *initialized-logger* nil)

(defun init-logger ()
  (if (not *initialized-logger*)
      (progn
	(setf *initialized-logger* t)
	(setf cl-threadpool:*logger*
	      (lambda(level who format-control format-arguments)
		(let ((msg (apply #'format nil format-control format-arguments)))
		  (cond
		    ((eq level :info)
		     (verbose:info who msg))
		    ((eq level :debug)
		     (verbose:debug who msg))
		    ((eq level :warn)
		     (verbose:warn who msg))
		    ((eq level :error)
		     (verbose:error who msg))
		    ((eq level :trace)
		     (verbose:trace who msg)))))))))

