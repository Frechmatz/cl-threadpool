(in-package :cl-threadpool-test)

(init-logger)

(define-test error-handling-add-job ()
  "Test that a condition signalled by a worker is properly handled."
  (let ((pool (cl-threadpool:make-threadpool 2 :name "error-handling-add-job")))
    (cl-threadpool:start pool)
    (let ((catched-error nil)
	  (future (cl-threadpool:add-job
		   pool
		   (lambda()
		     (sleep 1)
		     (error "Job failure")))))
      (handler-case
	  (cl-threadpool:future-value future)
	(error (err)
	  (setf catched-error err)))
      (cl-threadpool:stop pool)
      (assert-true catched-error)
      (assert-true (typep
		    catched-error
		    'cl-threadpool:threadpool-execution-error)))))

(define-test error-handling-run-jobs ()
  "Run a batch of jobs and properly handle unhandled job errors."
  (let ((pool (cl-threadpool:make-threadpool 2 :name "error-handling-run-jobs")))
    (cl-threadpool:start pool)
    (let ((catched-error nil))
      (handler-case
	  (cl-threadpool:run-jobs
	   pool
	   (list
	    (lambda() (sleep 1) "Job 1")
	    (lambda() (sleep 2) (error "Job 2 Error"))
	    (lambda() (sleep 3) "Job 3")))
	(error (err)
	  (progn
	    (setf catched-error err))))
      (let ((l (cl-threadpool:queue-size pool)))
	(cl-threadpool:stop pool)
	(assert-true catched-error)
	(assert-true (typep
		      catched-error
		      'cl-threadpool:threadpool-execution-error))
	(assert-equal 0 l)))))

