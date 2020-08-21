(in-package :cl-threadpool-test)

(init-logger)

(define-test add-job-1 ()
  "Add a job and get result from future"
  (let ((pool (cl-threadpool:make-threadpool 2 :name "add-job-1")))
    (let ((future (cl-threadpool:add-job pool (lambda() (sleep 5) "Job 1"))))
      (let ((result (cl-threadpool:job-result future)))
	(cl-threadpool:stop pool)
	(assert-true (cl-threadpool:pool-stopped-p pool))
	(assert-equal "Job 1" result)))))

(define-test add-job-2 ()
  "Add some jobs and get results from futures"
  (let ((pool (cl-threadpool:make-threadpool 2 :name "add-job-2")))
    (let ((future-1 (cl-threadpool:add-job pool (lambda() (sleep 7) "Job 1")))
	  (future-2 (cl-threadpool:add-job pool (lambda() (sleep 5) "Job 2")))
	  (future-3 (cl-threadpool:add-job pool (lambda() (sleep 1) "Job 3")))
	  (future-4 (cl-threadpool:add-job pool (lambda() (sleep 3) "Job 4")))
	  (future-5 (cl-threadpool:add-job pool (lambda() "Job 5"))))
      (let ((result-4 (cl-threadpool:job-result future-4))
	    (result-1 (cl-threadpool:job-result future-1))
	    (result-3 (cl-threadpool:job-result future-3))
	    (result-2 (cl-threadpool:job-result future-2))
	    (result-5 (cl-threadpool:job-result future-5)))
	(cl-threadpool:stop pool)
	(assert-true (cl-threadpool:pool-stopped-p pool))
	(assert-equal "Job 1" result-1)
	(assert-equal "Job 2" result-2)
	(assert-equal "Job 3" result-3)
	(assert-equal "Job 4" result-4)
	(assert-equal "Job 5" result-5)))))

(define-test add-job-execution-error ()
  "Test that a condition signalled by a worker is properly handled."
  (let ((pool (cl-threadpool:make-threadpool 2 :name "error-handling-add-job")))
    (let ((catched-error nil)
	  (future (cl-threadpool:add-job
		   pool
		   (lambda()
		     (sleep 1)
		     (error "Job failure")))))
      (handler-case
	  (cl-threadpool:job-result future)
	(error (err)
	  (setf catched-error err)))
      (cl-threadpool:stop pool)
      (assert-true (cl-threadpool:pool-stopped-p pool))
      (assert-true catched-error)
      (assert-true (typep
		    catched-error
		    'cl-threadpool:job-execution-error)))))
