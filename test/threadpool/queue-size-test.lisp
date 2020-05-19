(in-package :cl-threadpool-test)

(define-test queue-size-test-1 ()
  (let ((pool (cl-threadpool:make-threadpool 1)))
    (cl-threadpool:start pool)
    (let ((results
	   (cl-threadpool:run-jobs
	    pool
	    (list
	     (lambda() "Job 1")
	     (lambda() "Job 2")
	     (lambda() "Job 3")))))
      (let ((l (cl-threadpool:queue-size pool)))
	(cl-threadpool:stop pool)
	(assert-equal 0 l)))))

