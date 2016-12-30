
(in-package :cl-threadpool-test)

(define-test pool-stopping-worker-thread ()
  "Test that pool cannot be stopped by a worker thread"
  (let ((pool (cl-threadpool:make-threadpool "testpool" 5)))
    (cl-threadpool:start pool)
    (let ((got-error nil))
      (cl-threadpool:add-job
       pool
       (lambda ()
	 (handler-case 
	     (cl-threadpool:stop pool)
	   (error (err) (setf got-error err)))))
      (cl-threadpool:stop pool)
      (assert-true got-error))))





