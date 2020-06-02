
(in-package :cl-threadpool-test)

(init-logger)

(define-test pool-stopping-worker-thread ()
  "Test that pool cannot be stopped by a worker thread"
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-stopping-worker-thread")))
    (cl-threadpool:start pool)
    (let ((got-error nil))
      (cl-threadpool:run-jobs
       pool
       (list (lambda ()
	 (handler-case 
	     (cl-threadpool:stop pool)
	   (error (err) (setf got-error err))))))
      (cl-threadpool:stop pool)
      (assert-true got-error)
      (assert-true (typep got-error 'cl-threadpool:threadpool-error)))))





