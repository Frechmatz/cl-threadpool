(in-package :cl-threadpool-test)

(define-test pool-add-job-test-1 ()
  "Add job to pool that hasn't been started"
  (let ((pool (cl-threadpool:make-threadpool "testpool" 5)))
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:add-job pool (lambda ()))
	(error (err) (setf got-error err)))
      (assert-true got-error))))

(define-test pool-add-job-test-2 ()
  "Add job to pool that is stopping"
  (let ((pool (cl-threadpool:make-threadpool "testpool" 5)))
    (cl-threadpool:start pool)
    (cl-threadpool:add-job
     pool
     (lambda () (sleep 10)))
    (cl-threadpool:stop pool)
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:add-job pool (lambda ()))
	(error (err) (setf got-error err)))
      (assert-true got-error))))
