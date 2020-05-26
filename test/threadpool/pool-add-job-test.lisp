(in-package :cl-threadpool-test)

(init-logger)

(define-test pool-add-job-test-1 ()
  "Add job to pool that hasn't been started"
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-add-job-test-1")))
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:add-job pool (lambda ()))
	(error (err) (setf got-error err)))
      (assert-true got-error)
      (assert-true (typep got-error 'cl-threadpool:threadpool-error)))))

(define-test pool-add-job-test-2 ()
  "Add job to pool that has stopped"
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-add-job-test-2")))
    (cl-threadpool:start pool)
    (cl-threadpool:add-job
     pool
     (lambda () ()))
    (cl-threadpool:stop pool)
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:add-job pool (lambda ()))
	(error (err) (setf got-error err)))
      (assert-true got-error)
      (assert-true (typep got-error 'cl-threadpool:threadpool-error)))))
