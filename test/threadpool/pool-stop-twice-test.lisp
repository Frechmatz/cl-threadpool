
(in-package :cl-threadpool-test)

(define-test pool-stop-twice-test-1 ()
  "Stop pool after it has already been stopped."
  (let ((pool (cl-threadpool:make-threadpool "testpool" 5)))
    (cl-threadpool:start pool)
    (cl-threadpool:add-job
     pool
     (lambda ()))
    (cl-threadpool:stop pool)
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:stop pool)
	(error (err) (setf got-error err)))
      (assert-true got-error))))

