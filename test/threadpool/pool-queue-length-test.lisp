(in-package :cl-threadpool-test)

(define-test pool-queue-size-test-1 ()
  ""
  (let ((pool (cl-threadpool:make-threadpool "testpool" 1 :max-queue-size 3)))
    (cl-threadpool:start pool)
    (let ((got-error nil))
      (handler-case
	  (progn
	    (cl-threadpool:add-job pool (lambda () (sleep 5)))
	    (cl-threadpool:add-job pool (lambda () (sleep 5)))
	    (cl-threadpool:add-job pool (lambda () (sleep 5)))
	    (cl-threadpool:add-job pool (lambda () (sleep 5)))
	    (cl-threadpool:add-job pool (lambda () (sleep 5))))
	  (error (err) (setf got-error err)))
      (cl-threadpool:stop pool)
      (assert-true got-error))))

