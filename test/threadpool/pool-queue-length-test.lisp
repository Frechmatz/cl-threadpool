(in-package :cl-threadpool-test)

(define-test pool-queue-length-test-1 ()
  ""
  (let ((pool (cl-threadpool:make-threadpool 1 :max-queue-size 3)))
    (cl-threadpool:start pool)
    (let ((got-error nil))
      (handler-case
	  (progn
	    ;; assume, that first three jobs will be immediately assigned to a worker thread
	    (cl-threadpool:add-job pool (lambda () (sleep 2)))
	    (cl-threadpool:add-job pool (lambda () (sleep 2)))
	    (cl-threadpool:add-job pool (lambda () (sleep 2)))
	    ;; now exceed max size of queue
	    (cl-threadpool:add-job pool (lambda () (sleep 1)))
	    (cl-threadpool:add-job pool (lambda () (sleep 1)))
	    (cl-threadpool:add-job pool (lambda () (sleep 1)))
	    (cl-threadpool:add-job pool (lambda () (sleep 1))))
	  (error (err) (setf got-error err)))
      (cl-threadpool:stop pool)
      (assert-true got-error))))

