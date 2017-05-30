(in-package :cl-threadpool-test)

(define-test worker-thread-p-test-1 ()
  ""
  (let ((pool (cl-threadpool:make-threadpool 5)))
    (cl-threadpool:start pool)
    (let ((is-worker-thread nil))
      (cl-threadpool:add-job
       pool
       (lambda ()
	 (setf is-worker-thread (cl-threadpool:worker-thread-p pool))))
      (cl-threadpool:stop pool)
      (assert-true is-worker-thread))))

(define-test worker-thread-p-test-2 ()
  ""
  (let ((pool-1 (cl-threadpool:make-threadpool 5))
	(pool-2 (cl-threadpool:make-threadpool 5)))
    (cl-threadpool:start pool-1)
    (cl-threadpool:start pool-2)
    (let ((is-worker-thread t))
      (cl-threadpool:add-job
       pool-1
       (lambda ()
	 (setf is-worker-thread (cl-threadpool:worker-thread-p pool-2))))
      (cl-threadpool:stop pool-1)
      (cl-threadpool:stop pool-2)
      (assert-false is-worker-thread))))

