(in-package :cl-threadpool-test)

#|
(define-test crashing-worker-test ()
  "Test that a crashing worker job doesn't kill the pool"
  (let ((pool (cl-threadpool:make-threadpool "testpool" 1)) (result nil))
    (cl-threadpool:start pool)
    (cl-threadpool:add-job
     pool
     (lambda ()
       (v:info :cl-threadpool "Processing job 1")
       (sleep 1) ()))
    (cl-threadpool:add-job
     pool
     (lambda ()
       (v:info :cl-threadpool "Processing job 2")
       (sleep 5)
       (setf result t)))
    (cl-threadpool:stop pool)
    (sleep 2)
    (assert-true result)))



|#
