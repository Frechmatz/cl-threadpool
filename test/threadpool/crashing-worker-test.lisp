(in-package :cl-threadpool-test)


(define-test crashing-worker-test ()
  "Test that a crashing worker job doesn't kill the pool"
  (let ((pool (cl-threadpool:make-threadpool "testpool" 1)) (result nil))
    (cl-threadpool:start pool)
    (cl-threadpool:add-job
     pool
     (lambda ()
       (sleep 1) (error "Nope")))
    (cl-threadpool:add-job
     pool
     (lambda ()
       (sleep 5)
       (setf result t)))
    (cl-threadpool:stop pool)
    (sleep 2)
    (assert-true result)))


