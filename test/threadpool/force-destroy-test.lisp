(in-package :cl-threadpool-test)

(init-logger)

(define-test force-destroy-test-1 ()
  "Test will not return when thread won't be force destroyed"
  (let ((pool (cl-threadpool:make-threadpool 5 :name "force-destroy-test-1")))
    (cl-threadpool:start pool)
    (cl-threadpool:add-job
     pool
     (lambda () (loop
		   (sleep 1)
		   (cl-threadpool::write-log
		    :info
		    :cl-threadpool-test
		    "I will not terminate"))))
    (cl-threadpool:stop pool :force-destroy-timeout-seconds 5)
    (assert-true t)))

