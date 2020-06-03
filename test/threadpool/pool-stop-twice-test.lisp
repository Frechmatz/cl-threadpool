
(in-package :cl-threadpool-test)

(init-logger)

(define-test pool-stop-twice-test-1 ()
  "Nested pool stopping (this is allowed)."
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-stop-twice-test-1")))
    (cl-threadpool:start pool)
    (cl-threadpool:stop pool)
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:stop pool)
	(error (err) (setf got-error err)))
      (assert-false got-error))))

