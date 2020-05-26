(in-package :cl-threadpool-test)

(init-logger)

(define-test pool-start-twice-test ()
  "Start pool twice"
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-start-twice-test")))
    (cl-threadpool:start pool)
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:start pool)
	(error (err) (setf got-error err)))
      (assert-true got-error)
      (assert-true (typep got-error 'cl-threadpool:threadpool-error)))))

