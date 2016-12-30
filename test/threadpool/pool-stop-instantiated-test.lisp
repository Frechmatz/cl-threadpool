(in-package :cl-threadpool-test)

(define-test pool-stop-instantiated-test-1 ()
  "Stop pool after it has been instantiated but not started"
  (let ((pool (cl-threadpool:make-threadpool "testpool" 5)))
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:stop pool)
	(error (err) (setf got-error err)))
      (assert-false got-error))))


(define-test pool-stop-instantiated-test-2 ()
  "Stop pool two times after it has been instantiated but not started"
  (let ((pool (cl-threadpool:make-threadpool "testpool" 5)))
    (cl-threadpool:stop pool)
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:stop pool)
	(error (err) (setf got-error err)))
      (assert-true got-error))))


