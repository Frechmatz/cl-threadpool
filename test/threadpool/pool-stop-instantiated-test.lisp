(in-package :cl-threadpool-test)

(init-logger)

(define-test pool-stop-instantiated-test-1 ()
  "Stop pool after it has been instantiated but not started"
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-stop-instantiated-test-1")))
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:stop pool)
	(error (err) (setf got-error err)))
      (assert-false got-error))))


(define-test pool-stop-instantiated-test-2 ()
  "Stop pool two times after it has been instantiated but not started (this is allowd)"
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-stop-instantiated-test-2")))
    (cl-threadpool:stop pool)
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:stop pool)
	(error (err) (setf got-error err)))
      (assert-false got-error))))


