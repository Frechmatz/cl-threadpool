(in-package :cl-threadpool-test)

(init-logger)

(define-test threadpoolp-test-1 ()
  ""
  (let ((pool (cl-threadpool:make-threadpool 5 :name "threadpoolp-test-1")))
    (assert-true (cl-threadpool:threadpoolp pool))))

(define-test threadpoolp-test-2 ()
  ""
  (let ((pool (cl-threadpool:make-threadpool 5 :name "threadpoolp-test-2")))
    (declare (ignore pool))
    (assert-false (cl-threadpool:threadpoolp "foo"))))

