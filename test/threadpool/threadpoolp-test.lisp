(in-package :cl-threadpool-test)


(define-test threadpoolp-test-1 ()
  ""
  (let ((pool (cl-threadpool:make-threadpool 5)))
    (assert-true (cl-threadpool:threadpoolp pool))))

(define-test threadpoolp-test-2 ()
  ""
  (let ((pool (cl-threadpool:make-threadpool 5)))
    (declare (ignore pool))
    (assert-false (cl-threadpool:threadpoolp "foo"))))

