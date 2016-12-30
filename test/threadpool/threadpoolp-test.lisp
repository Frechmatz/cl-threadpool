(in-package :cl-threadpool-test)


(define-test threadpoolp-test-1 ()
  ""
  (let ((pool (cl-threadpool:make-threadpool "testpool" 5)))
    (assert-true (cl-threadpool:threadpoolp pool))))

(define-test threadpoolp-test-2 ()
  ""
  (let ((pool (cl-threadpool:make-threadpool "testpool" 5)))
    (assert-false (cl-threadpool:threadpoolp "foo"))))

