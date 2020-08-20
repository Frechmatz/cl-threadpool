(defpackage :cl-threadpool-example-1
  (:documentation "Synchronously execute a batch of jobs")
  (:use :cl))
(in-package :cl-threadpool-example-1)

(defun example()
  (let ((threadpool (cl-threadpool:make-threadpool 5 :name "Example thread pool")))
    (let ((results
           (cl-threadpool:run-jobs
            threadpool
            (list
             (lambda() (sleep 5) "Batch-Job 1")
             (lambda() (sleep 2) "Batch-Job 2")
             (lambda() (sleep 1) "Batch-Job 3")))))
      (format t "~%~a" (first results)) ;; => "Batch-Job 1"
      (format t "~%~a" (second results)) ;; => "Batch-Job 2"
      (format t "~%~a" (third results))) ;; => "Batch-Job 3"
      (cl-threadpool:stop threadpool)))

;; (example)

