(defpackage :cl-threadpool-example-2
  (:documentation "Add some jobs and retrieve their results in a later point of time")
  (:use :cl))
(in-package :cl-threadpool-example-2)

(defun example()
  (let ((threadpool (cl-threadpool:make-threadpool 5 :name "Example thread pool")))
    (let ((job-1 (cl-threadpool:add-job
		  threadpool
		  (lambda()
		    (sleep 5)
		    "Job 1")))
	  (job-2 (cl-threadpool:add-job
		  threadpool
		  (lambda()
		    (sleep 5)
		    "Job 2"))))
      ;;
      ;; Print status of jobs
      ;;
      (format t "~%job-1 done: ~a" (cl-threadpool:job-done-p job-1)) ;; => "job-1 done: NIL"
      (format t "~%job-2 done: ~a" (cl-threadpool:job-done-p job-2)) ;; => "job-2 done: NIL"
      ;;
      ;; Retrieve job results
      ;;
      (format t "~%~a" (cl-threadpool:job-value job-1)) ;; => "Job 1"
      (format t "~%~a" (cl-threadpool:job-value job-2)) ;; => "Job 2"
      ;;
      ;; Print status of jobs
      ;;
      (format t "~%job-1 done: ~a" (cl-threadpool:job-done-p job-1)) ;; => "job-1 done: T"
      (format t "~%job-2 done: ~a" (cl-threadpool:job-done-p job-2)) ;; => "job-2 done: T"
      (cl-threadpool:stop threadpool))))

;; (example)

