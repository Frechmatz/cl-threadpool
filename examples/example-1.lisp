(defpackage :cl-threadpool-example-1 (:use :cl))
(in-package :cl-threadpool-example-1)

(defun example()
  (let ((threadpool (cl-threadpool:make-threadpool 5 :name "Example thread pool")))
    ;;
    ;; Run a batch of jobs. Blocks the current thread until all jobs have finished.
    ;;
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
    ;;
    ;; Add some jobs
    ;;
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
      (format t "~%job-1 done? => ~a" (cl-threadpool:job-done-p job-1)) ;; "job-1 done? => NIL"
      (format t "~%job-2 done? => ~a" (cl-threadpool:job-done-p job-2)) ;; "job-2 done? => NIL"
      ;;
      ;; Wait for completion of jobs
      ;;
      (format t "~%~a" (cl-threadpool:job-value job-1)) ;; => "Job 1"
      (format t "~%~a" (cl-threadpool:job-value job-2)) ;; => "Job 2"
      ;;
      ;; Print status of jobs
      ;;
      (format t "~%job-1 done? => ~a" (cl-threadpool:job-done-p job-1)) ;; "job-1 done? => T"
      (format t "~%job-2 done? => ~a" (cl-threadpool:job-done-p job-2)) ;; "job-2 done? => T"
      ;;
      ;; Stop the thread pool.
      ;;
      (cl-threadpool:stop threadpool))))

;; (example)

