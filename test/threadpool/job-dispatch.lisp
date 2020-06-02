(in-package :cl-threadpool-test)

(init-logger)

(define-test job-dispatch-run-jobs-sync-1 ()
  "Test that jobs are dispatched to all available worker threads"
  (let ((pool (cl-threadpool:make-threadpool 3 :name "job-dispatch-run-jobs-sync-1" )))
    (cl-threadpool:start pool)
    (let ((results
	   (cl-threadpool:run-jobs
	    pool
	    (list
	     (lambda() (sleep 5) (bt:thread-name (bt:current-thread)))
	     (lambda() (sleep 5) (bt:thread-name (bt:current-thread)))
	     (lambda() (sleep 5) (bt:thread-name (bt:current-thread)))))))
      (cl-threadpool:stop pool)
      (assert-equal 3 (length results))
      ;; Assert that different worker threads have been executed the jobs
      (assert-false (equal (first results) (second results)))
      (assert-false (equal (first results) (third results)))
      (assert-false (equal (second results) (third results))))))


