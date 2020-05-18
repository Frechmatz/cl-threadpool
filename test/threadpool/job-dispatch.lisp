(in-package :cl-threadpool-test)

(init-logger)

(define-test job-dispatch-run-jobs-sync-1 ()
  "Test that jobs are dispatched to all available worker threads"
  (let ((pool (cl-threadpool:make-threadpool 3)))
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


(define-test job-dispatch-run-jobs-async-1 ()
  "Test that jobs are dispatched to all available worker threads"
  (let ((pool (cl-threadpool:make-threadpool 3))
	(results (make-result-list)))
    (cl-threadpool:start pool)
    (cl-threadpool:add-job
     pool
     (lambda ()
       (cl-threadpool::write-log
	:info
	:cl-threadpool
	"Job 1 is running")
       (sleep 10)
       (funcall (getf results :add) (bt:thread-name (bt:current-thread)))))
    (cl-threadpool:add-job
     pool
     (lambda ()
       (cl-threadpool::write-log
	:info
	:cl-threadpool
	"Job 2 is running")
       (sleep 10)
       (funcall (getf results :add) (bt:thread-name (bt:current-thread)))))
    (cl-threadpool:add-job
     pool
     (lambda ()
       (cl-threadpool::write-log
	:info
	:cl-threadpool
	"Job 3 is running")
       (sleep 10)
       (funcall (getf results :add) (bt:thread-name (bt:current-thread)))))
    (cl-threadpool:stop pool)
    (let ((results (funcall (getf results :get))))
      (assert-equal 3 (length results))
      ;; Assert that different worker threads have been executed the jobs
      (assert-false (equal (first results) (second results)))
      (assert-false (equal (first results) (third results)))
      (assert-false (equal (second results) (third results))))))
