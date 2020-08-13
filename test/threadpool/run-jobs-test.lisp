(in-package :cl-threadpool-test)

(init-logger)


(define-test run-jobs-simple ()
  "Test synchronous execution of jobs"
  (let ((pool (cl-threadpool:make-threadpool 2 :name "run-jobs-simple")))
    (let ((results
	   (cl-threadpool:run-jobs
	    pool
	    (list
	     (lambda() (sleep 5) "Job 1")
	     (lambda() (sleep 2) "Job 2")
	     (lambda() (sleep 1) "Job 3")))))
      (cl-threadpool:stop pool)
      (assert-true (cl-threadpool:pool-stopped-p pool))
      (assert-equal 3 (length results))
      (assert-equal "Job 1" (first results))
      (assert-equal "Job 2" (second results))
      (assert-equal "Job 3" (third results)))))

(define-test run-jobs-future-pool-management ()
  "Test management of re-usable futures"
  (let ((pool (cl-threadpool:make-threadpool 2 :name "run-jobs-future-pool-management")))
    (let ((results
	   (cl-threadpool:run-jobs
	    pool
	    (list
	     (lambda() (sleep 5) "Job 1")
	     (lambda() (sleep 2) "Job 2")
	     (lambda() (sleep 1) "Job 3")))))

      (assert-equal 3 (length results))
      (assert-equal "Job 1" (first results))
      (assert-equal "Job 2" (second results))
      (assert-equal "Job 3" (third results))
      (let ((future-factory (slot-value pool 'cl-threadpool::future-factory)))
	(assert-equal 3 (length (slot-value future-factory 'cl-threadpool::pool)))
	(assert-equal 3 (slot-value future-factory 'cl-threadpool::created-future-count)))

      (setf results
	    (cl-threadpool:run-jobs
	     pool
	     (list
	      (lambda() (sleep 5) "Job 11")
	      (lambda() "Job 22")
	      (lambda() (sleep 1) "Job 33"))))

      (cl-threadpool:stop pool)
      (assert-true (cl-threadpool:pool-stopped-p pool))

      (assert-equal 3 (length results))
      (assert-equal "Job 11" (first results))
      (assert-equal "Job 22" (second results))
      (assert-equal "Job 33" (third results))
      (let ((future-factory (slot-value pool 'cl-threadpool::future-factory)))
	(assert-equal 3 (length (slot-value future-factory 'cl-threadpool::pool)))
	(assert-equal 3 (slot-value future-factory 'cl-threadpool::created-future-count)))
      )))

(define-test run-jobs-many-threads ()
  "Test synchronous execution of jobs"
  (let ((pool (cl-threadpool:make-threadpool 10 :name "run-jobs-many-threads")))
    (let ((results
	   (cl-threadpool:run-jobs
	    pool
	    (list
	     (lambda() (sleep 5) "Job 1")
	     (lambda() (sleep 2) "Job 2")
	     (lambda() (sleep 2) "Job 3")))))
      (cl-threadpool:stop pool)
      (assert-true (cl-threadpool:pool-stopped-p pool))
      (assert-equal 3 (length results))
      (assert-equal "Job 1" (first results))
      (assert-equal "Job 2" (second results))
      (assert-equal "Job 3" (third results)))))

(define-test run-jobs-few-threads ()
  "Test synchronous execution of jobs"
  (let ((pool (cl-threadpool:make-threadpool 1 :name "run-jobs-few-threads")))
    (let ((results
	   (cl-threadpool:run-jobs
	    pool
	    (list
	     (lambda() (sleep 5) "Job 1")
	     (lambda() (sleep 2) "Job 2")
	     (lambda() (sleep 2) "Job 3")))))
      (cl-threadpool:stop pool)
      (assert-true (cl-threadpool:pool-stopped-p pool))
      (assert-equal 3 (length results))
      (assert-equal "Job 1" (first results))
      (assert-equal "Job 2" (second results))
      (assert-equal "Job 3" (third results)))))

(define-test run-jobs-pressure-1 ()
  "Test synchronous execution of jobs with high pressure on the queue"
  (let ((pool (cl-threadpool:make-threadpool 5 :name "run-jobs-pressure-1")) (job-count 1000))
    (let ((jobs nil) (expected-job-results (make-array job-count)))
      (dotimes (i job-count)
	(let ((job-result (format nil "job-result-~a" i)))
	  (push (lambda() job-result) jobs)
	  (setf (elt expected-job-results i) job-result)))
      
      (let ((results (list-to-array (cl-threadpool:run-jobs pool (reverse jobs)))))
	(cl-threadpool:stop pool)
	(assert-true (cl-threadpool:pool-stopped-p pool))
	(dotimes (i job-count)
	  (assert-equal (elt expected-job-results i) (elt results i)))))))

(define-test run-jobs-pressure-2 ()
  "Test synchronous execution of jobs with high pressure on the queue"
  (let ((pool (cl-threadpool:make-threadpool 2 :name "run-jobs-pressure-2")) (job-count 10))
    (let ((jobs nil) (expected-job-results (make-array job-count)))
      (dotimes (i job-count)
	(let ((job-result (format nil "job-result-~a" i)))
	  ;; sleep, then returm result
	  (push (lambda() (sleep 1) job-result) jobs)
	  (setf (elt expected-job-results i) job-result)))
      
      (let ((results (list-to-array (cl-threadpool:run-jobs pool (reverse jobs)))))
	(cl-threadpool:stop pool)
	(assert-true (cl-threadpool:pool-stopped-p pool))
	(dotimes (i job-count)
	  (assert-equal (elt expected-job-results i) (elt results i)))))))

(define-test run-jobs-execution-error ()
  "Run a batch of jobs and test that job execution errors are properly handled."
  (let ((pool (cl-threadpool:make-threadpool 2 :name "error-handling-run-jobs")))
    (let ((catched-error nil))
      (handler-case
	  (cl-threadpool:run-jobs
	   pool
	   (list
	    (lambda() (sleep 1) "Job 1")
	    (lambda() (sleep 2) (error "Job 2 Error"))
	    (lambda() (sleep 3) "Job 3")))
	(error (err)
	  (progn
	    (setf catched-error err))))
      (let ((l (cl-threadpool:queue-size pool)))
	(cl-threadpool:stop pool)
	(assert-true (cl-threadpool:pool-stopped-p pool))
	(assert-true catched-error)
	(assert-true (typep
		      catched-error
		      'cl-threadpool:threadpool-execution-error))
	(assert-equal 0 l)))))

(define-test run-jobs-stop-pool-1 ()
  "Stop a pool which is running a batch of jobs. Assumes probably too much about 
   the underlying thread implementation"
  (let ((pool-to-stop (cl-threadpool:make-threadpool 1 :name "run-jobs-stop-pool-1"))
	(got-cancellation-error nil)
	(lock (bt:make-lock "run-jobs-stop-pool-1-lock")))
    (bt:make-thread (lambda()
		      (bt:acquire-lock lock) 
		      (handler-case
			  (cl-threadpool:run-jobs
			   pool-to-stop
			   (list
			    (lambda() (sleep 10) "Job 1")
			    (lambda() (sleep 10) "Job 2")
			    (lambda() (sleep 10) "Job 3")))
			(error (err)
			  (progn
			    (cl-threadpool::log-info "Got error: ~a" err)
			    (if (typep err 'cl-threadpool:threadpool-cancellation-error)
				(setf got-cancellation-error t)))))
		      (bt:release-lock lock)))
    (sleep 3) ;; Assume that tread is running after x seconds
    (cl-threadpool:stop pool-to-stop :timeout-seconds 20)
      (bt:acquire-lock lock) 
      (bt:release-lock lock) 
      (assert-true (cl-threadpool:pool-stopped-p pool-to-stop))
      (assert-true got-cancellation-error)))

