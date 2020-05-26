(in-package :cl-threadpool-test)

(init-logger)

(define-test run-jobs-sync-simple ()
  "Test synchronous execution of jobs"
  (let ((pool (cl-threadpool:make-threadpool 2 :name "run-jobs-sync-simple")))
    (cl-threadpool:start pool)
    (let ((results
	   (cl-threadpool:run-jobs
	    pool
	    (list
	     (lambda() (sleep 5) "Job 1")
	     (lambda() (sleep 2) "Job 2")
	     (lambda() (sleep 1) "Job 3")))))
      (cl-threadpool:stop pool)
      (assert-equal 3 (length results))
      (assert-equal "Job 1" (first results))
      (assert-equal "Job 2" (second results))
      (assert-equal "Job 3" (third results)))))

(define-test run-jobs-sync-pool-management ()
  "Test management of job-locks"
  (let ((pool (cl-threadpool:make-threadpool 2 :name "run-jobs-sync-pool-management")))
    (cl-threadpool:start pool)
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
      (assert-equal 3 (funcall (getf (slot-value pool 'cl-threadpool::job-lock-pool) :length)))
      (assert-equal 3 (funcall (getf (slot-value pool 'cl-threadpool::job-lock-pool) :created-lock-count)))

      (setf results
	    (cl-threadpool:run-jobs
	     pool
	     (list
	      (lambda() (sleep 5) "Job 11")
	      (lambda() "Job 22")
	      (lambda() (sleep 1) "Job 33"))))

      (cl-threadpool:stop pool)

      (assert-equal 3 (length results))
      (assert-equal "Job 11" (first results))
      (assert-equal "Job 22" (second results))
      (assert-equal "Job 33" (third results))
      (assert-equal 3 (funcall (getf (slot-value pool 'cl-threadpool::job-lock-pool) :length)))
      (assert-equal 3 (funcall (getf (slot-value pool 'cl-threadpool::job-lock-pool) :created-lock-count))))))

(define-test run-jobs-sync-many-threads ()
  "Test synchronous execution of jobs"
  (let ((pool (cl-threadpool:make-threadpool 10 :name "run-jobs-sync-many-threads")))
    (cl-threadpool:start pool)
    (let ((results
	   (cl-threadpool:run-jobs
	    pool
	    (list
	     (lambda() (sleep 5) "Job 1")
	     (lambda() (sleep 2) "Job 2")
	     (lambda() (sleep 2) "Job 3")))))
      (cl-threadpool:stop pool)
      (assert-equal 3 (length results))
      (assert-equal "Job 1" (first results))
      (assert-equal "Job 2" (second results))
      (assert-equal "Job 3" (third results)))))

(define-test run-jobs-sync-few-threads ()
  "Test synchronous execution of jobs"
  (let ((pool (cl-threadpool:make-threadpool 1 :name "run-jobs-sync-few-threads")))
    (cl-threadpool:start pool)
    (let ((results
	   (cl-threadpool:run-jobs
	    pool
	    (list
	     (lambda() (sleep 5) "Job 1")
	     (lambda() (sleep 2) "Job 2")
	     (lambda() (sleep 2) "Job 3")))))
      (cl-threadpool:stop pool)
      (assert-equal 3 (length results))
      (assert-equal "Job 1" (first results))
      (assert-equal "Job 2" (second results))
      (assert-equal "Job 3" (third results)))))

(define-test run-jobs-sync-pressure-1 ()
  "Test synchronous execution of jobs with high pressure on the queue"
  (let ((pool (cl-threadpool:make-threadpool 5 :name "run-jobs-sync-pressure-1")) (job-count 1000))
    (cl-threadpool:start pool)
    (let ((jobs nil) (expected-job-results (make-array job-count)))
      (dotimes (i job-count)
	(let ((job-result (format nil "job-result-~a" i)))
	  (push (lambda() job-result) jobs)
	  (setf (elt expected-job-results i) job-result)))
      
      (let ((results (list-to-array (cl-threadpool:run-jobs pool (reverse jobs)))))
	(cl-threadpool:stop pool)
	(dotimes (i job-count)
	  (assert-equal (elt expected-job-results i) (elt results i)))))))

(define-test run-jobs-sync-pressure-2 ()
  "Test synchronous execution of jobs with high pressure on the queue"
  (let ((pool (cl-threadpool:make-threadpool 2 :name "run-jobs-sync-pressure-2")) (job-count 10))
    (cl-threadpool:start pool)
    (let ((jobs nil) (expected-job-results (make-array job-count)))
      (dotimes (i job-count)
	(let ((job-result (format nil "job-result-~a" i)))
	  ;; sleep, then returm result
	  (push (lambda() (sleep 1) job-result) jobs)
	  (setf (elt expected-job-results i) job-result)))
      
      (let ((results (list-to-array (cl-threadpool:run-jobs pool (reverse jobs)))))
	(cl-threadpool:stop pool)
	(dotimes (i job-count)
	  (assert-equal (elt expected-job-results i) (elt results i)))))))

