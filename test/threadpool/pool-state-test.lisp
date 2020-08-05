(in-package :cl-threadpool-test)

(init-logger)

(define-test pool-state-threadpoolp-1 ()
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-state-threadpoolp-1")))
    (assert-true (cl-threadpool:threadpoolp pool))))

(define-test pool-state-threadpoolp-2 ()
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-state-threadpoolp-2")))
    (declare (ignore pool))
    (assert-false (cl-threadpool:threadpoolp "foo"))))

(define-test pool-state-not-started ()
  "Run jobs against pool that hasn't been started"
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-state-not-started")))
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:run-jobs pool (list (lambda ())))
	(error (err) (setf got-error err)))
      (assert-true got-error)
      (assert-true (typep got-error 'cl-threadpool:threadpool-error)))))

(define-test pool-state-stopped ()
  "Run jobs against pool that has stopped"
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-state-stopped")))
    (cl-threadpool:start pool)
    (cl-threadpool:run-jobs
     pool
     (list (lambda () ())))
    (cl-threadpool:stop pool)
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:run-jobs pool (list (lambda ())))
	(error (err) (setf got-error err)))
      (assert-true got-error)
      (assert-true (typep got-error 'cl-threadpool:threadpool-error)))))

(define-test pool-state-start-twice ()
  "Start pool twice"
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-state-start-twice")))
    (cl-threadpool:start pool)
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:start pool)
	(error (err) (setf got-error err)))
      (assert-true got-error)
      (assert-true (typep got-error 'cl-threadpool:threadpool-error)))))

(define-test pool-state-stop-instantiated-1 ()
  "Stop pool after it has been instantiated but not started"
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-state-stop-instantiated-1")))
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:stop pool)
	(error (err) (setf got-error err)))
      (assert-false got-error))))


(define-test pool-state-stop-instantiated-2 ()
  "Stop pool two times after it has been instantiated but not started (this is allowed)"
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-state-stop-instantiated-2")))
    (cl-threadpool:stop pool)
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:stop pool)
	(error (err) (setf got-error err)))
      (assert-false got-error))))

(define-test pool-state-stop-twice ()
  "Nested pool stopping (this is allowed)."
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-state-stop-twice")))
    (cl-threadpool:start pool)
    (cl-threadpool:stop pool)
    (let ((got-error nil))
      (handler-case 
	  (cl-threadpool:stop pool)
	(error (err) (setf got-error err)))
      (assert-false got-error))))

(define-test pool-state-stop-pool-by-worker ()
  "Test that pool cannot be stopped by a worker thread"
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-state-stop-pool-by-worker")))
    (cl-threadpool:start pool)
    (let ((got-error nil))
      (cl-threadpool:run-jobs
       pool
       (list (lambda ()
	 (handler-case 
	     (cl-threadpool:stop pool)
	   (error (err) (setf got-error err))))))
      (cl-threadpool:stop pool)
      (assert-true got-error)
      (assert-true (typep got-error 'cl-threadpool:threadpool-error)))))

(define-test pool-state-worker-thread-p-1 ()
  (let ((pool (cl-threadpool:make-threadpool 5 :name "pool-state-worker-thread-p-1")))
    (cl-threadpool:start pool)
    (let ((is-worker-thread nil))
      (cl-threadpool:run-jobs
       pool
       (list (lambda ()
	 (setf is-worker-thread (cl-threadpool:worker-thread-p pool)))))
      (cl-threadpool:stop pool)
      (assert-true is-worker-thread))))

(define-test pool-state-worker-thread-p-2 ()
  (let ((pool-1 (cl-threadpool:make-threadpool 5 :name "pool-state-worker-thread-p-2-1"))
	(pool-2 (cl-threadpool:make-threadpool 5 :name "pool-state-worker-thread-p-2-2")))
    (cl-threadpool:start pool-1)
    (cl-threadpool:start pool-2)
    (let ((is-worker-thread t))
      (cl-threadpool:run-jobs
       pool-1
       (list (lambda ()
	 (setf is-worker-thread (cl-threadpool:worker-thread-p pool-2)))))
      (cl-threadpool:stop pool-1)
      (cl-threadpool:stop pool-2)
      (assert-false is-worker-thread))))

(define-test pool-state-queue-size ()
  (let ((pool (cl-threadpool:make-threadpool 1 :name "pool-state-queue-size")))
    (cl-threadpool:start pool)
    (cl-threadpool:run-jobs
     pool
     (list
      (lambda() "Job 1")
      (lambda() "Job 2")
      (lambda() "Job 3")))
    (let ((l (cl-threadpool:queue-size pool)))
      (cl-threadpool:stop pool)
      (assert-equal 0 l))))

