(in-package :cl-threadpool-test)



(define-test lost-wakeup-1 ()
  "Test that worker-threads are polling the job queue"
  (let ((pool (cl-threadpool:make-threadpool 2 :name "lost-wakeup-1"))
	(results (make-result-list)))
    (cl-threadpool:start pool)
    (cl-threadpool:add-job
     pool
     (lambda ()
       (v:info :cl-threadpool (format nil "Job 1 is running"))
       (sleep 10)
       (funcall (getf results :add) "Job 1")))
    (cl-threadpool:add-job
     pool
     (lambda ()
       (v:info :cl-threadpool (format nil "Job 2 is running"))
       (sleep 10)
       (funcall (getf results :add) "Job 2")))
    (sleep 2)
    ;; Assume, that both worker threads are busy.
    ;; Now add a new job to the pool.
    ;; The pool will notify the cv but no worker threads are blocking on it.
    ;; However, currently running threads are executing the fetch/process loop,
    ;; so the new job will not get lost but will be executed.
    (cl-threadpool:add-job
     pool
     (lambda ()
       (v:info :cl-threadpool (format nil "Job 3 is running"))
       (sleep 5)
       (funcall (getf results :add) "Job 3")))
    (sleep 20)
    ;; get results, before pool is being stopped
    ;; (because the pool stop function calls notify on pool cv)
    (let ((result (funcall (getf results :get))))
      (cl-threadpool:stop pool)
      (assert-equal 3 (length result)))))
