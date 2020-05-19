(in-package :cl-threadpool-test)

(init-logger)

(define-test pool-stop-processed-all-1-test ()
  "Test that after stopping of the pool all jobs are executed"
  (let ((result-lock (bt:make-lock)) (result nil))
    (flet ((add-result (r)
	     (bt:with-lock-held (result-lock) (push r result))))
      (flet ((create-worker ()
	       (lambda ()
		 (sleep 1)
		 (add-result "RESULT"))))
	;; one worker thread
	(let ((pool (cl-threadpool:make-threadpool 1)))
	  (cl-threadpool:start pool)
	  (cl-threadpool:add-job pool (create-worker))
	  (cl-threadpool:add-job pool (create-worker))
	  (cl-threadpool:add-job pool (create-worker))
	  (cl-threadpool:stop pool)
	  (assert-true (= 3 (length result))))))))

