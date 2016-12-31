(in-package :cl-threadpool-test)

(defun create-worker (name fn)
  (lambda ()
    ;;(v:info :cl-threadpool (format nil "********** ~a: Doing something ***********" name))
    (funcall fn name)
    (sleep 3)
    ;;(v:info :cl-threadpool (format nil "********** ~a: Doing something ***********" name))
    (funcall fn name)
    ))
  
(define-test run-workers ()
  "Run some workers"
  (let ((result-lock (bt:make-lock)) (result nil))
    (flet ((add-result (r)
	     (bt:with-lock-held (result-lock) (push r result))))
      (let ((pool (cl-threadpool:make-threadpool "testpool" 5)))
	(cl-threadpool:start pool)
	(cl-threadpool:add-job pool (create-worker "Job 1" #'add-result))
	(cl-threadpool:add-job pool (create-worker "Job 2" #'add-result))
	(cl-threadpool:add-job pool (create-worker "Job 3" #'add-result))
	(cl-threadpool:stop pool)
	(sleep 1) ;;(v:sync)
	(assert-true (= 6 (length result)))))))


(define-test run-workers-one-thread ()
  "Test processing of jobs with one worker thread"
  (let ((pool (cl-threadpool:make-threadpool "testpool" 1)) (result nil))
    (cl-threadpool:start pool)
    (cl-threadpool:add-job
     pool
     (lambda ()
       (sleep 1) ()))
    (cl-threadpool:add-job
     pool
     (lambda ()
       (sleep 5)
       (setf result t)))
    (cl-threadpool:stop pool)
    (sleep 2)
    (assert-true result)))
