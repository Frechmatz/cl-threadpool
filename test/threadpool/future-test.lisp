(in-package :cl-threadpool-test)

(init-logger)

(defun future-test-catch-get-value-error (future)
  (let ((catched-error nil))
    (handler-case
	(cl-threadpool:job-value future)
      (error (err)
	(setf catched-error err)))
    catched-error))

(defun future-test-catch-invocation-error (f)
  (let ((catched-error nil))
    (handler-case
	(funcall f)
      (error (err)
	(setf catched-error err)))
    catched-error))

;;
;; Completed
;;
;; Call sequences
;; - completed 
;; - completed -> completed
;; - completed -> cancelled
;; - completed -> rejected

(define-test future-completed ()
  "completed"
  (let ((future (make-instance 'cl-threadpool::future)))
    (cl-threadpool::complete-job future "RESULT")
    (assert-equal "RESULT" (cl-threadpool:job-value future))))

(define-test future-completed-completed ()
  "completed-completed"
  (let ((future (make-instance 'cl-threadpool::future)))
    (cl-threadpool::complete-job future "RESULT-1")
    (assert-true
     (future-test-catch-invocation-error
      (lambda() (cl-threadpool::complete-job future "RESULT-2"))))
    (assert-equal "RESULT-1" (cl-threadpool:job-value future))))

(define-test future-completed-cancelled ()
  "completed-cancelled"
  (let ((future (make-instance 'cl-threadpool::future)))
    (cl-threadpool::complete-job future "RESULT")
    (assert-true
     (not (future-test-catch-invocation-error
	   (lambda() (cl-threadpool:cancel-job future)))))
    (assert-equal "RESULT" (cl-threadpool:job-value future))))

(define-test future-completed-rejected ()
  "completed-rejected"
  (let ((future (make-instance 'cl-threadpool::future)))
    (cl-threadpool::complete-job future "RESULT")
    (assert-true
     (future-test-catch-invocation-error
      (lambda() (cl-threadpool::reject-job future "REPORT"))))
    (assert-equal "RESULT" (cl-threadpool:job-value future))))

;;
;; Rejected
;;
;; Call sequences
;; - rejected
;; - rejected -> rejected
;; - rejected -> completed
;; - rejected -> cancelled

(define-test future-rejected ()
  "rejected"
  (let ((future (make-instance 'cl-threadpool::future)))
    (cl-threadpool::reject-job future "REPORT")
    (assert-true (typep
		  (future-test-catch-get-value-error future)
		  'cl-threadpool:threadpool-execution-error))))

(define-test future-rejected-rejected ()
  "rejected-rejected"
  (let ((future (make-instance 'cl-threadpool::future)))
    (cl-threadpool::reject-job future "REPORT-1")
    (assert-true
     (future-test-catch-invocation-error
      (lambda() (cl-threadpool::reject-job future "REPORT-2"))))
    (assert-true (typep
		  (future-test-catch-get-value-error future)
		  'cl-threadpool:threadpool-execution-error))))

(define-test future-rejected-completed ()
  "rejected-completed"
  (let ((future (make-instance 'cl-threadpool::future)))
    (cl-threadpool::reject-job future "REPORT")
    (assert-true
     (future-test-catch-invocation-error
      (lambda() (cl-threadpool::complete-job future "RESULT"))))
    (assert-true (typep
		  (future-test-catch-get-value-error future)
		  'cl-threadpool:threadpool-execution-error))))

(define-test future-rejected-cancelled ()
  "rejected-cancelled"
  (let ((future (make-instance 'cl-threadpool::future)))
    (cl-threadpool::reject-job future "REJECTED")
    (assert-true
     (not (future-test-catch-invocation-error
	   (lambda() (cl-threadpool:cancel-job future)))))
    (assert-true (typep
		  (future-test-catch-get-value-error future)
		  'cl-threadpool:threadpool-execution-error))))

;;
;; Cancelled
;;
;; Call sequences
;; - cancelled
;; - cancelled -> cancelled
;; - cancelled -> completed
;; - cancelled -> rejected

(define-test future-cancelled ()
  "cancelled"
  (let ((future (make-instance 'cl-threadpool::future)))
    (cl-threadpool:cancel-job future)
    (assert-true (typep
		  (future-test-catch-get-value-error future)
		  'cl-threadpool:threadpool-cancellation-error))))

(define-test future-cancelled-cancelled ()
  "cancelled-cancelled"
  (let ((future (make-instance 'cl-threadpool::future)))
    (cl-threadpool:cancel-job future)
    (assert-true
     (not (future-test-catch-invocation-error
	   (lambda() (cl-threadpool:cancel-job future)))))
    (assert-true
     (typep
      (future-test-catch-get-value-error future)
      'cl-threadpool:threadpool-cancellation-error))))

(define-test future-cancelled-completed ()
  "cancelled-completed"
  (let ((future (make-instance 'cl-threadpool::future)))
    (cl-threadpool:cancel-job future)
    (assert-true
     (not (future-test-catch-invocation-error
	   (lambda() (cl-threadpool::complete-job future "RESULT")))))
    (assert-true
     (typep
      (future-test-catch-get-value-error future)
      'cl-threadpool:threadpool-cancellation-error))))

(define-test future-cancelled-rejected ()
  "cancelled-rejected"
  (let ((future (make-instance 'cl-threadpool::future)))
    (cl-threadpool:cancel-job future)
    (assert-true
     (not (future-test-catch-invocation-error
	   (lambda() (cl-threadpool::reject-job future "REPORT")))))
    (assert-true
     (typep
      (future-test-catch-get-value-error future)
      'cl-threadpool:threadpool-cancellation-error))))

