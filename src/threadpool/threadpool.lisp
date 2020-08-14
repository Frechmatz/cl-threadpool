;;
;; A Thread Pool
;;


(in-package :cl-threadpool)

;;
;; Logging
;;

(defparameter *logger*
  (lambda(level who format-control format-arguments)
    (declare (ignore level who format-control format-arguments))
    nil)
  "Logging hook. The default implementation is empty.")

(defun log-info (format-control &rest format-arguments)
  (funcall *logger* :info :cl-threadpool format-control format-arguments))

(defun log-trace (format-control &rest format-arguments)
  (funcall *logger* :trace :cl-threadpool format-control format-arguments))

(defun log-error (format-control &rest format-arguments)
  (funcall *logger* :error :cl-threadpool format-control format-arguments))

;;
;; Errors
;;

(define-condition threadpool-error (error)
  ((text :initarg :text :reader text))
  (:documentation "The default condition that is signalled by thread pools"))

(define-condition threadpool-execution-error (error)
  ((report :initarg :report :reader report))
  (:documentation
   "Represents unhandled errors thrown by a job. Initialization arguments:
    :report An object representing the error. This object is typically not an instance 
    of error."))

(defparameter *job-error-to-report*
  (lambda (err)
    (list :message (format nil "~a" err)))
  "Function to convert an unhandled job error to an object that
   is passed as :report property to the instantiation function of
   threadpool-execution-error. The function is called in the context
   of a worker thread. The final threadpool-execution-error error will 
   typically be thrown in a different thread context.")

(define-condition threadpool-cancellation-error (error)
  ((text :initarg :text :reader text))
  (:documentation
   "This error is signalled when the result of a job is requested but the job has been cancelled."))

;;
;; Future
;;

(defclass future ()
  ((lock :initform (bt:make-lock "future-lock"))
   (cv :initform (bt:make-condition-variable))
   (state :initform :pending)
   (job :initform nil :documentation "Function to be invoked by a worker thread.")
   (value :initform nil))
  (:documentation "Represents the result of a job."))

(defun futurep (obj)
  "Returns t if the given object represents a future."
  (typep obj 'future))

(defun assert-futurep (obj)
  (if (not (futurep obj))
      (error 'threadpool-error :text "Object is not an instance of future")))

(defun future-state-done-p (future-state)
  (or (eq future-state :set) (eq future-state :rejected) (eq future-state :cancelled)))

(defun future-state-cancelled-p (future-state)
  (eq future-state :cancelled))

(defun future-done-p (future)
  "Returns t if the job has completed. A job is completed when it has
   normally terminated, thrown an error or been cancelled."
  (assert-futurep future)
  (bt:with-lock-held ((slot-value future 'lock))
    (future-state-done-p (slot-value future 'state))))

(defun future-cancelled-p (future)
  "Returns t if the job has been cancelled."
  (assert-futurep future)
  (bt:with-lock-held ((slot-value future 'lock))
    (future-state-cancelled-p (slot-value future 'state))))

(defun reset-future (future)
  (setf (slot-value future 'state) :pending)
  (setf (slot-value future 'value) nil)
  (setf (slot-value future 'job) nil)
  nil)

(defun complete-future (future value)
  "Sets the value of a successfully completed future. If the future 
   has already been cancelled the value is ignored and the future remains cancelled.
   If the future is already completed but not cancelled the function signals an error.
   The function has the following arguments:
   <ul>
    <li>future The future.</li>
    <li>value The value.</li>
   </ul>"
  (bt:with-lock-held ((slot-value future 'lock))
    (let ((state (slot-value future 'state)))
      (cond
	((future-state-cancelled-p state)
	 nil)
	((future-state-done-p state)
	 (error "Internal error: Cannot complete an already completed future"))
	(t
	 (setf (slot-value future 'value) value)
	 (setf (slot-value future 'state) :set)
	 (setf (slot-value future 'job) nil)
	 (bt:condition-notify (slot-value future 'cv))))))
  nil)

(defun cancel-future (future)
  "Sets a future to cancelled. Does nothing when the future is already done.
   The function has the following arguments:
   <ul>
    <li>future The future.</li>
   </ul>"
  (bt:with-lock-held ((slot-value future 'lock))
    (let ((state (slot-value future 'state)))
      (if (not (future-state-done-p state))
	  (progn
	   (setf (slot-value future 'value) "Job has been cancelled")
	   (setf (slot-value future 'state) :cancelled)
	   (setf (slot-value future 'job) nil)
	   (bt:condition-notify (slot-value future 'cv)))))
    nil))

(defun reject-future (future report)
  "Sets a future to rejected. This happens when a job signals an unhandled condition.
   Does nothing when the future is cancelled. Signals an error when the future
   has already completed or rejected.
   The function has the following arguments:
   <ul>
    <li>future The future.</li>
    <li>report The error report as generated by *job-error-to-report*.</li> 
   </ul>"
  (bt:with-lock-held ((slot-value future 'lock))
    (let ((state (slot-value future 'state)))
      (cond
	((future-state-cancelled-p state)
	 nil)
	((future-state-done-p state)
	 (error "Internal error: Cannot reject an already done future"))
	(t
	 (setf (slot-value future 'value) report)
	 (setf (slot-value future 'state) :rejected)
	 (setf (slot-value future 'job) nil)
	 (bt:condition-notify (slot-value future 'cv))))))
    nil)

(defun future-value (future)
  "Get the result of the job. If the result is already available it will immediately 
   be returned. Otherwise the function blocks on the completion of the job."
  (assert-futurep future)
  (flet ((release-lock-and-return-value ()
	   (bt:release-lock (slot-value future 'lock))
	   ;; We can safely access state and value without having a lock set,
	   ;; because set-future-value will prevent any changes.
	   (let ((state (slot-value future 'state)) (value (slot-value future 'value)))
	     (cond
	       ((eq state :set)
		value)
	       ((eq state :rejected)
		(error 'threadpool-execution-error :report value))
	       ((eq state :cancelled)
		(error 'threadpool-cancellation-error :text "Job has been cancelled"))
	       (t
		(error (format nil "Dont know how to handle state ~a" state)))))))
    (bt:acquire-lock (slot-value future 'lock))
    (if (future-state-done-p (slot-value future 'state))
	(release-lock-and-return-value)
	(progn
	  (loop
	     ;; Loop until future is done in order to handle spurious wakeups
	     ;; Entry point of loop body assumes that lock is set
	     (bt:condition-wait (slot-value future 'cv)
				(slot-value future 'lock))
	     (if (future-state-done-p (slot-value future 'state))
		 (return)))
	  (release-lock-and-return-value)))))

;;
;; Future Factory
;;

(defclass future-factory ()
  ((lock :initform (bt:make-lock "future-factory-lock"))
   (pool :initform nil)
   (created-future-count
    :initform 0
    :documentation "For debugging/testing purposes."))
  (:documentation
   "Creates future instances and keeps a pool of futures that can be re-used."))

(defgeneric get-future (future-factory)
  (:documentation
   "Get a future. Returns an available future or creates a new one."))

(defgeneric put-future (future-factory future)
  (:documentation
   "Put back a previously requested future into the pool. Putting back
    a future can only happen when the ownership of the future is well
    defined. For example when the threadpool is asked to synchronously run
    a batch of jobs then the underlying futures are not exposed to the application
    and can safely be put back for further re-use."))

(defmethod get-future ((future-factory-instance future-factory))
  (bt:with-lock-held ((slot-value future-factory-instance 'lock))
    (let ((l (pop (slot-value future-factory-instance 'pool))))
      (if (not l)
	  (progn
	    (setf (slot-value future-factory-instance 'created-future-count)
		  (+ 1 (slot-value future-factory-instance 'created-future-count)))
	    (setf l (make-instance 'future))))
      l)))

(defmethod put-future ((future-factory-instance future-factory) future)
  (assert-futurep future)
  (bt:with-lock-held ((slot-value future-factory-instance 'lock))
    (reset-future future)
    (push future (slot-value future-factory-instance 'pool))
    nil))

;;
;; Thread pool
;;

(defclass threadpool ()
  ((job-queue :initform (queues:make-queue :simple-queue)
	      :documentation "Pending jobs. Jobs are represented by instances of future.")
   (future-factory :initform (make-instance 'future-factory))
   (threads :initform nil)
   (size :initarg :size :documentation "Number of worker threads")
   (name :initarg :name :initform (format nil "Threadpool-~a" (gensym)))
   (state :initform :pending
	  :documentation
	  "State of the thread pool. One of :PENDING, :RUNNING, :STOPPING, :STOPPED")
   (lock :initform (bt:make-lock "thread-pool-lock"))
   (cv :initform (bt:make-condition-variable))))

(defun threadpoolp (obj)
  "Returns t if the given object represents a thread pool."
  (typep obj 'threadpool))

(defun assert-threadpoolp (obj)
  (if (not (threadpoolp obj))
      (error 'threadpool-error :text "Object is not an instance of threadpool")))

(defun assert-jobp (job)
  (if (not (functionp job))
      (error 'threadpool-error :text "Job must be a function")))
  
(defun queue-size (pool)
  "Get the current length of the job queue."
  (assert-threadpoolp pool)
  (bt:with-lock-held ((slot-value pool 'lock))
    (queues:qsize (slot-value pool 'job-queue))))

(defun pool-name (pool)
  "Get the name of the pool."
  (assert-threadpoolp pool)
  (bt:with-lock-held ((slot-value pool 'lock))
    (slot-value pool 'name)))

(defun pool-state-stopping-p (pool-state)
    (eq pool-state :stopping))

(defun pool-state-stopped-p (pool-state)
    (eq pool-state :stopped))

(defun pool-stopped-p (pool)
  "Returns t if the pool has successfully been stopped."
  (assert-threadpoolp pool)
  (bt:with-lock-held ((slot-value pool 'lock))
    (pool-state-stopped-p (slot-value pool 'state))))

(defun make-worker-thread (pool thread-id)
  (bt:make-thread
   (lambda ()
     (log-info "Worker thread ~a has started." thread-id)
     (bt:acquire-lock (slot-value pool 'lock) t)
     (loop ;; Entry point of loop body assumes that pool lock is set
	(let ((future (queues:qpop (slot-value pool 'job-queue)))
	      (pool-state (slot-value pool 'state)))
	  (if (pool-state-stopped-p pool-state)
	      (progn
		(bt:release-lock (slot-value pool 'lock))
		(error 'threadpool-error
		       :text (format
			      nil
			      "Worker thread ~a: Pool stopped but worker thread still alive"
			      thread-id))))
	  (if future
	      (let ((future-state nil) (future-job nil))
		(bt:with-lock-held ((slot-value future 'lock))
		  (setf future-state (slot-value future 'state))
		  (setf future-job (slot-value future 'job)))
		(bt:release-lock (slot-value pool 'lock))
		(cond
		  ((future-state-cancelled-p future-state)
		   (log-info "Worker thread ~a: Skipping cancelled job" thread-id))
		  ((future-state-done-p future-state)
		   (error 'threadpool-error
			  :text (format nil "Worker thread ~a: Job already done." thread-id)))
		  ((or (pool-state-stopping-p pool-state))
		   ;; When pool is to be stopped then cancel job
		   (log-info "Worker thread ~a: Pool is stopping. Cancelling job" thread-id)
		   (cancel-future future))
		  (t ;; Execute job
		   (handler-case
		       (complete-future future (funcall future-job))
		     (condition (c)
		       (log-error "Worker thread ~a: Unhandled job error: ~a" thread-id c)
		       (reject-future future (funcall *job-error-to-report* c))))))
		(bt:acquire-lock (slot-value pool 'lock) t))
	      (progn
		(cond
		  ((pool-state-stopping-p pool-state)
		   ;; No Job and pool is stopping => Quit loop and let thread end
		   (bt:release-lock (slot-value pool 'lock))
		   (return))
		  (t ;; No Job -> Wait on Job Queue
		   (bt:condition-wait (slot-value pool 'cv) (slot-value pool 'lock))))))))
     ;; Remove thread from pool
     (bt:with-lock-held ((slot-value pool 'lock))
       (setf (slot-value pool 'threads)
	     (remove-if (lambda (item)
			  (eq (getf item :id) thread-id))
			(slot-value pool 'threads))))
     (log-info "Worker thread ~a has stopped." thread-id))
   :name thread-id))

(defun make-threadpool (size &key (name nil))
  "Create a thread pool.
   name -- Name of the pool.
   size -- Number of worker threads."
  (let ((pool (make-instance 'threadpool :name name :size size)))
    (bt:with-lock-held ((slot-value pool 'lock))
      (log-info "Starting thread pool ~a..." (slot-value pool 'name))
      (dotimes (i (slot-value pool 'size))
	(let ((thread-id (format nil "~a-Thread-~a" (slot-value pool 'name) (gensym))))
	  (push (list
		 :id thread-id
		 :thread (make-worker-thread pool thread-id))
		(slot-value pool 'threads))))
      (setf (slot-value pool 'state) :running)
      (log-info "Thread pool ~a has been started" (slot-value pool 'name))
      pool)))

(defun worker-thread-p (pool)
  "Returns true if the current thread is a worker thread of the given pool."
  (assert-threadpoolp pool)
  (let ((current-thread (bt:current-thread)))
    (bt:with-lock-held ((slot-value pool 'lock))
      (find-if (lambda (cur-thread) (eq current-thread (getf cur-thread :thread)))
	       (slot-value pool 'threads)))))

(defmacro poll ((&key (timeout-seconds nil)) test-body timeout-body)
  "Evaluates repeatedly test-body. If the test-body returns true the loop
   terminates. If the timeout has been reached the timeout-body is executed 
   and the loop terminates.
   timeout-seconds -- the timeout in seconds or nil for no timeout.
   test-body -- The form to be evaluated repeatedly. It is up to the 
      test body to take care of CPU usage. The test-body is evaluated 
      at least once. If test-body returns t then the pool loop quits.
   timeout-body -- The form to be evaluated when a timeout occurs."
  (let ((start-time (gensym)) (first-test-p (gensym)))
    `(progn
       (let ((,start-time (get-internal-real-time)) (,first-test-p t))
	 (loop
	    (if (or
		 ,first-test-p
		 (not ,timeout-seconds)
		 (> ,timeout-seconds
		    (/
		     (- (get-internal-real-time) ,start-time)
		     internal-time-units-per-second)))
		(progn
		  (setf ,first-test-p nil)
		  (if ,test-body
		      (return)))
		(progn
		  ,timeout-body
		  (return))))))))

(defun stop (pool &key (timeout-seconds nil))
  "Stops all worker threads. The function returns when all worker threads are no longer alive 
   or when the timeout has been reached or when the pool is stopping or is already stopped. 
   All pending jobs that are not currently being executed by a worker will be cancelled.
   The function does not destroy threads but signals to the worker threads that they are 
   supposed to end. If a worker thread refuses to end it will be left running.
   See also pool-stopped-p"
  (assert-threadpoolp pool)
  (if (worker-thread-p pool)
      (error 'threadpool-error
	     :text (format
		    nil
		    "Thread pool cannot be stopped by a worker thread: ~a"
		    (slot-value pool 'name))))
  (bt:acquire-lock (slot-value pool 'lock))
  (cond
    ((or (eq :stopped (slot-value pool 'state)) (eq :stopping (slot-value pool 'state)))
     (bt:release-lock (slot-value pool 'lock))
     nil)
    (t
     (setf (slot-value pool 'state) :stopping)
     (log-info "Stopping thread pool ~a..." (slot-value pool 'name))
     (bt:release-lock (slot-value pool 'lock))
     (poll (:timeout-seconds timeout-seconds)
	   (progn
	     (dotimes (i (slot-value pool 'size))
	       (bt:condition-notify (slot-value pool 'cv)))
	     (sleep 1)
	     (bt:with-lock-held ((slot-value pool 'lock))
	       (if (not (eq 0 (length (slot-value pool 'threads))))
		   nil
		   (progn
		     (setf (slot-value pool 'state) :stopped)
		     (log-info "Pool ~a has stopped" (slot-value pool 'name))
		     t))))
	   (progn
	     (log-info
	      "Stopping thread pool ~a: Timeout reached. Giving up."
	      (slot-value pool 'name))))
     nil)))

(defun add-job-impl (pool job)
  (assert-threadpoolp pool)
  (assert-jobp job)
  (bt:with-lock-held ((slot-value pool 'lock))
    (let ((s (slot-value pool 'state)))
      (if (or (eq s :stopping) (eq s :stopped))
	  (error 'threadpool-error
		 :text (format nil "Cannot add job to stopping or stopped thread pool: ~a"
			       (slot-value pool 'name))))
      (let ((future (get-future (slot-value pool 'future-factory))))
	(assert-futurep future)
	(setf (slot-value future 'job) job)
	(queues:qpush (slot-value pool 'job-queue) future)
	(log-trace "Added job to queue of thread pool ~a" (slot-value pool 'name))
	;; Worker threads are waiting on pool cv. Once a thread has been
	;; notified it starts a loop of fetching and executing queued jobs.
	;; If the queue is empty, the thread again waits on pool cv.
	(bt:condition-notify (slot-value pool 'cv))
	future))))

(defun add-job (pool job)
  "Add a job to the pool. 
   pool -- A threadpool instance as created by make-threadpool.   
   job -- A function with zero arguments. A job is supposed to handle all errors.
   * The pool must have been started.
   * The pool must not be in stopping state.
   * The pool must not be in stopped state.
   Returns a future."
  (add-job-impl pool job))

(defun run-jobs (pool jobs)
  "Synchronously run a list of jobs and return their results. Blocks the current thread until 
   all jobs have been completed. Jobs are supposed to handle all errors. In the case
   of unhandled job errors or job cancellations this function still synchronizes on 
   the completion of all jobs and after that re-throws the first error it has catched.
   - pool: The threadpool
   - jobs: A list of jobs. Each job is represented by a function with no arguments.
   Returns an ordered list of the results that the jobs have returned.
   Signals the following errors:
   - threadpool-execution-error When a job has thrown an unhandled error."
  (assert-threadpoolp pool)
  (if (not (listp jobs))
      (error "jobs must be a list"))
  (dolist (job jobs)
    (assert-jobp job))
  (let ((futures (mapcar (lambda(job) (add-job-impl pool job)) jobs))
	(job-error nil)
	(job-results nil))
    (dolist (future futures)
      (handler-case
	  (push (future-value future) job-results)
	(condition (c)
	  (if (not job-error) (setf job-error c))))
      ;; We are owning the futures here and can put them back into the pool.
      (put-future (slot-value pool 'future-factory) future))
    (if job-error (error job-error) (reverse job-results))))

  
