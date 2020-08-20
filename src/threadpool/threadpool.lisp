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

(define-condition job-execution-error (error)
  ((pool-name :initarg :pool-name :reader job-execution-error-pool-name)
   (thread-id :initarg :thread-id :reader job-execution-error-thread-id)
   (message :initarg :message :reader job-execution-error-message))
  (:documentation
   "This condition is signalled when the result of a job is requested but the job has
    signalled a condition during its execution."))

(define-condition job-cancellation-error (error)
  ()
  (:documentation
   "This condition is signalled when the result of a job is requested but the 
    job has been cancelled."))

;;
;; Future
;;

(defclass future ()
  ((lock :initform (bt:make-lock "future-lock"))
   (cv :initform (bt:make-condition-variable))
   (state :initform :pending)
   (job :initform nil :documentation "Function to be invoked by a worker thread.")
   (value :initform nil))
  (:documentation "A Future represents the result of a job. Futures provide functions
   to get the result of a job, to check if the job has been completed or cancelled 
   and to cancel a job."))

(defun futurep (obj)
  "Returns t if the given object represents a future."
  (typep obj 'future))

(defun assert-futurep (obj)
  (if (not (futurep obj))
      (error "Object is not an instance of future")))

(defun future-state-done-p (future-state)
  (or (eq future-state :set) (eq future-state :rejected) (eq future-state :cancelled)))

(defun future-state-cancelled-p (future-state)
  (eq future-state :cancelled))

(defun job-done-p (future)
  "Returns t if the job is done. A job is done when it has
   succesfully completed, signalled a condition or has been cancelled."
  (assert-futurep future)
  (bt:with-lock-held ((slot-value future 'lock))
    (future-state-done-p (slot-value future 'state))))

(defun job-cancelled-p (future)
  "Returns t if the job has been cancelled."
  (assert-futurep future)
  (bt:with-lock-held ((slot-value future 'lock))
    (future-state-cancelled-p (slot-value future 'state))))

(defun complete-job (future value)
  "Sets the value of a successfully completed job. If the future 
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

(defun cancel-job (future)
  "Cancels a job. The function does nothing when the job is already done."
  (bt:with-lock-held ((slot-value future 'lock))
    (let ((state (slot-value future 'state)))
      (if (not (future-state-done-p state))
	  (progn
	   (setf (slot-value future 'value) "Job has been cancelled")
	   (setf (slot-value future 'state) :cancelled)
	   (setf (slot-value future 'job) nil)
	   (bt:condition-notify (slot-value future 'cv)))))
    nil))

(defun reject-job (future pool-name thread-id message)
  "Sets a job to rejected. This happens when a job signals an unhandled condition.
   Does nothing when the future is cancelled. Signals an error when the future
   has already completed or rejected.
   The function has the following arguments:
   <ul>
    <li>future The future.</li>
    <li>pool-name Name of the pool.</li> 
    <li>thread-id Id of the worker thread.</li> 
    <li>message A message.</li> 
   </ul>"
  (bt:with-lock-held ((slot-value future 'lock))
    (let ((state (slot-value future 'state)))
      (cond
	((future-state-cancelled-p state)
	 nil)
	((future-state-done-p state)
	 (error "Internal error: Cannot reject an already done future"))
	(t
	 (setf (slot-value future 'value)
	       (list :pool-name pool-name :thread-id thread-id :message message))
	 (setf (slot-value future 'state) :rejected)
	 (setf (slot-value future 'job) nil)
	 (bt:condition-notify (slot-value future 'cv))))))
    nil)

(defun job-value (future)
  "Get the result of a job. If the result is already available it will immediately 
   be returned. Otherwise the function blocks on the completion of the job.
   The function may signal one of the following conditions:
   <ul>
   <li>job-execution-error The job has signalled a condition.</li>
   <li>job-cancellation-error The job has been cancelled.</li>
   </ul>"
  (assert-futurep future)
  (flet ((release-lock-and-return-value ()
	   (bt:release-lock (slot-value future 'lock))
	   ;; We can safely access state and value without having a lock set,
	   ;; because complete-job will prevent any changes.
	   (let ((state (slot-value future 'state)) (value (slot-value future 'value)))
	     (cond
	       ((eq state :set)
		value)
	       ((eq state :rejected)
		(error 'job-execution-error
		       :pool-name (getf value :pool-name)
		       :thread-id (getf value :thread-id)
		       :message (getf value :message)))
	       ((eq state :cancelled)
		(error 'job-cancellation-error))
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
    (setf (slot-value future 'state) :pending)
    (setf (slot-value future 'value) nil)
    (setf (slot-value future 'job) nil)
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
      (error "Object is not an instance of threadpool")))

(defun assert-jobp (job)
  (if (not (functionp job))
      (error "Job must be a function")))

(defun queue-size-no-lock (pool)
  (queues:qsize (slot-value pool 'job-queue)))

(defun queue-size (pool)
  "Returns the current length of the job queue."
  (assert-threadpoolp pool)
  (bt:with-lock-held ((slot-value pool 'lock))
    (queue-size-no-lock pool)))

(defun pool-name (pool)
  "Returns the name of the pool."
  (assert-threadpoolp pool)
  (bt:with-lock-held ((slot-value pool 'lock))
    (slot-value pool 'name)))

(defun pool-state-stopping-p (pool-state)
    (eq pool-state :stopping))

(defun pool-state-stopped-p (pool-state)
    (eq pool-state :stopped))

(defun pool-stopped-p (pool)
  "Returns t if the job queue of the pool is empty and all worker threads have ended."
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
	      (pool-state (slot-value pool 'state))
	      (pool-name (slot-value pool 'name)))
	  (if (pool-state-stopped-p pool-state)
	      (progn
		(bt:release-lock (slot-value pool 'lock))
		(error (format
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
		   (log-info "Worker thread ~a: Skipping cancelled job." thread-id))
		  ((future-state-done-p future-state)
		   (error (format nil "Worker thread ~a: Job already done." thread-id)))
		  ((or (pool-state-stopping-p pool-state))
		   ;; When pool is to be stopped then cancel job
		   (log-info "Worker thread ~a: Pool is stopping. Cancelling job." thread-id)
		   (cancel-job future))
		  (t ;; Execute job
		   (handler-case
		       (complete-job future (funcall future-job))
		     (condition (c)
		       (log-error "Worker thread ~a: Unhandled job error: ~a" thread-id c)
		       (reject-job
			future
			pool-name
			thread-id
			(format nil "~a" c))))))
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
  "Instantiates a thread pool. The function has the following arguments:
   <ul>
   <li>size Number of worker threads.</li>
   <li>:name Name of the pool.</li>
   </ul>
   Returns a thread pool."
  (if (<= size 0)
      (error "Pool size must be at least one."))
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
      (log-info "Thread pool ~a has been started." (slot-value pool 'name))
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
  "<p>Stops all worker threads. The function returns when all worker threads are no longer alive 
   or when the timeout has been reached or when the pool is stopping or is already stopped. 
   All pending jobs that are not currently being executed by a worker thread will be cancelled
   by one of the worker threads.</p>
   <p>The function does not destroy threads but signals to the worker threads that they are 
   supposed to end. If a worker thread refuses to end it will be left running.</p>
   <p>See also pool-stopped-p to check if the pool has successfully been stopped.</p>"
  (assert-threadpoolp pool)
  (if (worker-thread-p pool)
      (error (format
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
		     (if (not (eq 0 (queue-size-no-lock pool)))
			 (error
			  "All worker threads of pool ~a have ended but job queue is not empty."
			  (slot-value pool 'name)))
		     (setf (slot-value pool 'state) :stopped)
		     (log-info "Pool ~a has stopped." (slot-value pool 'name))
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
	  (error (format nil
			 "Cannot add job to stopping or stopped thread pool: ~a"
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
  "Adds a job to the pool. The function has the following arguments:
   <ul> 
   <li>pool A thread pool.</li>
   <li>job A function with no arguments.</li>
   </ul>
   Returns a future."
  (add-job-impl pool job))

(defun run-jobs (pool jobs)
  "Executes a batch of jobs and returns their results. Blocks until 
   all jobs are done. The function has the following arguments:
   <ul>
   <li>pool A thread pool</li>
   <li>jobs A list of jobs. Each job is represented by a function with no arguments.</li>
   </ul>
   May signal one of the following conditions:
   <ul>
   <li>job-execution-error When a job has signalled a condition.</li>
   <li>job-cancellation-error When a job has been cancelled.</li>
   </ul>
   Returns an ordered list of job results."
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
	  (push (job-value future) job-results)
	(condition (c)
	  (if (not job-error) (setf job-error c))))
      ;; We are owning the futures here and can put them back into the pool.
      (put-future (slot-value pool 'future-factory) future))
    (if job-error (error job-error) (reverse job-results))))

  
