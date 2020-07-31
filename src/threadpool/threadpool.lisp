;;
;; A Thread Pool
;;


(in-package :cl-threadpool)

(defparameter *logger*
  (lambda(level who format-control format-arguments)
    (declare (ignore level who format-control format-arguments))
    nil))

(defun log-info (format-control &rest format-arguments)
  (funcall *logger* :info :cl-threadpool format-control format-arguments))

(defun log-trace (format-control &rest format-arguments)
  (funcall *logger* :trace :cl-threadpool format-control format-arguments))


(define-condition threadpool-error (error)
  ((text :initarg :text :reader text))
  (:documentation "The default condition that is signalled by thread pools"))


;;
;; Future
;; TODO Think about which functions should be generic
;;

(defclass future ()
  (
   (lock :initform (bt:make-lock "future-lock"))
   (cv :initform (bt:make-condition-variable))
   (state :initform :pending) ;; TODO Think about initial state
   (job :initform nil)
   (value :initform nil)
   ))

(defgeneric set-value(future value)
  (:documentation "Sets the result of the job"))

(defgeneric get-value(future)
  (:documentation "Get the result of the job. If the result is 
     already available it will immediately be returned. Otherwise the function
     blocks on the execution of the job."))
(defgeneric reset (future))

(defmethod set-value ((future-instance future) value)
  (bt:with-lock-held ((slot-value future-instance 'lock))
    (setf (slot-value future-instance 'value) value)
    (setf (slot-value future-instance 'state) :set)
    (setf (slot-value future-instance 'job) nil) ;; Release job
    (bt:condition-notify (slot-value future-instance 'cv)))
  nil)

(defmethod get-value ((future-instance future))
  (bt:acquire-lock (slot-value future-instance 'lock))
  (if (eq (slot-value future-instance 'state) :set)
      (progn
	(bt:release-lock (slot-value future-instance 'lock))
	(slot-value future-instance 'value))
      (progn
	;; loop in order to handle spurious wakeups
	(loop
	   (bt:condition-wait (slot-value future-instance 'cv) (slot-value future-instance 'lock))
	   (if (eq (slot-value future-instance 'state) :set)
	       (return)))
	(bt:release-lock (slot-value future-instance 'lock))
	(slot-value future-instance 'value))))

(defmethod reset ((future-instance future))
  (setf (slot-value future-instance 'state) :pending)
  (setf (slot-value future-instance 'value) nil)
  (setf (slot-value future-instance 'job) nil)
  nil)

(defun futurep (obj)
  "Returns t if the given object represents a future."
  (typep obj 'future))

(defun assert-futurep (obj)
  (if (not (futurep obj))
      (error 'threadpool-error :text "Object is not an instance of future")))


;;
;; Future Factory
;;

(defclass future-factory ()
  (
   (lock :initform (bt:make-lock "future-factory"))
   (pool :initform nil)
   (created-future-count :initform 0)
  ))

(defgeneric get-future (future-factory)
  (:documentation "Get a future. Returns an available future or creates a new one."))

(defgeneric put-future (future-factory future)
  (:documentation "Put back a previously allocated future into the pool."))

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
  (bt:with-lock-held ((slot-value future-factory-instance 'lock))
    (reset future)
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

(defun make-worker-thread (pool thread-id)
  (bt:make-thread
   (lambda ()
     (log-info "Worker thread ~a has started." thread-id)
     (bt:acquire-lock (slot-value pool 'lock) t)
     (loop ;; Entry point of loop body assumes that pool lock is set
	(let ((future (queues:qpop (slot-value pool 'job-queue))))
	  (if (eq (slot-value pool 'state) :stopping)
	      (progn
		;; TODO Future handling (set to cancelled)
		(bt:release-lock (slot-value pool 'lock))
		(return)))
	  (if future
	      (progn
		;; TODO Check if cancelled
		(assert-futurep future) ;; TODO Replace assertion with logging and ignore job
		(bt:release-lock (slot-value pool 'lock))
		(set-value future (funcall (slot-value future 'job)))
		(bt:acquire-lock (slot-value pool 'lock) t))
	      (progn
		;; No Job -> Wait on Job Queue
		(bt:condition-wait (slot-value pool 'cv) (slot-value pool 'lock))))))
     ;; Remove thread from pool
     (bt:with-lock-held ((slot-value pool 'lock))
       (setf (slot-value pool 'threads)
	(remove-if (lambda (item) (eq (getf item :id) thread-id)) (slot-value pool 'threads))))
     (log-info "Worker thread ~a has stopped." thread-id))
   :name thread-id))

(defun make-threadpool (size &key (name nil))
  "Create a thread pool.
   name -- Name of the pool.
   size -- Number of worker threads."
  (make-instance 'threadpool :name name :size size))

(defun worker-thread-p (pool)
  "Returns true if the current thread is a worker thread of the given pool."
  (assert-threadpoolp pool)
  (let ((current-thread (bt:current-thread)))
    (bt:with-lock-held ((slot-value pool 'lock))
      (find-if (lambda (cur-thread) (eq current-thread (getf cur-thread :thread)))
	       (slot-value pool 'threads)))))

(defun start (pool)
  "Start the thread pool.
   pool -- A thread pool instance created by make-threadpool."
  (assert-threadpoolp pool)
  (bt:with-lock-held ((slot-value pool 'lock))
    (let ((s (slot-value pool 'state)))
      (if (not (eq s :pending))
	  (error 'threadpool-error
		 :text (format nil "Thread pool can only be started once: ~a"
			       (slot-value pool 'name))))
      (log-info "Starting thread pool ~a..." (slot-value pool 'name))
      (dotimes (i (slot-value pool 'size))
	(let ((thread-id (format nil "~a-Thread-~a" (slot-value pool 'name) (gensym))))
	  (push (list
		 :id thread-id
		 :thread (make-worker-thread pool thread-id))
		(slot-value pool 'threads))))
      (setf (slot-value pool 'state) :running)
      (log-info "Thread pool ~a has been started" (slot-value pool 'name)))))

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
   or when the timeout has been reached. Jobs still sitting in the queue may not be executed. 
   The function does not destroy threads but signals to the worker threads that they are 
   supposed to end. If a worker thread refuses to end it will be left running.
   Returns nil when all worker threads have been be stopped."
  (assert-threadpoolp pool)
  (if (worker-thread-p pool)
      (error 'threadpool-error
	     :text (format
		    nil
		    "Thread pool cannot be stopped by a worker thread: ~a"
		    (slot-value pool 'name))))
  (let ((pool-already-stopped nil) (stopped-all-threads t))
    (bt:with-lock-held ((slot-value pool 'lock))
      (let ((s (slot-value pool 'state)))
	(if (eq s :stopped)
	    (setf pool-already-stopped t)
	    (setf (slot-value pool 'state) :stopping))))
    (if (not pool-already-stopped)
	(progn
	  (log-info "Stopping thread pool ~a..." (slot-value pool 'name))
	  (poll (:timeout-seconds timeout-seconds)
		(progn
		  (dotimes (i (slot-value pool 'size))
		    (bt:condition-notify (slot-value pool 'cv)))
		  (sleep 1)
		  (let ((thread-count nil))
		    (bt:with-lock-held ((slot-value pool 'lock))
		      (setf thread-count (length (slot-value pool 'threads))))
		    (if (eq 0 thread-count)
			(progn
			  (bt:with-lock-held ((slot-value pool 'lock))
			    (setf (slot-value pool 'state) :stopped))
			  (setf stopped-all-threads t)
			  (log-info
			   "Stopping thread pool ~a: Pool has successfully stopped"
			   (slot-value pool 'name))
			  t)
			nil)))
		(progn
		  (setf stopped-all-threads nil)
		  (log-info "Stopping thread pool ~a: Timeout reached. Giving up."
			    (slot-value pool 'name))))))
    (not stopped-all-threads)))
  
(defun add-job (pool job)
  "Add a job to the pool. 
   pool -- A threadpool instance as created by make-threadpool.   
   job -- A function with zero arguments. A job is supposed to handle all conditions.
   * The pool must have been started.
   * The pool must not be in stopping state.
   * The pool must not be in stopped state.
   Returns a future."
  (assert-threadpoolp pool)
  (assert-jobp job)
  (bt:with-lock-held ((slot-value pool 'lock))
    (let ((s (slot-value pool 'state)))
      (if (eq s :pending)
	  (error 'threadpool-error
		 :text (format nil "Cannot add job to thread pool that hasn't been started: ~a"
			       (slot-value pool 'name))))
      (if (eq s :stopping)
	  (error 'threadpool-error
		 :text (format nil "Cannot add job to stopping thread pool: ~a"
			       (slot-value pool 'name))))
      (if (eq s :stopped)
	  (error 'threadpool-error
		 :text (format nil "Cannot add job to stopped thread pool: ~a"
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

(defun run-jobs (pool jobs)
  "Synchronously run a list of jobs and return their results. Blocks the current thread until 
   all jobs have been completed. Jobs are supposed to handle all conditions.
   - pool: The threadpool
   - jobs: A list of jobs. Each job is represented by a function with no arguments.
   Returns an ordered list of the results that the jobs have returned."
  (assert-threadpoolp pool)
  (if (not (listp jobs))
      (error "jobs must be a list"))
  (dolist (job jobs)
    (assert-jobp job))
  (let ((futures nil) (job-results nil))
    ;; TODO Replace with map
    (dolist (job jobs)
      (push (add-job pool job) futures))
    ;; Wait for completion of jobs
    (dolist (future futures)
      (assert-futurep future)
      (let ((result (get-value future)))
	(push result job-results)
	;; put future back into lock pool
	(put-future (slot-value pool 'future-factory) future)))
    ;; Why no reverse of the results? -> Job results have already been fetched in reverse order.
    job-results))

