;;
;; A Thread Pool
;;


(in-package :cl-threadpool)

(defparameter *logger*
  (lambda(level who format-control format-arguments)
    (declare (ignore level who format-control format-arguments))
    nil))

(defun write-log (level who format-control &key (format-arguments nil))
  (funcall *logger* level who format-control format-arguments))

(define-condition threadpool-error (error)
  ((text :initarg :text :reader text))
  (:documentation "The default condition that is signalled by thread pools"))

;;
;; Thread list
;;

(defclass threadlist ()
  ((lock :initform (bt:make-lock "threadlist-lock"))
   (threads :initform '())
   (thread-name-prefix :initform "threadlist"))
  (:documentation
   "Instances of this class hold all threads created by the thread pool."))

(defmethod initialize-instance :after ((tlist threadlist) &key thread-name-prefix)
  (setf (slot-value tlist 'thread-name-prefix) thread-name-prefix))

(defun generate-thread-id (threadlist)
  "Generate a thread name"
  (bt:with-lock-held ((slot-value threadlist 'lock))
    (format
     nil
     "~a-Thread-~a"
     (slot-value threadlist 'thread-name-prefix)
     (gensym))))

(defun add-thread (threadlist thread thread-id)
  "Add a thread."
  ;; TODO Assert that id not exists
  (bt:with-lock-held ((slot-value threadlist 'lock))
    (push (list :id thread-id :thread thread) (slot-value threadlist 'threads))))

(defun remove-thread (threadlist thread-id)
  "Remove a thread."
  (bt:with-lock-held ((slot-value threadlist 'lock))
    (setf (slot-value threadlist 'threads)
	  (remove-if (lambda (item)
		       (eq (getf item :id) thread-id))
		     (slot-value threadlist 'threads)))))

(defun thread-count (threadlist)
  "Get number of non-exited threads"
  (bt:with-lock-held ((slot-value threadlist 'lock))
    (length (slot-value threadlist 'threads))))

(defun threadlist-worker-thread-p (threadlist thread)
  "Returns true if the given thread is a worker thread of the given pool."
  (bt:with-lock-held ((slot-value threadlist 'lock))
    (find-if (lambda (cur-thread) (eq thread (getf cur-thread :thread)))
		     (slot-value threadlist 'threads))))

;;
;; Helper stuff for synchronous execution of jobs
;;

(defun make-job-lock-pool ()
  "Creates a pool that manages job locks.
   Returns a property list with the following keys:
   - :get A function that allocates a job lock. If there is no lock available 
     a new one will be created. Locks returned by this function are supposed to be put back
     into the pool.
   - :put A function that puts back a previously allocated job lock into the pool.
   A job lock is represented by a property list with the following keys:
   - :set-value A function with one argument that sets the result of a job.
   - :get-value A function that returns the result of a job. If the result is 
     already available it will immediately be returned. Otherwise the function
     blocks on the execution of the job."
  (let ((pool-lock (bt:make-lock "job-lock-pool-lock")) (pool nil) (created-lock-count 0))
    (flet ((make-job-lock ()
	     (let ((job-lock (bt:make-lock "job-lock"))
		   (job-cv (bt:make-condition-variable))
		   (result nil)
		   (state :pending))
	       (list
		:set-value (lambda(v)
			     (bt:with-lock-held (job-lock)
			       (setf result v)
			       (setf state :set)
			       (bt:condition-notify job-cv)))
		:get-value (lambda()
			     (bt:acquire-lock job-lock)
			     (if (eq state :set)
				 (progn
				   (bt:release-lock job-lock)
				   result)
				 (progn
				   ;; loop in order to handle spurious wakeups
				   (loop
				      (bt:condition-wait job-cv job-lock)
				      (if (eq state :set)
					  (return)))
				   (bt:release-lock job-lock)
				   result)))
		:reset (lambda()
			 (setf state :pending)
			 (setf result nil))))))
      (list
       :get (lambda()
	      "Return an available lock or create a new one."
	      (bt:with-lock-held (pool-lock)
		(let ((l (pop pool)))
		  (if (not l)
		      (progn
			(setf created-lock-count (+ 1 created-lock-count))
			(setf l (make-job-lock))))
		  l)))
       :put (lambda(job-lock)
	      "Put a lock back into the pool."
	      (bt:with-lock-held (pool-lock)
		(funcall (getf job-lock :reset))
		(push job-lock pool)
		nil))
       :length (lambda()
		 (bt:with-lock-held (pool-lock)
		   (length pool)))
       :created-lock-count (lambda()
		 (bt:with-lock-held (pool-lock)
		   created-lock-count))))))

;;
;; Thread pool
;;

(defclass threadpool ()
  ((job-queue :initform (queues:make-queue :simple-queue))
   (job-lock-pool :initform (make-job-lock-pool))
   (threads :initform (make-instance 'threadlist :thread-name-prefix "Threadpool"))
   (size :initform nil :documentation "Number of worker threads")
   (name :initform "Threadpool")
   (state :initform :pending
	  :documentation
	  "State of the thread pool. 
           One of nil, :PENDING, :RUNNING, :STOPPING, :STOPPED")
   (state-lock :initform (bt:make-lock "thread-pool-state-lock"))
   (cv :initform (bt:make-condition-variable))))

(defmethod initialize-instance :after ((pool threadpool) &key name size) 
  (setf (slot-value pool 'name) (if name name (format nil "Threadpool-~a" (gensym))))
  (setf (slot-value pool 'size) size)
  (setf (slot-value (slot-value pool 'threads) 'thread-name-prefix) (slot-value pool 'name)))

(defun queue-size (pool)
  "Get the current length of the job queue."
  (if (not (threadpoolp pool))
      (error 'threadpool-error :text "Not an instance of threadpool"))
  (bt:with-lock-held ((slot-value pool 'state-lock))
    (queues:qsize (slot-value pool 'job-queue))))

(defun pool-name (pool)
  "Get the name of the pool."
  (if (not (threadpoolp pool))
      (error 'threadpool-error :text "Not an instance of threadpool"))
  (bt:with-lock-held ((slot-value pool 'state-lock))
    (slot-value pool 'name)))

(defun make-worker-thread (pool thread-id)
  "Adds a worker thread to the pool. Assumes that pool lock is set."
  (bt:make-thread
   (lambda ()
     (write-log :info :cl-threadpool "Worker thread ~a has started."
		:format-arguments (list thread-id))
     (bt:acquire-lock (slot-value pool 'state-lock) t)
     (loop ;; Entry point of loop assumes that pool lock is set
	(let ((job (queues:qpop (slot-value pool 'job-queue))))
	  (if (eq (slot-value pool 'state) :stopping)
	      (progn
		(bt:release-lock (slot-value pool 'state-lock))
		(return)))
	  (if job
	      (progn
		(bt:release-lock (slot-value pool 'state-lock))
		;;(write-log
		;; :info
		;; :cl-threadpool
		;; "Worker thread ~a executes a job."
		;; :format-arguments (list thread-id))
		(funcall job)
		(bt:acquire-lock (slot-value pool 'state-lock) t))
	      (progn
		(bt:condition-wait (slot-value pool 'cv)
				   (slot-value pool 'state-lock))))))
     (remove-thread
      (slot-value pool 'threads) thread-id)
     (write-log
      :info
      :cl-threadpool
      "Worker thread ~a has stopped."
      :format-arguments (list thread-id)))
   :name thread-id))

(defun make-threadpool (size &key (name nil))
  "Create a thread pool.
   name -- Name of the pool.
   size -- Number of worker threads."
  (make-instance 'threadpool :name name :size size))

(defun threadpoolp (obj)
  "Returns t if the given object represents a thread pool."
  (typep obj 'threadpool))

(defun worker-thread-p (pool)
  "Returns true if the current thread is a worker thread of the given pool."
  (if (not (threadpoolp pool))
      nil
      (threadlist-worker-thread-p (slot-value pool 'threads) (bt:current-thread))))

(defun start (pool)
  "Start the thread pool.
   pool -- A thread pool instance created by make-threadpool."
  (if (not (threadpoolp pool))
      (error 'threadpool-error :text "Not an instance of threadpool"))
  (bt:with-lock-held ((slot-value pool 'state-lock))
    (let ((s (slot-value pool 'state)))
      (if (not (eq s :pending))
	  (error 'threadpool-error
		 :text (format nil "Thread pool can only be started once: ~a"
			       (slot-value pool 'name))))
      (write-log
       :info
       :cl-threadpool
       "Starting thread pool ~a..."
       :format-arguments (list (slot-value pool 'name)))
      (dotimes (i (slot-value pool 'size))
	(let ((thread-id (generate-thread-id (slot-value pool 'threads))))
	  (add-thread
	   (slot-value pool 'threads)
	   (make-worker-thread pool thread-id)
	   thread-id)))
      (setf (slot-value pool 'state) :running)
      (write-log
       :info
       :cl-threadpool
       "Thread pool ~a has been started"
       :format-arguments (list (slot-value pool 'name))))))

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
  "Stop the thread pool.
   Returns when all threads have stopped.
   pool -- A threadpool instance created by make-threadpool.
   timeout-seconds -- An optional timeout in seconds.
   Returns nil when all worker threads have been be stopped."
  (if (not (threadpoolp pool))
      (error 'threadpool-error :text "Not an instance of threadpool"))
  (if (worker-thread-p pool)
      (error 'threadpool-error
	     :text (format
		    nil
		    "Thread pool cannot be stopped by a worker thread: ~a"
		    (slot-value pool 'name))))
  (let ((pool-already-stopped nil) (stopped-all-threads t))
    (bt:with-lock-held ((slot-value pool 'state-lock))
      (let ((s (slot-value pool 'state)))
	(if (eq s :stopped)
	    (setf pool-already-stopped t)
	    (setf (slot-value pool 'state) :stopping))))
    (if (not pool-already-stopped)
	(progn
	  (write-log
	   :info
	   :cl-threadpool
	   "Stopping thread pool ~a..."
	   :format-arguments (list (slot-value pool 'name)))
	  (poll (:timeout-seconds timeout-seconds)
		(progn
		  (dotimes (i (thread-count (slot-value pool 'threads)))
		    (bt:condition-notify (slot-value pool 'cv)))
		  (sleep 1)
		  (if (eq 0 (thread-count (slot-value pool 'threads)))
		      (progn
			(bt:with-lock-held ((slot-value pool 'state-lock))
			  (setf (slot-value pool 'state) :stopped))
			(write-log
			 :info
			 :cl-threadpool
			 "Stopping thread pool ~a: Pool has successfully stopped"
			 :format-arguments (list (slot-value pool 'name)))
			t)
		      nil))
		(progn
		  (setf stopped-all-threads nil)
		  (write-log
		   :info
		   :cl-threadpool
		   "Stopping thread pool ~a: Timeout reached. Giving up."
		   :format-arguments (list (slot-value pool 'name)))))))
    (not stopped-all-threads)))
  
(defun add-job (pool job)
  "Add a job to the pool. 
   pool -- A threadpool instance as created by make-threadpool.   
   job -- A function with zero arguments. A job is supposed to handle all conditions.
   * The pool must have been started.
   * The pool must not be in stopping state.
   * The pool must not be in stopped state."
  (if (not (threadpoolp pool))
      (error 'threadpool-error :text "Not an instance of threadpool"))
  (bt:with-lock-held ((slot-value pool 'state-lock))
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
      (queues:qpush (slot-value pool 'job-queue) job)
      (write-log
       :trace
       :cl-threadpool
       "Added job to queue of thread pool ~a"
       :format-arguments (list (slot-value pool 'name)))
      ;; All worker threads are waiting on pool cv. Once a thread has been
      ;; notified it starts a loop of fetching and processing queued jobs.
      ;; If the queue is empty, the thread again waits on pool cv.
      (bt:condition-notify (slot-value pool 'cv))
      nil)))

(defun run-jobs (pool jobs)
  "Synchronously run a list of jobs and return their results. Blocks the current thread until 
   all jobs have been completed. Jobs are supposed to handle all conditions.
   - pool: The threadpool
   - jobs: A list of jobs. Each job is represented by a function with no arguments.
   Returns an ordered list of the results that the jobs have returned."
  (if (not (threadpoolp pool))
      (error 'threadpool-error :text "pool is not an instance of threadpool"))
  (if (not (listp jobs))
      (error "jobs must be a list"))
  (flet ((wrap-job (job job-lock)
	     (lambda()
	       (funcall (getf job-lock :set-value) (funcall job)))))
    (let ((job-locks nil) (job-results nil))
      ;; Wrap jobs and push them into the job queue
      (dolist (job jobs)
	(if (not (functionp job))
	    (error "Job must be a function"))
	(let* ((job-lock (funcall (getf (slot-value pool 'job-lock-pool) :get)))
	       (wrapped-job (wrap-job job job-lock)))
	  (push job-lock job-locks)
	  (add-job pool wrapped-job)))
      ;; Wait for completion of jobs
      (dolist (job-lock job-locks)
	(let ((result (funcall (getf job-lock :get-value))))
	  (push result job-results)
	  ;; put job-lock back into lock pool
	  (funcall (getf (slot-value pool 'job-lock-pool) :put) job-lock)))
      ;; Why no reverse of the results?
      ;; Because job results have already been fetched in reverse order.
      job-results)))
    
