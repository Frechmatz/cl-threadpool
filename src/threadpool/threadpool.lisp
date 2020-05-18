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

(define-condition threadpool-error-queue-capacity-exceeded (error)
  ((text :initarg :text :reader text))
  (:documentation "This condition is signalled when a job could not be added to the pool
   because the maximum number of pending jobs was reached"))

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

(defmacro with-threads (threadlist thread  &body body)
  "Iterate through all non-exited threads"
  (let ((cur-thread (gensym)))
    `(bt:with-lock-held ((slot-value ,threadlist 'lock))
       (dolist (,cur-thread (slot-value ,threadlist 'threads))
	 (if ,cur-thread
	     (if (not (bt:thread-alive-p ,cur-thread))
		 (setf ,cur-thread nil) ;;; release thread object
		   (let ((,thread ,cur-thread))
		     ,@body)))))))

(defun generate-thread-name (threadlist)
  "Generate a thread name"
  (bt:with-lock-held ((slot-value threadlist 'lock))
    (format nil "~a-Thread-~a" (slot-value threadlist 'thread-name-prefix) (gensym))))

(defun add-thread (threadlist thread)
  "Add a thread. If the thread is already present then do nothing"
  (bt:with-lock-held ((slot-value threadlist 'lock))
    (if (not (find-if (lambda (cur-thread) (eq thread cur-thread)) (slot-value threadlist 'threads)))
	(push thread (slot-value threadlist 'threads)))))

(defun thread-count (threadlist)
  "Get number of non-exited threads"
  (let ((c 0))
    (with-threads threadlist thread
      (declare (ignore thread))
      (setf c (+ c 1)))
    c))

(defun threadlist-worker-thread-p (threadlist thread)
  "Returns true if the given thread is a worker thread of the given pool."
  (bt:with-lock-held ((slot-value threadlist 'lock))
    (find-if (lambda (cur-thread) (eq thread cur-thread)) (slot-value threadlist 'threads))))

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
   (max-queue-size :initform nil)
   (job-lock-pool :initform (make-job-lock-pool))
   (threads :initform (make-instance 'threadlist :thread-name-prefix "Threadpool"))
   (size :initform nil :documentation "Number of worker threads")
   (name :initform "Threadpool")
   (state :initform nil
	  :documentation
	  "State of the thread pool. 
           One of nil, :RUNNING, :STOPPING, :STOPPED")
   (state-lock :initform (bt:make-lock "thread-pool-state-lock"))
   (cv :initform (bt:make-condition-variable))
   (cv-lock :initform (bt:make-lock "thread-pool-cv-lock"))))

(defmethod initialize-instance :after ((pool threadpool) &key name size max-queue-size) 
  (setf (slot-value pool 'name) (if name name (format nil "Threadpool-~a" (gensym))))
  (setf (slot-value pool 'size) size)
  (setf (slot-value pool 'max-queue-size)
	(if max-queue-size
	    max-queue-size
	    50))
  (setf (slot-value (slot-value pool 'threads) 'thread-name-prefix) (slot-value pool 'name)))

(defmacro with-pool-state-lock-held (pool state &body body)
  `(bt:with-lock-held ((slot-value ,pool 'state-lock))
     (let ((,state (slot-value pool 'state)))
       ,@body)))

(defun get-job (pool)
  "Get a job of the job queue. Returns nil if no job is available"
  (bt:with-lock-held ((slot-value pool 'state-lock))
    (queues:qpop (slot-value pool 'job-queue))))

(defmacro signal-pool-error-if (predicate pool error-message &key (cond-type 'threadpool-error))
  `(if (funcall ,predicate)
       (error ',cond-type :text (format nil "~a: ~a" ,error-message (slot-value ,pool 'name)))))

(defun notify-all (pool)
  "Wake up all blocked threads."
  ;; did not found a better approach yet :(
  (dotimes (i (thread-count (slot-value pool 'threads)))
    (bt:condition-notify (slot-value pool 'cv))))

(defun destroy-all (pool)
  "Destroy all threads that haven't exited yet."
  (with-threads (slot-value pool 'threads) thread
    (write-log
     :info
     :cl-threadpool
     "Destroying thread ~a"
     :format-arguments (list (bt:thread-name thread)))
    (bt:destroy-thread thread)))

(defun all-threads-stopped-p (pool) 
  "Returns t if all threads are stopped."
  (eq 0 (thread-count (slot-value pool 'threads))))

(defun make-worker-thread (pool)
  "Adds a worker thread to the pool."
  (let ((thread-name (generate-thread-name (slot-value pool 'threads))))
    (add-thread
     (slot-value pool 'threads)
     (bt:make-thread
      (lambda ()
	;; Register thread when it starts execution
	(add-thread (slot-value pool 'threads) (bt:current-thread))
	(labels
	    ((wait ()
	       "Wait until thread is scheduled for execution"
	       ;; Lock
	       (bt:acquire-lock (slot-value pool 'cv-lock) t)
	       ;; Wait
	       (bt:condition-wait
		(slot-value pool 'cv)
		(slot-value pool 'cv-lock))
	       ;; Unlock (we do not need the lock for further processing)
	       (bt:release-lock (slot-value pool 'cv-lock))
	       (write-log
		:trace
		:cl-threadpool
		"Worker thread ~a has been woken up"
		:format-arguments (list thread-name)))
	     (is-quit ()
	       "Returns true when pool is stopping and the thread shall exit"
	       (with-pool-state-lock-held pool state
		 (eq state :stopping)))
	     (process ()
	       "Fetch jobs from queue and process them"
	       (loop
		  (let ((job (get-job pool)))
		    (if job
			(progn
			  (handler-case
			      (funcall job)
			    (condition (c)
			      (write-log
			       :error
			       :cl-threadpool
			       "Job of worker thread ~a signalled a condition: ~a"
			       :format-arguments (list thread-name c))
			      (error c))))
			(return))))))
	  (write-log
	   :info
	   :cl-threadpool
	   "Worker thread ~a has started."
	   :format-arguments (list thread-name))
	  (loop
	     (wait)
	     (process)
	     (if (is-quit)
		 (return)))
	  (write-log
	   :info
	   :cl-threadpool
	   "Worker thread ~a has stopped."
	   :format-arguments (list thread-name))))
      :name thread-name))
    nil))


;;
;; Helper stuff for stopping a pool
;;

(defmacro poll ((&key (timeout-seconds nil)) test-body timeout-body)
  "Evaluates repeatedly test-body. If the test-body returns true the loop
   terminates. If the timeout has been reached the timeout-body is executed 
   and the loop terminates.
   timeout-seconds -- the timeout in seconds or nil for no timeout.
   test-body -- The form to be evaluated repeatedly. It is up to the 
      test body to take care of CPU usage. The test-body is evaluated 
      at least once.
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

;;
;;
;; API
;;
;;

(defun make-threadpool (size &key (name nil) (max-queue-size nil))
  "Create a thread pool.
   name -- Name of the pool.
   size -- Number of worker threads.
   max-queue-size -- The maximum number of pending jobs"
  (make-instance 'threadpool
		 :name name
		 :size size
		 :max-queue-size max-queue-size))

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
  (with-pool-state-lock-held pool s
    (signal-pool-error-if (lambda() s) pool "Thread pool can only be started once")
    (write-log
     :info
     :cl-threadpool
     "Starting thread pool ~a..."
     :format-arguments (list (slot-value pool 'name)))
    (dotimes (i (slot-value pool 'size))
      (make-worker-thread pool))
    (setf (slot-value pool 'state) :running)
    (write-log
     :info
     :cl-threadpool
     "Thread pool ~a has been started"
     :format-arguments (list (slot-value pool 'name)))))

(defun stop (pool &key (force-destroy-timeout-seconds nil))
  "Stop the thread pool.
   Returns when all threads have stopped.
   pool -- A threadpool instance created by make-threadpool.
   force-destroy-timeout-seconds -- An optional timeout after all still active
      threads will be destroyed.
   * All queued jobs will be executed.
   * The stopping thread must not be a worker thread of the pool (to avoid deadlock)."
  (if (not (threadpoolp pool))
      (error 'threadpool-error :text "Not an instance of threadpool"))
  (signal-pool-error-if
   (lambda () (worker-thread-p pool))
   pool
   "Thread pool cannot be stopped by a worker thread")
  (if (with-pool-state-lock-held pool s
	(if (eq s :stopped)
	    nil) ;;; already stopped: return nil
	(setf (slot-value pool 'state) :stopping)
	t) ;;; not stopped: return t
      (poll (:timeout-seconds force-destroy-timeout-seconds)
	    (progn
	      (write-log
	       :info
	       :cl-threadpool
	       "Stopping thread pool ~a..."
	       :format-arguments (list (slot-value pool 'name)))
	      (notify-all pool)
	      (sleep 1)
	      (all-threads-stopped-p pool))
	    (progn
	      (write-log
	       :info
	       :cl-threadpool
	       "Stopping thread pool ~a: Timeout reached. Destroying threads..."
	       :format-arguments (list (slot-value pool 'name)))
	      (destroy-all pool))))
  (with-pool-state-lock-held pool s
    (setf s :stopped))


  (write-log
   :info
   :cl-threadpool
   "Thread pool ~a has stopped"
   :format-arguments (list (slot-value pool 'name))))
  
(defun add-job (pool job)
  "Add a job to the pool. 
   pool -- A threadpool instance as created by make-threadpool.   
   job -- A function with zero arguments. A job is supposed to handle all conditions.
   * The pool must have been started.
   * The pool must not be in stopping state.
   * The pool must not be in stopped state."
  (if (not (threadpoolp pool))
      (error 'threadpool-error :text "Not an instance of threadpool"))
  (with-pool-state-lock-held pool s
    (signal-pool-error-if
     (lambda () (eq s nil))
     pool
     "Cannot add job to thread pool that hasn't been started")
    (signal-pool-error-if
     (lambda () (eq s :stopping))
     pool
     "Cannot add job to stopping thread pool")
    (signal-pool-error-if
     (lambda () (eq s :stopped))
     pool
     "Cannot add job to stopped thread pool")
    (signal-pool-error-if
     (lambda () (> (queues:qsize (slot-value pool 'job-queue)) (slot-value pool 'max-queue-size)))
     pool
     "Maximum job queue length reached"
     :cond-type threadpool-error-queue-capacity-exceeded)
    (queues:qpush (slot-value pool 'job-queue) job)
    (write-log
     :trace
     :cl-threadpool
     "Added job to queue of thread pool ~a"
     :format-arguments (list (slot-value pool 'name)))
    ;; All worker threads are waiting on pool cv. Once a thread has been
    ;; notified it starts a loop of fetching and processing queued jobs.
    ;; If the queue is empty, the thread again waits on pool cv.
    (bt:condition-notify (slot-value pool 'cv))))

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
      job-results)))
    
