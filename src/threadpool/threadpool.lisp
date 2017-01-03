;;
;; A Thread Pool
;;

(in-package :cl-threadpool)

(defclass threadpool ()
  ((job-queue :initform (queues:make-queue :simple-queue))
   (max-queue-size :initform nil)
   (threads :initform '())
   (size :initform nil)
   (name :initform "Threadpool")
   (state :initform nil
	  :documentation
	     "State of the thread pool. 
              One of nil, :RUNNING, :STOPPING, :STOPPED")
   (state-lock :initform (bt:make-lock "thread-pool-state-lock"))
   (cv :initform (bt:make-condition-variable))
   (cv-lock :initform (bt:make-lock "thread-pool-cv-lock"))))

(defun get-job (pool)
  (let ((job nil))
    (bt:with-lock-held ((slot-value pool 'state-lock))
      (setf job (queues:qpop (slot-value pool 'job-queue))))
  job))

(defclass thread-data ()
  ;; Thread state MUST NOT be changed by the thread pool.
  ((state :initform nil
	  :documentation
	  "State of the worker thread. One of
           nil, :IDLE, :PROCESSING-JOBS, :STOPPED")
   ;; Thread state is monitored by the thread pool.
   ;; Thats why we need a lock.
   (state-lock :initform (bt:make-lock "thread-data-state-lock"))))

(defun set-thread-state (thread state)
  (bt:with-lock-held ((slot-value thread 'state-lock))
    (setf (slot-value thread 'state) state)))

(defun is-thread-state (thread state)
  (let ((s nil))
    (bt:with-lock-held ((slot-value thread 'state-lock))
      (setf s (eq state (slot-value thread 'state))))
    s))

;; Add a thread instance to the thread pool
(defun add-thread (pool thread thread-local-data)
  (push
   (list thread thread-local-data)
   (slot-value pool 'threads)))

(defun for-each-thread (pool fn)
  "Calls given function for each thread with thread and thread data. 
  Aborts loop when fn returns t"
  (dolist (thread (slot-value pool 'threads))
    (if (funcall fn (first thread) (second thread))
	(return))))

(defun is-worker-thread (thread pool)
  (let ((is nil))
    (for-each-thread
     pool
     (lambda (cur-thread cur-thread-local-data)
       (declare (ignore cur-thread-local-data))
       (if (eq thread cur-thread)
	   (progn
	     (setf is t)
	     t)
	   nil)))
    is))
     
(defun notify-all-idle-threads (pool)
  "Wake up all blocked threads."
  (for-each-thread
   pool
   (lambda (thread thread-local-data)
     (declare (ignore thread))
     (if (is-thread-state thread-local-data :idle)
	 (bt:condition-notify (slot-value pool 'cv)))
     nil)))

(defun is-all-threads-stopped (pool)
  "Returns t if all threads are stopped." 
  (let ((stopped t))
    (for-each-thread
     pool
     (lambda (thread thread-local-data)
       (declare (ignore thread))
       (if (not (is-thread-state thread-local-data :stopped))
	   (progn
	     (setf stopped nil)
	     t) ;; Abort loop
	   nil)))
    stopped))

;; Create a worker thread.
;; The thread must not change the pool state
(defun make-worker-thread (pool name)
  (let ((thread-local-data (make-instance 'thread-data)))
    (let ((thread
	   (bt:make-thread
	    (lambda ()
	      (labels (
		       (wait ()
			 (v:trace :cl-threadpool "Worker thread ~a is going to sleep" name)
			 (set-thread-state thread-local-data :idle) 
			 ;; Lock
			 (bt:acquire-lock (slot-value pool 'cv-lock) t)
			 ;; Wait
			 (bt:condition-wait
			  (slot-value pool 'cv)
			  (slot-value pool 'cv-lock))
			 ;; Unlock (we do not need the lock for further processing)
			 (bt:release-lock (slot-value pool 'cv-lock))
			 (v:trace :cl-threadpool "Worker thread ~a has been woken up" name))
		       (is-quit ()
			 (let ((s nil))
			   (bt:with-lock-held ((slot-value pool 'state-lock))
			     (setf s (slot-value pool 'state)))
			   (eq s :stopping)))
		       (process ()
			 "Process jobs until there is no more job available. 
                          Returns nil if worker thread is to be stopped"
			 (set-thread-state thread-local-data :processing-jobs) 
			 (loop
			    (let ((job (get-job pool)))
			      (if job
				  (handler-case
				      (funcall job)
				    (condition (c)
				      (v:error
				       :cl-threadpool
				       "Job of worker thread ~a signalled a condition: ~a"
				       name c)))
				  (return (not (is-quit))))))))
		(v:info :cl-threadpool "Worker thread ~a has started." name)
		(loop
		   (wait)
		   (if (not (process))
		       (return)))
		(set-thread-state thread-local-data :stopped) 
		(v:info :cl-threadpool "Worker thread ~a has stopped." name)
		))
	    :name name)))
      (list thread thread-local-data))))

;;
;;
;; Exported functions
;;
;;

(defun make-threadpool (name size &key (max-queue-size nil))
  "Create a thread pool.
   name: Name of the pool.
   size: Number of worker threads.
   max-queue-size: The maximum number of pending jobs"
  ;; todo: Input validation
  (let ((pool (make-instance 'threadpool)))
    (setf (slot-value pool 'name) name)
    (setf (slot-value pool 'size) size)
    (setf (slot-value pool 'max-queue-size)
	  (if max-queue-size
	      max-queue-size
	      (* size 2)))
    pool))

(defun threadpoolp (obj)
  "Returns t if the given object represents a thread pool."
  (typep obj 'threadpool))

(defun start (pool)
  "Start the thread pool.
   pool: A thread pool instance created by make-threadpool."
  (if (not (threadpoolp pool))
      (error "Not an instance of threadpool"))
  (bt:with-lock-held ((slot-value pool 'state-lock))
    (let ((s (slot-value pool 'state)))
      (if s
	  (error (format
		  nil
		  "Thread pool can only be started once: ~a"
		  (slot-value pool 'name)))))
    (v:info :cl-threadpool "Starting thread pool ~a..." (slot-value pool 'name))
    (dotimes (i (slot-value pool 'size))
      (let ((thread
	     (make-worker-thread
	      pool
	      (format
	       nil
	       "~a-thread-~a"
	       (slot-value pool 'name) i))))
	(add-thread pool (first thread) (second thread))))
    (setf (slot-value pool 'state) :running)
    (v:info :cl-threadpool
	    "Thread pool ~a has been started"
	    (slot-value pool 'name))))

(defun stop (pool)
  "Stop the thread pool.
   pool: A threadpool instance created by make-threadpool.
   - The function returns when all worker threads have stopped.
   - All pending jobs will be executed.
   - The stopping thread must not be a worker thread of the pool (to avoid deadlock).
   - The pool must not be in stopping state.
   - The pool must not be in stopped state.
   - If the pool has not been started yet, the pool state is set to stopped."
  (if (not (threadpoolp pool))
      (error "Not an instance of threadpool"))
  (if (is-worker-thread (bt:current-thread) pool)
      (error (format
	      nil
	      "Thread pool cannot be stopped by a worker thread: ~a"
	      (slot-value pool 'name))))
  (let ((enter-loop nil))
    (bt:with-lock-held ((slot-value pool 'state-lock))
      (let ((s (slot-value pool 'state)))
	(if (eq s :stopping)
	    (error (format
		    nil
		    "Tried stopping a thread pool that is already stopping: ~a"
		    (slot-value pool 'name))))
	(if (eq s :stopped)
	    (error (format
		    nil
		    "Tried stopping an already stopped thread pool: ~a"
		    (slot-value pool 'name))))
	(if (not s)
	    (setf (slot-value pool 'state) :stopped)
	    (progn 
	      (setf (slot-value pool 'state) :stopping)
	      (setf enter-loop t))
	    )))
    (if enter-loop
	(loop
	   (v:info :cl-threadpool "Stopping thread pool ~a..." (slot-value pool 'name))
	   (notify-all-idle-threads pool)
	   (sleep 1)
	   (if (is-all-threads-stopped pool)
	       (return))))
    (bt:with-lock-held ((slot-value pool 'state-lock))
      (setf (slot-value pool 'state) :stopped))
    (v:info :cl-threadpool "Thread pool ~a has stopped" (slot-value pool 'name))))

(defun add-job (pool job)
  "Add a job to the pool. 
   pool: A threadpool instance as created by make-threadpool.
   job: a function with zero arguments.
   - The pool must have been started.
   - The pool must not be in stopping state.
   - The pool must not be in stopped state."
  (if (not (threadpoolp pool))
      (error "Not an instance of threadpool"))
  (bt:with-lock-held ((slot-value pool 'state-lock))
    (let ((s (slot-value pool 'state)))
      (if (not s)
	  (error "Cannot add job to thread pool that hasn't been started"))
      (if (eq s :stopping)
	  (error "Cannot add job to stopping thread pool"))
      (if (eq s :stopped)
	  (error "Cannot add job to stopped thread pool"))
	(if (> (queues:qsize (slot-value pool 'job-queue)) (slot-value pool 'max-queue-size))
	    (error "Maximum job queue length reached: ~a" (slot-value pool 'name)))
	(queues:qpush (slot-value pool 'job-queue) job)
	(v:trace :cl-threadpool "Added job to queue of thread pool ~a" (slot-value pool 'name))
	(bt:condition-notify (slot-value pool 'cv)))))


