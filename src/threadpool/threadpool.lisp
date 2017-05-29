;;
;; A Thread Pool
;;


(in-package :cl-threadpool)

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

(defmacro with-pool-threads (threadlist thread  &body body)
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
    (format nil "~a-~a" (slot-value threadlist 'thread-name-prefix) (gensym))))

(defun add-thread (threadlist thread)
  "Add a thread. If the thread is already present, then do nothing"
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (bt:with-lock-held ((slot-value threadlist 'lock))
    (if (not (find-if (lambda (cur-thread) (eq thread cur-thread)) (slot-value threadlist 'threads)))
	(push thread (slot-value threadlist 'threads)))))

(defun thread-count (threadlist)
  "Get number of non-exited threads"
  (let ((c 0))
    (with-pool-threads threadlist thread
      (declare (ignore thread))
      (setf c (+ c 1)))
    c))

(defun threadlist-worker-thread-p (threadlist thread)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  ;;(break)
  (bt:with-lock-held ((slot-value threadlist 'lock))
    (find-if (lambda (cur-thread) (eq thread cur-thread)) (slot-value threadlist 'threads))))

;;
;; Thread pool
;;

(defclass threadpool ()
  ((job-queue :initform (queues:make-queue :simple-queue))
   (max-queue-size :initform nil)
   (threads :initform (make-instance 'threadlist :thread-name-prefix "Threadpool"))
   (size :initform nil)
   (name :initform "Threadpool")
   (state :initform nil
	  :documentation
	     "State of the thread pool. 
              One of nil, :RUNNING, :STOPPING, :STOPPED")
   (state-lock :initform (bt:make-lock "thread-pool-state-lock"))
   (cv :initform (bt:make-condition-variable))
   (cv-lock :initform (bt:make-lock "thread-pool-cv-lock"))))

(defmethod initialize-instance :after ((pool threadpool) &key name size max-queue-size) 
  (setf (slot-value pool 'name) name)
  (setf (slot-value pool 'size) size)
  (setf (slot-value pool 'max-queue-size)
	(if max-queue-size
	    max-queue-size
	    (* size 2)))
  (setf (slot-value (slot-value pool 'threads) 'thread-name-prefix) name))

(defun get-job (pool)
  (let ((job nil))
    (bt:with-lock-held ((slot-value pool 'state-lock))
      (setf job (queues:qpop (slot-value pool 'job-queue))))
  job))

(defmacro with-pool-state-lock-held (pool state &body body)
  `(bt:with-lock-held ((slot-value ,pool 'state-lock))
     (let ((,state (slot-value pool 'state)))
       ,@body)))

(defmacro signal-pool-error-if (predicate pool error-message)
  `(if (funcall ,predicate)
       (error (format nil "~a: ~a" ,error-message (slot-value ,pool 'name)))))

(defun threadpoolp (obj)
  "Returns t if the given object represents a thread pool."
  (typep obj 'threadpool))

(defun worker-thread-p (pool)
  (if (not (threadpoolp pool))
      nil
      (threadlist-worker-thread-p (slot-value pool 'threads) (bt:current-thread))))

(defun notify-all (pool)
  "Wake up all blocked threads."
  ;; did not found a better approach yet :(
  (dotimes (i (thread-count (slot-value pool 'threads)))
    (bt:condition-notify (slot-value pool 'cv))))

(defun all-threads-stopped-p (pool) 
  "Returns t if all threads are stopped."
  (eq 0 (thread-count (slot-value pool 'threads))))

(defun make-worker-thread (pool)
  "Adds a worker thread to the pool. The function has no useful return value."
  (let ((thread-name (generate-thread-name (slot-value pool 'threads))))
    (add-thread
     (slot-value pool 'threads)
     (bt:make-thread
      (lambda ()
	;; register the thread once more or for the first time to ensure that
	;; regardless of timing circumstances a thread can be identified
	;; as a pool worker thread.
	(add-thread (slot-value pool 'threads) (bt:current-thread))
	(labels ((wait ()
		   (v:trace :cl-threadpool "Worker thread ~a is going to sleep" thread-name)
		   ;; Lock
		   (bt:acquire-lock (slot-value pool 'cv-lock) t)
		   ;; Wait
		   (bt:condition-wait
		    (slot-value pool 'cv)
		    (slot-value pool 'cv-lock))
		   ;; Unlock (we do not need the lock for further processing)
		   (bt:release-lock (slot-value pool 'cv-lock))
		   (v:trace :cl-threadpool "Worker thread ~a has been woken up" thread-name))
		 (is-quit ()
		   (with-pool-state-lock-held pool state
		     (eq state :stopping)))
		 (process ()
		   "Process jobs until there is no more job available."
		   (loop
		      (let ((job (get-job pool)))
			(if job
			    (handler-case
				(funcall job)
			      (condition (c)
				(v:error
				 :cl-threadpool
				 "Job of worker thread ~a signalled a condition: ~a"
				 thread-name c)))
			    (return))))))
	  (v:info :cl-threadpool "Worker thread ~a has started." thread-name)
	  (loop
	     (wait)
	     (process)
	     (if (is-quit)
		 (return)))
	  (v:info :cl-threadpool "Worker thread ~a has stopped." thread-name)))
      :name thread-name))
    nil))

;;
;;
;; API
;;
;;

(defun make-threadpool (name size &key (max-queue-size nil))
  "Create a thread pool.
   name -- Name of the pool.
   size -- Number of worker threads.
   max-queue-size -- The maximum number of pending jobs"
  (make-instance 'threadpool :name name :size size :max-queue-size max-queue-size))

(defun start (pool)
  "Start the thread pool.
   pool -- A thread pool instance created by make-threadpool."
  (if (not (threadpoolp pool))
      (error "Not an instance of threadpool"))
  (with-pool-state-lock-held pool s
    (signal-pool-error-if (lambda() s) pool "Thread pool can only be started once")
    (v:info :cl-threadpool "Starting thread pool ~a..." (slot-value pool 'name))
    (dotimes (i (slot-value pool 'size))
      (make-worker-thread pool))
    (setf (slot-value pool 'state) :running)
    (v:info :cl-threadpool
	    "Thread pool ~a has been started"
	    (slot-value pool 'name))))

(defun stop (pool)
  "Stop the thread pool.
   Returns when all threads have stopped.
   pool -- A threadpool instance created by make-threadpool.
   * Does not kill threads.
   * All pending jobs will be executed.
   * The stopping thread must not be a worker thread of the pool (to avoid deadlock).
   * The pool must not be in stopping state."
  (if (not (threadpoolp pool))
      (error "Not an instance of threadpool"))
  (signal-pool-error-if
   (lambda () (worker-thread-p pool))
   pool
   "Thread pool cannot be stopped by a worker thread")
  (if (with-pool-state-lock-held pool s
	(if (eq s :stopped)
	    nil) ;;; already stopped: return nil
	(signal-pool-error-if
	 (lambda () (eq s :stopping))
	 pool
	 "Cannot stop a thread pool that is already stopping")
	(setf (slot-value pool 'state) :stopping)
	t) ;;; not stopped: return t
      (loop
	 (v:info :cl-threadpool "Stopping thread pool ~a..." (slot-value pool 'name))
	 (notify-all pool)
	 (sleep 1)
	 (if (all-threads-stopped-p pool)
	     (return))))
  (with-pool-state-lock-held pool s
    (setf s :stopped))
  (v:info :cl-threadpool "Thread pool ~a has stopped" (slot-value pool 'name)))

(defun add-job (pool job)
  "Add a job to the pool. 
   pool -- A threadpool instance as created by make-threadpool.   
   job -- A function with zero arguments.
   * The pool must have been started.
   * The pool must not be in stopping state.
   * The pool must not be in stopped state."
  (if (not (threadpoolp pool))
      (error "Not an instance of threadpool"))
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
     "Maximum job queue length reached")
    (queues:qpush (slot-value pool 'job-queue) job)
    (v:trace :cl-threadpool "Added job to queue of thread pool ~a" (slot-value pool 'name))
    (bt:condition-notify (slot-value pool 'cv))))


