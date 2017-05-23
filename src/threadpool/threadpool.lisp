;;
;; A Thread Pool
;;
;; Todo: rename threadsv2
;;

(in-package :cl-threadpool)

;;
;; Thread list
;;

;; Todo: Set name of pool in constructor
(defclass thread-list ()
  ((lock :initform (bt:make-lock "thread-list-lock"))
   (threads :initform '())))

(defun thread-list-add-slot (thread-list)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  ;;(break)
  (bt:with-lock-held ((slot-value thread-list 'lock))
    (let ((name (format nil "cl-threadpool-worker-thread-~a" (gensym))))
      (push (list name :pending) (slot-value thread-list 'threads))
      name)))

(defun thread-list-attach-thread-to-slot (thread-list slot-name thread)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  ;;(break)
  (bt:with-lock-held ((slot-value thread-list 'lock))
    (let ((slot (assoc slot-name (slot-value thread-list 'threads) :test #'string=)))
      (if (not slot)
	  (error (format nil "Slot not found for name ~a" slot-name))
	  (setf (second slot) thread)))))

(defun thread-list-remove-slot (thread-list slot-name)
  (bt:with-lock-held ((slot-value thread-list 'lock))
    (setf (slot-value thread-list 'threads)
	  (remove-if (lambda (thread) (string= slot-name (first thread))) (slot-value thread-list 'threads)))))

(defun thread-list-length (thread-list)
  (bt:with-lock-held ((slot-value thread-list 'lock))
    (length (slot-value thread-list 'threads))))

(defun thread-list-worker-thread-p (thread-list thread)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  ;;(break)
  (bt:with-lock-held ((slot-value thread-list 'lock))
    (find-if (lambda (cur-thread) (eq thread (second cur-thread))) (slot-value thread-list 'threads))))

(defmacro thread-list-with-pool-threads (thread-list thread  &body body)
  "Iterate through all non-pending threads"
  (let ((cur-thread (gensym)))
    `(bt:with-lock-held ((slot-value ,thread-list 'lock))
       (dolist (,cur-thread (slot-value ,thread-list 'threads))
	 (if (not (eq :pending (second ,cur-thread)))
	     (let ((,thread (second ,cur-thread)))
	       ,@body))))))

;;
;; Thread pool
;;

(defclass threadpool ()
  ((job-queue :initform (queues:make-queue :simple-queue))
   (max-queue-size :initform nil)
   (threadsv2 :initform (make-instance 'thread-list))
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
      (thread-list-worker-thread-p (slot-value pool 'threadsv2) (bt:current-thread))))

;; Todo: do-times (thread-count)) as the algorithm is stupid enough
(defun notify-all-idle-threads (pool)
  "Wake up all blocked threads."
  (thread-list-with-pool-threads (slot-value pool 'threadsv2) thread
    (declare (ignore thread))
    (bt:condition-notify (slot-value pool 'cv)))
    nil)

(defun is-all-threads-stopped (pool) 
  "Returns t if all threads are stopped."
  (eq 0 (thread-list-length (slot-value pool 'threadsv2))))

(defun make-worker-thread (pool)
  "Adds a worker thread to the pool. The function has no useful return value."
  ;; Register the thread.
  (let ((thread-name (thread-list-add-slot (slot-value pool 'threadsv2))))
    (bt:make-thread
     (lambda ()
       ;; Attach thread instance to previously registered thread
       (thread-list-attach-thread-to-slot
	(slot-value pool 'threadsv2)
	thread-name
	(bt:current-thread))
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
		  "Process jobs until there is no more job available. 
                          Returns nil if worker thread is to be stopped"
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
			   (return (not (is-quit))))))))
	 (v:info :cl-threadpool "Worker thread ~a has started." thread-name)
	 (loop
	    (wait)
	    (if (not (process))
		(return)))
	 (thread-list-remove-slot (slot-value pool 'threadsv2) thread-name)
	 (v:info :cl-threadpool "Worker thread ~a has stopped." thread-name)))
     :name thread-name)
    nil))

;;
;;
;; Exported functions
;;
;;

(defun make-threadpool (name size &key (max-queue-size nil))
  "Create a thread pool.
name -- Name of the pool.
size -- Number of worker threads.
max-queue-size -- The maximum number of pending jobs"
  (let ((pool (make-instance 'threadpool)))
    (setf (slot-value pool 'name) name)
    (setf (slot-value pool 'size) size)
    (setf (slot-value pool 'max-queue-size)
	  (if max-queue-size
	      max-queue-size
	      (* size 2)))
    pool))

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
 pool -- A threadpool instance created by make-threadpool.
 * The function returns when all worker threads have stopped.
 * All pending jobs will be executed.
 * The stopping thread must not be a worker thread of the pool (to avoid deadlock).
 * The pool must not be in stopping state.
 * The pool must not be in stopped state.
 * If the pool has not been started yet, the pool state is set to stopped."
  (if (not (threadpoolp pool))
      (error "Not an instance of threadpool"))
  (if (worker-thread-p pool)
      (error (format
	      nil
	      "Thread pool cannot be stopped by a worker thread: ~a"
	      (slot-value pool 'name))))
  (let ((enter-loop nil))
    (with-pool-state-lock-held pool s
      (signal-pool-error-if (lambda () (eq s :stopping)) pool "Tried stopping a thread pool that is already stopping")
      (signal-pool-error-if (lambda () (eq s :stopped)) pool "Tried stopping an already stopped thread pool")
      (if (not s)
	  (setf (slot-value pool 'state) :stopped)
	  (progn 
	    (setf (slot-value pool 'state) :stopping)
	    (setf enter-loop t))))
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
 pool -- A threadpool instance as created by make-threadpool.   
 job -- A function with zero arguments.
 
* The pool must have been started.
* The pool must not be in stopping state.
* The pool must not be in stopped state."
  (if (not (threadpoolp pool))
      (error "Not an instance of threadpool"))
  (with-pool-state-lock-held pool s
    (signal-pool-error-if (lambda () (eq s nil)) pool "Cannot add job to thread pool that hasn't been started")
    (signal-pool-error-if (lambda () (eq s :stopping)) pool "Cannot add job to stopping thread pool")
    (signal-pool-error-if (lambda () (eq s :stopped)) pool "Cannot add job to stopped thread pool")
    (if (> (queues:qsize (slot-value pool 'job-queue)) (slot-value pool 'max-queue-size))
	(error "Maximum job queue length reached: ~a" (slot-value pool 'name)))
    (queues:qpush (slot-value pool 'job-queue) job)
    (v:trace :cl-threadpool "Added job to queue of thread pool ~a" (slot-value pool 'name))
    (bt:condition-notify (slot-value pool 'cv))))


