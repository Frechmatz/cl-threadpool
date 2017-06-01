;;
;; A Thread Pool
;;


(in-package :cl-threadpool)

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
;; Thread pool
;;

(defclass threadpool ()
  ((job-queue :initform (queues:make-queue :simple-queue))
   (max-queue-size :initform nil)
   (resignal-job-conditions :initform nil)
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

(defmethod initialize-instance :after ((pool threadpool) &key name size max-queue-size resignal-job-conditions) 
  (setf (slot-value pool 'name) (if name name (format nil "Threadpool-~a" (gensym))))
  (setf (slot-value pool 'resignal-job-conditions) resignal-job-conditions)
  (setf (slot-value pool 'size) size)
  (setf (slot-value pool 'max-queue-size)
	(if max-queue-size
	    max-queue-size
	    (* size 2)))
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
    (v:info :cl-threadpool "Destroying thread ~a" (bt:thread-name thread))
    (bt:destroy-thread thread)))

(defun all-threads-stopped-p (pool) 
  "Returns t if all threads are stopped."
  (eq 0 (thread-count (slot-value pool 'threads))))

(defun make-worker-thread (pool)
  "Adds a worker thread to the pool."
  (let ((thread-name (generate-thread-name (slot-value pool 'threads)))
	(resignal-job-conditions (slot-value pool 'resignal-job-conditions)))
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
	       (v:trace :cl-threadpool "Worker thread ~a has been woken up" thread-name))
	     (is-quit ()
	       "Returns true when pool is stopping and the thread shall exit"
	       (with-pool-state-lock-held pool state
		 (eq state :stopping)))
	     (process ()
	       "Fetch jobs from queue and process them"
	       (loop
		  (let ((job (get-job pool)))
		    (if job
			(handler-case
			    (funcall job)
			  (condition (c)
			    (v:error
			     :cl-threadpool
			     "Job of worker thread ~a signalled a condition: ~a"
			     thread-name c)
			    (if resignal-job-conditions
				(error c))))
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
;; Helper stuff
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

(defun make-threadpool (size &key (name nil) (max-queue-size nil) (resignal-job-conditions nil))
  "Create a thread pool.
   name -- Name of the pool.
   size -- Number of worker threads.
   max-queue-size -- The maximum number of pending jobs
   resignal-job-conditions -- if t then conditions signalled by the worker will be 
   resignalled as errors"
  (make-instance 'threadpool
		 :name name
		 :size size
		 :max-queue-size max-queue-size
		 :resignal-job-conditions resignal-job-conditions))

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
  ;; set global logging level
  ;;(setf (v:repl-level) :error)
  ;; disable logging
  ;;(setf (v:repl-categories) (v:remove-repl-category (list :cl-threadpool)))
  (if (not (threadpoolp pool))
      (error 'threadpool-error :text "Not an instance of threadpool"))
  (with-pool-state-lock-held pool s
    (signal-pool-error-if (lambda() s) pool "Thread pool can only be started once")
    (v:info :cl-threadpool "Starting thread pool ~a..." (slot-value pool 'name))
    (dotimes (i (slot-value pool 'size))
      (make-worker-thread pool))
    (setf (slot-value pool 'state) :running)
    (v:info :cl-threadpool
	    "Thread pool ~a has been started"
	    (slot-value pool 'name))))

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
	  (v:info :cl-threadpool
		  "Stopping thread pool ~a..."
		  (slot-value pool 'name))
	  (notify-all pool)
	  (sleep 1)
	  (all-threads-stopped-p pool))
	(progn
	  (v:info :cl-threadpool
		  "Stopping thread pool ~a: Timeout reached. Destroying threads..."
		  (slot-value pool 'name))
	  (destroy-all pool))))
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
    (v:trace :cl-threadpool "Added job to queue of thread pool ~a" (slot-value pool 'name))
    (bt:condition-notify (slot-value pool 'cv))))

