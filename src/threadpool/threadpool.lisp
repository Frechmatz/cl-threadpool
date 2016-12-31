;;
;; A Thread Pool
;;

(in-package :cl-threadpool)

(defparameter *THREADPOOL-STATE-INSTANTIATED* "INSTANTIATED")
(defparameter *THREADPOOL-STATE-RUNNING* "RUNNING")
(defparameter *THREADPOOL-STATE-STOPPING* "STOPPING")
(defparameter *THREADPOOL-STATE-STOPPED* "STOPPED")

(defparameter *THREAD-STATE-INSTANTIATED* "INSTANTIATED")
(defparameter *THREAD-STATE-IDLE* "IDLE")
(defparameter *THREAD-STATE-PROCESSING-JOBS* "PROCESSING-JOBS")
(defparameter *THREAD-STATE-STOPPED* "STOPPED")

(defclass threadpool ()
  ((job-queue :initform (queues:make-queue :simple-queue))
   (job-queue-lock :initform (bt:make-lock "thread-pool-queue-lock"))
   (threads :initform '())
   (size :initform 5)
   (name :initform "Threadpool")
   (state :initform *THREADPOOL-STATE-INSTANTIATED*)
   (state-lock :initform (bt:make-lock "thread-pool-state-lock"))
   (cv :initform (bt:make-condition-variable))
   (cv-lock :initform (bt:make-lock "thread-pool-cv-lock"))))

(defun get-job (pool)
  (let ((job nil))
    (bt:with-lock-held ((slot-value pool 'job-queue-lock))
      (setf job (queues:qpop (slot-value pool 'job-queue))))
  job))

(defun set-threadpool-state (pool state)
  (bt:with-lock-held ((slot-value pool 'state-lock))
    (setf (slot-value pool 'state) state)))

(defun is-threadpool-state (pool state)
  (let ((s nil))
    (bt:with-lock-held ((slot-value pool 'state-lock))
      (setf s (string= state (slot-value pool 'state))))
    s))

(defclass thread-data ()
  ((state :initform *THREAD-STATE-INSTANTIATED*)
   (state-lock :initform (bt:make-lock "thread-data-state-lock"))))

(defun set-thread-state (thread state)
  (bt:with-lock-held ((slot-value thread 'state-lock))
    (setf (slot-value thread 'state) state)))

(defun is-thread-state (thread state)
  (let ((s nil))
    (bt:with-lock-held ((slot-value thread 'state-lock))
      (setf s (string= state (slot-value thread 'state))))
    s))

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
  ;; Kind of a bozo implementation
  (for-each-thread
   pool
   (lambda (thread thread-local-data)
     (declare (ignore thread))
     (if (is-thread-state thread-local-data *THREAD-STATE-IDLE*)
	 (bt:condition-notify (slot-value pool 'cv)))
     nil)))

(defun is-all-threads-stopped (pool)
  "Returns t if all threads are stopped." 
  (let ((stopped t))
    (for-each-thread
     pool
     (lambda (thread thread-local-data)
       (declare (ignore thread))
       (if (not (is-thread-state thread-local-data *THREAD-STATE-STOPPED*))
	   (progn
	     (setf stopped nil)
	     t) ;; Abort loop
	   nil)))
    stopped))


(defun create-poolthread (pool name)
  (let ((thread-local-data (make-instance 'thread-data)))
    (let ((thread
	   (bt:make-thread
	    (lambda ()
	      (labels (
		       (wait ()
			 (v:trace :cl-threadpool "Worker thread ~a is going to sleep" name)
			 (set-thread-state thread-local-data *THREAD-STATE-IDLE*) 
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
			 (is-threadpool-state pool *THREADPOOL-STATE-STOPPING*))
		       (process ()
			 "Process jobs until there is no more job available. 
                          Returns nil if worker thread is to be stopped"
			 (set-thread-state thread-local-data *THREAD-STATE-PROCESSING-JOBS*) 
			 (loop
			    (let ((job (get-job pool)))
			      (if job
				  (handler-case
				      (funcall job)
				    (condition (c)
				      (v:error
				       :cl-threadpool
				       "Job of worker thread ~a signalled an unhandled condition: ~a"
				       name c))))
			      (return (not (is-quit)))))))
		(v:info :cl-threadpool "Worker thread ~a has started." name)
		(loop
		   (wait)
		   (if (not (process))
		       (return)))
		(set-thread-state thread-local-data *THREAD-STATE-STOPPED*) 
		(v:info :cl-threadpool "Worker thread ~a has stopped." name)
		))
	    :name name)))
      (list thread thread-local-data))))

;;
;;
;; Public functions
;;
;;

(defun threadpoolp (obj)
  "Returns t if the given object represents a thread pool."
  (typep obj 'threadpool))

(defun make-threadpool (name size)
  "Create a thread pool.
   name: Name of the pool.
   size: Number of worker threads."
  (let ((pool (make-instance 'threadpool)))
    (setf (slot-value pool 'name) name)
    (setf (slot-value pool 'size) size)
    pool))

(defun start (pool)
  "Start the threadpool.
   pool: A threadpool instance created by make-threadpool."
  (if (not (threadpoolp pool))
      (error "Not an instance of threadpool"))
  (bt:with-lock-held ((slot-value pool 'state-lock))
    (let ((s (slot-value pool 'state)))
      (if (not (string= s *THREADPOOL-STATE-INSTANTIATED*))
	  (error (format nil "Pool can only be started once: ~a" (slot-value pool 'name)))))
    (v:info :cl-threadpool "Starting threadpool ~a..." (slot-value pool 'name))
    (dotimes (i (slot-value pool 'size))
      (let ((thread
	     (create-poolthread
	      pool
	      (format nil "~a-thread-~a" (slot-value pool 'name) i))))
	(add-thread pool (first thread) (second thread))))
    (setf (slot-value pool 'state) *THREADPOOL-STATE-RUNNING*)
    (v:info :cl-threadpool "Threadpool ~a has been started" (slot-value pool 'name))))

(defun stop (pool)
  "Stop the threadpool.
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
      (error (format nil "Threadpool cannot be stopped by a worker thread: ~a" (slot-value pool 'name))))
  (let ((enter-loop nil))
    (bt:with-lock-held ((slot-value pool 'state-lock))
      (let ((s (slot-value pool 'state)))
	(if (string= s *THREADPOOL-STATE-STOPPING*)
	    (error "Tried stopping a threadpool that is already stopping: ~a" (slot-value pool 'name)))
	(if (string= s *THREADPOOL-STATE-STOPPED*)
	    (error "Tried stopping an already stopped threadpool: ~a" (slot-value pool 'name)))
	(if (string= s *THREADPOOL-STATE-INSTANTIATED*)
	    (setf (slot-value pool 'state) *THREADPOOL-STATE-STOPPED*)
	    (progn 
	      (setf (slot-value pool 'state) *THREADPOOL-STATE-STOPPING*)
	      (setf enter-loop t))
	    )))
    (if enter-loop
	(loop
	   (v:info :cl-threadpool "Stopping threadpool ~a..." (slot-value pool 'name))
	   (notify-all-idle-threads pool)
	   (sleep 1)
	   (if (is-all-threads-stopped pool)
	       (return))))
    (set-threadpool-state pool *THREADPOOL-STATE-STOPPED*)
    (v:info :cl-threadpool "Threadpool ~a has stopped" (slot-value pool 'name))))

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
      (if (string= s *THREADPOOL-STATE-STOPPING*)
	  (error "Tried adding job to stopping threadpool"))
      (if (string= s *THREADPOOL-STATE-STOPPED*)
	  (error "Tried adding job to stopped threadpool"))
      (if (string= s *THREADPOOL-STATE-INSTANTIATED*)
	  (error "Tried adding job to threadpool that hasn't been started"))
      (bt:with-lock-held ((slot-value pool 'job-queue-lock))
	(queues:qpush (slot-value pool 'job-queue) job)
	(v:info :cl-threadpool "Added job to queue")
	(bt:condition-notify (slot-value pool 'cv))))))


