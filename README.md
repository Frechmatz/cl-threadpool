# cl-threadpool

A thread pool implemented in Common Lisp.

A thread pool consists of a bunch of worker threads and a job queue. The focus of this
library is to provide an API that is as simple as possible.

Change-Log
----------

## Version 1.0.0

Initial release of cl-threadpool.

## Version 2.0.0

This version is the current quicklisp release.

Version 2 is a major rework of the thread pool with bugfixes, new features and
removal of features that have been identified as not being useful.

### Breaking changes

* Removed add-job. Has been replaced with run-jobs.
* Removed :resignal-job-conditions argument from make-threadpool. The pool no longer handles conditions signalled by a job.
* Removed :max-queue-size argument from make-threadpool. The size of the job queue is now unlimited.
* Removed condition threadpool-error-queue-capacity-exceeded.
* Removed dependency 'verbose'. The library no longer depends on a logging framework.

### New features

* Added run-jobs for synchronous execution of jobs.
* Added queue-size to get the number of jobs waiting for execution.
* Added pool-name to get the name of a thread pool.

## Version 3.0.0

Version 3.0.0 shall be the last version which introduces breaking changes.

### Breaking changes

* Removed cl-threadpool:start. A threadpool is now ready to use immediately after its instantiation.
* Removed condition threadpool-error. Pool usage errors and internal errors are now
  represented by simple-error.
* cl-threadpool:stop no longer returns an indicator if the pool has successfully been stopped. This
must now explicitly be checked via cl-threadpool:pool-stopped-p.
* Error handling of jobs has been reworked. The threadpool now catches all unhandled conditions of jobs. If a job has signalled a condition and its result is requested, a job-execution-error is signalled by the threadpool.

### New features

* Introduces Futures. A future represents the result of an asynchronous operation. 
* Added add-job Adds a job to the queue and returns a future.
* Added job-value, cancel-job, job-done-p, job-cancelled-p.
* Added conditions job-execution-error and job-cancellation-error.
* Added pool-stopped-p

Installation
------------

Install (downloads and installs all dependencies)

    (ql:quickload "cl-threadpool")

Example
-------

Load cl-threadpool.

    (asdf:load-system "cl-threadpool")

Create a thread pool with 5 worker threads.

    (defparameter *threadpool* (cl-threadpool:make-threadpool 5))

Run a batch of jobs. Blocks the current thread until all jobs have finished.

    (let ((results
           (cl-threadpool:run-jobs
            *threadpool*
            (list
             (lambda() (sleep 5) "Job 1")
             (lambda() (sleep 2) "Job 2")
             (lambda() (sleep 1) "Job 3")))))
      (format t "~a" (first results)) ;; => "Job 1"
      (format t "~a" (second results)) ;; => "Job 2"
      (format t "~a" (third results)))) ;; => "Job 3"

Stop the thread pool.

    (cl-threadpool:stop *threadpool*)


API
---

* **make-threadpool** (size &key (name nil))

    * __size__ Number of worker threads
    * __name__  Name of the pool

    Returns an object representing a thread pool.
    
* **run-jobs** (pool jobs)

   Synchronously executes a list of jobs. The current thread will be blocked until all jobs have finished. 

    * __pool__ A thread pool   
    * __jobs__  A list of jobs. Each job is represented by a function with zero arguments. A Job is supposed to handle all conditions.

    Returns an ordered list of job results.

* **stop** (pool &key (timeout-seconds nil))

   Stops all worker threads of the given thread pool. The function returns when all worker threads are no longer alive or when the timeout has been reached. Jobs still sitting in the queue may not be executed. The function does not destroy threads but signals to the worker threads that they are supposed to end. If a worker thread refuses to end it will be left running.
   
   Returns nil when all worker threads have been be stopped.

    * __pool__ A thread pool   
    * __timeout-seconds__ An optional timeout in seconds.
  
* **threadpoolp** (obj) => generalized boolean

   Returns t if the given object represents a thread pool.

* **worker-thread-p** (pool) => generalized boolean

   Returns t if the current thread is a worker thread of the given thread pool.

   * __pool__ A thread pool   

* **queue-size** (pool)

   Returns the number of jobs waiting for execution for the given thread pool.

    * __pool__ A thread pool   

* **pool-name** (pool)

   Returns the name of the given thread pool.
   
    * __pool__ A thread pool   

* **pool-stopped-p** (pool)

   Returns t if the pool has been stopped.
   
    * __pool__ A thread pool   


Logging
-------

The thread pool logs low-level events by calling the function ``cl-threadpool:*logger*``
The default implementation of this function is empty.

Running the tests
-----------------

    (asdf:load-system "cl-threadpool-test")
    (in-package :cl-threadpool-test)
    (run-tests)

Supported Lisp implementations and operating systems
----------------------------------------------------

cl-threadpool has been tested with:

* __MacOS 10.11.3 (El Capitan)__: SBCL, CCL, ABCL (Java 1.8)
* __MacOS 10.15.1 (Catalina)__: SBCL 2.0.2
* __MacOS 10.15.1 (Catalina)__: ABCL 1.6.1 Java 1.8.0_252 AdoptOpenJDK
* __MacOS 10.15.1 (Catalina)__: CCL Version 1.12 DarwinX8664
* __Windows 10__: SBCL 2.0.0
* __Windows 10__: ABCL 1.6.1 Java(TM) SE Runtime Environment (build 1.8.0_251-b08)

Contact
-------

On any questions/bugs/feature requests create an issue or contact me: [Oliver](mailto:frechmatz@gmx.de)




