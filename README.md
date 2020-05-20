# cl-threadpool

A Thread pool implemented in Common Lisp

The thread pool consists of a fixed number of worker threads and a job queue. The worker
threads are picking jobs from the queue and execute them. 

Change-Log
----------

* Version 1.0.0 (current quicklisp release)
* Version 2.0.0 (work in progress)
    * Added run-jobs (pool, jobs) for synchronous execution of jobs.
    * Removed :resignal-job-conditions argument from threadpool. The pool no longer handles conditions signalled by a job.
    * Removed dependency 'verbose'.
    * Removed :max-queue-size argument from threadpool. The size of the job queue is now unlimited.
    * Removed condition threadpool-error-queue-capacity-exceeded.
    * Added queue-size (pool) to get the number of jobs waiting for execution.

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

Start the pool.

    (cl-threadpool:start *threadpool*)

Run some jobs. Blocks the current thread until all jobs have finished.

    (let ((results
           (cl-threadpool:run-jobs
            pool
            (list
             (lambda() (sleep 5) "Job 1")
             (lambda() (sleep 2) "Job 2")
             (lambda() (sleep 1) "Job 3")))))
      (format t "~a" (first results)) ;; => "Job 1"
      (format t "~a" (second results)) ;; => "Job 2"
      (format t "~a" (third results)))) ;; => "Job 3"

Stop the pool

    (cl-threadpool:stop *threadpool*)


API
---

* **make-threadpool** (size &key (name nil))

    * __size__ Number of worker threads
    * __name__  Name of the pool

    Returns an object representing a threadpool.
    
* **start** (pool)

    Starts the given pool by instantiating the worker threads.

* **add-job** (pool job)

    Adds a job to the queue. 

    * __pool__ A threadpool   
    * __job__  A function with zero arguments that will be executed by a worker thread of the pool. A Job is supposed to handle all conditions.

   Returns nil.

* **run-jobs** (pool jobs)

   Synchronously executes a list of jobs. The current thread will be blocked until all jobs have been executed. 

    * __pool__ A threadpool   
    * __jobs__  A list of jobs. Each job is represented by a function with zero arguments. A Job is supposed to handle all conditions.

    Returns an ordered list of job results.

* **stop** (pool &key (force-destroy-timeout-seconds nil))

   Stops the thread pool. The function returns when all worker threads are no longer alive. A worker thread terminates
when no job is available and the thread pool is stopping.

    * __force-destroy-timeout-seconds__ An optional timeout in seconds after which all still alive
pool threads will be destroyed.
  
* **threadpoolp** (obj) => generalized boolean

   Returns t if the given object represents a thread pool.

* **worker-thread-p** (pool) => generalized boolean

   Returns t if the current thread is a worker thread of the pool.

* **queue-size** (pool)

   Returns the number of jobs waiting for execution.
  

Condition-Types
---------------

* **threadpool-error** (error)

   The default condition that is signalled by the pool in any case of pool thread usage related errors.

Logging
-------

The threadpool logs low-level events by calling the function ``cl-threadpool:*logger*``
The default implementation of this function is empty.

Running the tests
-----------------

    (asdf:load-system "cl-threadpool-test")
    (in-package :cl-threadpool-test)
    (run-tests)


The thread pool has been tested on the following operating systems and Lisp implementations:

* __MacOS 10.11.3 (El Capitan)__: SBCL, CCL, ABCL (Java 1.8)
* __Windows 10__: SBCL, ABCL (Java 1.8)
* __MacOS 10.15.1 (Catalina)__: SBCL 2.0.2
* __MacOS 10.15.1 (Catalina)__: ABCL 1.6.1 Java 1.8.0_252 AdoptOpenJDK

The `/script` directory contains a couple of shell scripts for running the test suite.


Contact
-------

On any questions/bugs/feature requests create an issue or contact me: [Oliver](mailto:frechmatz@gmx.de)




