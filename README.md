# cl-threadpool

A Thread pool implemented in Common Lisp

The thread pool consists of a number of worker threads and a job queue. The worker threads are picking
jobs from the queue and execute them. 

Installation
------------

Download cl-threadpool and add it to the asdf-system path

Dependencies (all available in Quicklisp):

* [bordeaux-threads](https://github.com/sionescu/bordeaux-threads) 
* [queues](https://github.com/oconnore/queues)
* [verbose](https://github.com/Shinmera/verbose)

Example
-------

Load cl-threadpool

    (asdf:load-system "cl-threadpool")

Create a thread pool with 5 worker threads

    (defparameter *threadpool* (cl-threadpool:make-threadpool 5))

Start the pool

    (cl-threadpool:start *threadpool*)

Add a job

    (cl-threadpool:add-job
       *threadpool*
       (lambda () (sleep 5)))

Stop the pool

    (cl-threadpool:stop *threadpool*)


API
---

More detailed documentation is provided by the documentation strings of the functions.

* **make-threadpool** (size &key (name nil) (max-queue-size nil) (resignal-job-conditions nil))

    * __size__ Number of worker threads
    * __max-queue-size__ Maximum size of job queue
    * __name__  Name of the pool
    * __resignal-job-conditions__ if t then conditions signalled by a worker will be resignalled as errors
  
* **start** (pool)

* **add-job** (pool job)

   See also **threadpool-error-queue-capacity-exceeded**

    * __pool__ a threadpool   
    * __job__  a function with zero arguments that will be executed by a worker thread of the pool

* **stop** (pool &key (force-destroy-timeout-seconds nil))

   The function returns when all worker threads are no longer alive. A worker thread terminates
when no job is available and the thread pool shall be stopped.

    * __force-destroy-timeout-seconds__ An optional timeout in seconds after which all pending
worker threads will be destroyed.
  
* **threadpoolp** (obj)

   Returns t if the given object represents a thread pool.

* **worker-thread-p** (pool)

   Returns t if the current thread is a worker thread of the pool.

Condition-Types
---------------

* **threadpool-error** (error)

   The default condition that is signalled by the pool in any case of pool thread usage related errors.

* **threadpool-error-queue-capacity-exceeded** (error)

   This condition is signalled when the capacity of the job queue is being exceeded when adding a job.


Logging
-------

The thread pool uses the [Verbose](https://github.com/Shinmera/verbose) framework for logging.

Disable logging

    (setf (v:repl-categories) (v:remove-repl-category (list :cl-threadpool)))

Set logging level (globally)

    (setf (v:repl-level) :error)

