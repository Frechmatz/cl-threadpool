# cl-threadpool

A Thread pool implemented in Common Lisp

The thread pool consists of a fixed number of worker threads and a job queue. The worker
threads are picking jobs from the queue and execute them. 

Change-Log
----------

* The pool setting 'resignal-job-conditions' has been removed. Conditions signalled during
  the execution of a job will always be re-signalled.


Installation
------------

Install (downloads and installs all dependencies)

    (ql:quickload "cl-threadpool")

Dependencies:

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

* **make-threadpool** (size &key (name nil) (max-queue-size nil))

    * __size__ Number of worker threads
    * __max-queue-size__ Maximum size of job queue
    * __name__  Name of the pool
  
* **start** (pool)

* **add-job** (pool job)

   See also **threadpool-error-queue-capacity-exceeded**

    * __pool__ a threadpool   
    * __job__  a function with zero arguments that will be executed by a worker thread of the pool. A Job is supposed to handle all conditions.

* **run-jobs** (pool jobs)

   Synchronously executes a list of jobs. The current thread will be blocked until all jobs have been executed. 

    * __pool__ a threadpool   
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

Condition-Types
---------------

* **threadpool-error** (error)

   The default condition that is signalled by the pool in any case of pool thread usage related errors.

* **threadpool-error-queue-capacity-exceeded** (error)

   This condition is signalled when the capacity of the job queue is being exceeded when adding a job.


Logging
-------

The thread pool uses the [Verbose](https://github.com/Shinmera/verbose) framework for logging.

Disable logging of the cl-threadpool package

    (setf (v:repl-categories) (v:remove-repl-category (list :cl-threadpool)))

Set logging level (globally)

    (setf (v:repl-level) :error)

Running the tests
-----------------

The tests are using the [lisp-unit](https://github.com/OdonataResearchLLC/lisp-unit) framework.
The system definition file is cl-threadpool-test.asd.

Run all tests

    (asdf:load-system "cl-threadpool-test")
    (in-package :cl-threadpool-test)
    (run-tests)

Run a specific test

    (asdf:load-system "cl-threadpool-test")
    (in-package :cl-threadpool-test)
    (run-tests '(worker-thread-p-test-1))

The thread pool has been tested on the following operating systems and Lisp implementations:

* __MacOS 10.11.3 (El Capitan)__: SBCL, CCL, ABCL (Java 1.8)
* __Windows 10__: SBCL, ABCL (Java 1.8)
* __MacOS 10.15.1 (Catalina)__: SBCL 2.0.2

The `/script` directory contains a couple of shell scripts for running the test suite.


Contact
-------

On any questions/bugs/feature requests create an issue or contact me: [Oliver](mailto:frechmatz@gmx.de)




