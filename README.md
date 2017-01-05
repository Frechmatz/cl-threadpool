# cl-threadpool
A Common Lisp implementation of a thread pool

Installation
------------

Download cl-threadpool and add it to the asdf-system path

Dependencies:
* bordeaux-threads
* queues
* verbose

Example
-------

Load cl-threadpool

```lisp
(asdf:load-system "cl-threadpool")
```

Create a thread pool with 5 worker threads

```lisp
(defparameter *threadpool* (cl-threadpool:make-threadpool "My Thread Pool" 5))
```

Start the pool

```lisp
(cl-threadpool:start *threadpool*)
```

Add a job

```lisp
(cl-threadpool:add-job
   *threadpool*
   (lambda () (sleep 5)))
```

Stop the pool

```lisp
(cl-threadpool:stop *threadpool*)
```

