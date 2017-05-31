# cl-threadpool
A Common Lisp Thread-Pool implementation

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
(defparameter *threadpool* (cl-threadpool:make-threadpool 5))
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

Logging
-------

Disable logging

```lisp
(setf (v:repl-categories) (v:remove-repl-category (list :cl-threadpool)))
```

Set logging level (this effects all logging categories)

```lisp
;; log errors
(setf (v:repl-level) :error)
```

API
---

Condition-Types
---------------


