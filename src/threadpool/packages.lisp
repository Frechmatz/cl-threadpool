(defpackage :cl-threadpool
  (:use :cl)
  (:export :future)
  (:export :get-value)
  (:export :make-threadpool)
  (:export :start)
  (:export :stop)
  (:export :threadpoolp)
  (:export :worker-thread-p)
  (:export :threadpool-error)
  (:export :threadpool-execution-error)
  (:export :add-job)
  (:export :run-jobs)
  (:export :queue-size)
  (:export :pool-name)
  (:export :*logger*))


