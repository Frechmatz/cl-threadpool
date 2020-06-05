(defpackage :cl-threadpool
  (:use :cl)
  (:export :make-threadpool)
  (:export :start)
  (:export :stop)
  (:export :threadpoolp)
  (:export :worker-thread-p)
  (:export :threadpool-error)
  (:export :run-jobs)
  (:export :queue-size)
  (:export :pool-name)
  (:export :*logger*))


