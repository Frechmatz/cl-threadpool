(defpackage :cl-threadpool
  (:use :cl)
  (:export :threadpool-error)
  (:export :threadpool-execution-error)
  (:export :threadpoolp)
  (:export :worker-thread-p)
  (:export :future-value)
  (:export :future-done-p)
  (:export :make-threadpool)
  (:export :start)
  (:export :stop)
  (:export :add-job)
  (:export :run-jobs)
  (:export :queue-size)
  (:export :pool-name)
  (:export :*logger*)
  (:export :*job-error-to-report*))


