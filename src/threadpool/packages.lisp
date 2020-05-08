(defpackage :cl-threadpool
  (:use :cl)
  (:export :make-threadpool)
  (:export :start)
  (:export :stop)
  (:export :add-job)
  (:export :threadpoolp)
  (:export :worker-thread-p)
  (:export :threadpool-error)
  (:export :threadpool-error-queue-capacity-exceeded)
  (:export :run-jobs)
  (:export :job-execution-error)
  (:export :job-execution-error-p))


