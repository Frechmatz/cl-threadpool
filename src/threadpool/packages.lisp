(defpackage :cl-threadpool
  (:use :cl)
  (:export :make-threadpool)
  (:export :start)
  (:export :stop)
  (:export :add-job)
  (:export :threadpoolp)
  (:export :worker-thread-p)
  )


