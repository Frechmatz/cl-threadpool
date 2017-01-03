(defsystem :cl-threadpool-test
  :serial t
  :description "Tests for cl-threadpool"
  :long-description "Tests for cl-threadpool"
  :licence "Public Domain / 0-clause MIT"
  :depends-on (
	       :lisp-unit
	       :bordeaux-threads
	       :queues.simple-cqueue
	       :verbose)
  :components (
	       (:module "src/threadpool"
			:serial t
			:components ((:file "packages")
				     (:file "threadpool")))
	       (:module "test"
                        :serial t
                        :components ((:file "packages")
				     ))
               (:module "test/threadpool"
                        :serial t
			:components ((:file "run-jobs")
				     (:file "threadpoolp-test")
				     (:file "pool-stopping-worker-test")
				     (:file "pool-stop-twice-test")
				     (:file "pool-stop-instantiated-test")
				     (:file "pool-start-twice-test")
				     (:file "pool-add-job-test")
				     (:file "crashing-worker-test")
				     (:file "pool-queue-length-test"))
			)
	       ))
