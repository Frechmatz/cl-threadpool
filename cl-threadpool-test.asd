(defsystem :cl-threadpool-test
    :serial t
    :description "Tests for cl-threadpool"
    :long-description "Tests for cl-threadpool"
    :licence "MIT"
    :author "Oliver <frechmatz@gmx.de>"
    :maintainer "Oliver <frechmatz@gmx.de>"
    :version "0.0.1"
    :homepage "https://github.com/Frechmatz/cl-threadpool"  
    :depends-on (:lisp-unit
		 :bordeaux-threads
		 :queues.simple-cqueue
		 :verbose)
    :components ((:module "src/threadpool"
			  :serial t
			  :components ((:file "packages")
				       (:file "threadpool")))
		 (:module "test"
			  :serial t
			  :components ((:file "packages")))
		 (:module "test/threadpool"
			  :serial t
			  :components (
				       (:file "worker-thread-p-test")
				       (:file "run-jobs")
				       (:file "threadpoolp-test")
				       (:file "pool-stopping-worker-test")
				       (:file "pool-stop-twice-test")
				       (:file "pool-stop-instantiated-test")
				       (:file "pool-start-twice-test")
				       (:file "pool-add-job-test")
				       (:file "crashing-worker-test")
				       (:file "pool-queue-length-test")
				       (:file "force-destroy-test")))))
