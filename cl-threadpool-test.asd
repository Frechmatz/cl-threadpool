(defsystem :cl-threadpool-test
    :serial t
    :description "Tests for cl-threadpool"
    :long-description "Tests for cl-threadpool"
    :licence "MIT"
    :author "Oliver <frechmatz@gmx.de>"
    :maintainer "Oliver <frechmatz@gmx.de>"
    :version "1.0.0"
    :homepage "https://github.com/Frechmatz/cl-threadpool"  
    :depends-on (:lisp-unit
		 :cl-threadpool
		 :verbose)
    :components ((:module "src/threadpool"
			  :serial t
			  :components ((:file "packages")
				       (:file "threadpool")))
		 (:module "test"
			  :serial t
			  :components ((:file "packages")))
		 (:module "test/util"
			  :serial t
			  :components ((:file "result-list")
				       (:file "list-to-array")
				       (:file "logger")))
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
				       (:file "pool-stop-processed-all-test")
				       (:file "pool-queue-length-test")
				       (:file "force-destroy-test")
				       (:file "lost-wakeup")
				       (:file "run-jobs-sync")
				       (:file "job-dispatch")))))
