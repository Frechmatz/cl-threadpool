(defsystem :cl-threadpool-test
    :serial t
    :description "Tests for cl-threadpool"
    :long-description "Tests for cl-threadpool"
    :licence "MIT"
    :author "Oliver <frechmatz@gmx.de>"
    :maintainer "Oliver <frechmatz@gmx.de>"
    :version "2.0.0"
    :homepage "https://github.com/Frechmatz/cl-threadpool"  
    :depends-on (:lisp-unit
		 :cl-threadpool
		 :verbose)
    :components ((:module "test"
			  :serial t
			  :components ((:file "packages")))
		 (:module "test/util"
			  :serial t
			  :components ((:file "result-list")
				       (:file "list-to-array")
				       (:file "logger")))
		 (:module "test/threadpool"
			  :serial t
			  :components ((:file "future-test")
				       (:file "pool-state-test")
				       (:file "add-job-test")
				       (:file "run-jobs-test")
				       (:file "job-worker-distribution-test")))))
