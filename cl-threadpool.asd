(defsystem :cl-threadpool
  :serial t
  :version "3.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-threadpool"  
  :description "Implementation of a thread pool"
  :long-description "Implementation of a thread pool"
  :depends-on (:bordeaux-threads
	       :queues.simple-cqueue)
  :components ((:module "src/threadpool"
			:serial t
			:components ((:file "packages")
				     (:file "threadpool"))))
  :in-order-to ((test-op (test-op "cl-threadpool/test"))))

(defsystem :cl-threadpool/test
  :serial t
  :description "Tests for cl-threadpool"
  :long-description "Tests for cl-threadpool"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :version "3.0.0"
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
			     (:file "job-worker-distribution-test"))))
  :perform (test-op (o c) (symbol-call :lisp-unit '#:run-tests :all :cl-threadpool-test)))

(defsystem :cl-threadpool/doc
  :serial t
  :version "3.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-threadpool"
  :description "Holds the documentation sources of cl-threadpool"
  :long-description
  "Holds the documentation sources of cl-threadpool. Documentation is created via (cl-threadpool-make-doc::make-doc)"
  :depends-on (:cl-threadpool :cl-html-readme :docparser)
  :components ((:module "examples"
			:serial t
			:components ((:file "example-1")
				     (:file "example-2")))
	       (:module "make-doc"
			:serial t
			:components ((:file "packages")
				     (:file "make-doc")))))
