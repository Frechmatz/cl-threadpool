(defsystem :cl-threadpool-test
  :serial t
  :description "Tests for cl-threadpool"
  :long-description "Tests for cl-threadpool"
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
				     (:file "threadpoolp-test"))
			)
	       ))
