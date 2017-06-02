(defsystem :cl-threadpool
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-threadpool"  
  :description "Implementation of a thread pool"
  :long-description "Implementation of a thread pool"
  :depends-on (:bordeaux-threads
	       :queues.simple-cqueue
	       :verbose)
  :components ((:module "src/threadpool"
			:serial t
			:components ((:file "packages")
				     (:file "threadpool")))))
