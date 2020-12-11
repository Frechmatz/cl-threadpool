(defsystem :cl-threadpool-make-doc
  :serial t
  :version "3.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-threadpool"
  :description "Generates the documentation of cl-threadpool"
  :long-description "Generates the documentation of cl-threadpool"
  :depends-on (:cl-threadpool :cl-html-readme :docparser)
  :components ((:module "examples"
			:serial t
			:components ((:file "example-1")
				     (:file "example-2")))
	       (:module "make-doc"
			:serial t
			:components ((:file "packages")
				     (:file "make-doc")))))

