(load "init-ql")
(asdf:load-system "cl-threadpool-test" :force t)
(in-package :cl-threadpool-test)
(format t "~%Running tests...~%")
(setf lisp-unit:*print-failures* t)
;;(use-debugger)
(run-tests)

