(load "init-ql")
(asdf:load-system :cl-threadpool/doc)
(cl-threadpool-make-doc::make-doc)
