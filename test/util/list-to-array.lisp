(in-package :cl-threadpool-test)


(defun list-to-array (l)
  (let ((a (make-array (length l))) (index 0))
    (dolist (item l)
      (setf (elt a index) item)
      (setf index (+ 1 index)))
    a))

    
