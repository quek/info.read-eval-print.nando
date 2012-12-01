(in-package :info.read-eval-print.nando)

(defmethod scan% ((class persistent-class) &key)
  (let ((object-ids (load-class-index class)))
    (lambda ()
      (if object-ids
          (values (load-object (pop object-ids)) t)
          (values nil nil)))))
