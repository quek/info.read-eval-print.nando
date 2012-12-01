(in-package :info.read-eval-print.nando)

(defmethod info.read-eval-print.series-ext::scan% ((class persistent-class) &key)
  (let ((object-ids (load-class-index class)))
    (lambda ()
      (if object-ids
          (values (load-object (pop object-ids)) t)
          (values nil nil)))))
