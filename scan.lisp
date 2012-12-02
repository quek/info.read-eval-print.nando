(in-package :info.read-eval-print.nando)

(defmethod info.read-eval-print.series-ext::scan% ((class persistent-class)
                                                   &key where
                                                     offset
                                                     limit
                                                     order)
  (let ((object-ids
          (if where
              (where-to-function (class-name class)
                                 (intern (princ-to-string (car where)) :keyword)
                                 (cdr where))
              (load-class-index class))))
    (lambda ()
      (let ((object-id (pop object-ids)))
        (if object-id
            (values (load-object object-id) t)
            (values nil nil))))))


(defgeneric where-to-function (class-name op args))

(defmethod where-to-function (class-name (op (eql :=)) args)
  (destructuring-bind (slot-name value) args
    (find-slot-index class-name slot-name value)))

(defmethod where-to-function (class-name (op (eql :in)) args)
  (destructuring-bind (slot-name &rest values) args
    (collect-append (find-slot-index class-name slot-name (scan values)))))