(in-package :info.read-eval-print.nando)

(defmethod info.read-eval-print.series-ext::scan% ((class persistent-class)
                                                   &key where
                                                     offset
                                                     limit
                                                     order)
  (let ((object-ids
          (if where
              (where-to-function (class-name class)
                                 (car where)
                                 (cdr where))
              (load-class-index class))))
    (lambda ()
      (let ((object-id (pop object-ids)))
        (if object-id
            (values (load-object object-id) t)
            (values nil nil))))))


(defgeneric where-to-function (class-name op args))

(defmethod where-to-function (class-name (op (eql '=)) args)
  (destructuring-bind (slot-name value) args
    (let ((slot (collect-first
                 (choose-if (lambda (x)
                              (eq slot-name (c2mop:slot-definition-name x)))
                            (scan (c2mop:class-slots (find-class class-name)))))))
      (find-slot-index class-name slot-name (slot-index slot)  value))))

(defmethod where-to-function (class-name (op (eql :in)) args)
  (destructuring-bind (slot-name &rest values) args
    (let ((slot-index (slot-index
                       (collect-first
                        (choose-if (lambda (x)
                                     (eq slot-name (c2mop:slot-definition-name x)))
                                   (scan (c2mop:class-slots (find-class class-name))))))))
      (collect-append (find-slot-index class-name slot-name slot-index (scan values))))))