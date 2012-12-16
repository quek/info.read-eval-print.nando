(in-package :info.read-eval-print.nando)

(defmethod info.read-eval-print.series-ext::scan% ((class persistent-class)
                                                   &key where
                                                     offset
                                                     (limit 0)
                                                     order)
  (info.read-eval-print.series-ext::scan%
   (make-find-query :collection (key "object")
                    :query (apply #'cl-mongo:kv
                                  (cl-mongo:kv (symbol-to-key +class+) (serialize (class-name class)))
                                  (let ((kv (compute-where
                                             (aand (car where)
                                                   ( intern (symbol-name it) :keyword))
                                             (cdr where))))
                                    (if kv
                                        (list kv)
                                        nil)))
                    :limit limit)))

(defgeneric compute-where (op args))

(defmethod compute-where ((op null) args)
  nil)

(defmethod compute-where ((op (eql :=)) args)
  (destructuring-bind (slot-name value) args
    (cl-mongo:kv (symbol-to-key slot-name) (serialize value))))

(defmethod compute-where ((op (eql :in)) args)
  (destructuring-bind (slot-name . values) args
    (cl-mongo:kv (symbol-to-key slot-name)
                 (cl-mongo:kv "$in" (mapcar #'serialize values)))))

(macrolet ((m (op-key op)
             `(defmethod compute-where ((op (eql ,op-key)) args)
                (destructuring-bind (slot-name value) args
                  (cl-mongo:kv (symbol-to-key slot-name)
                               (cl-mongo:kv ,op (serialize value)))))))
  (m :< "$lt")
  (m :> "$gt")
  (m :<= "$lte")
  (m :>= "$gte")
  (m :!= "$ne"))

