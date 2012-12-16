(in-package :info.read-eval-print.nando)

(defmethod info.read-eval-print.series-ext::scan% ((class persistent-class)
                                                   &key where
                                                     offset
                                                     limit
                                                     order)
  (let ((docs (find-object (apply #'cl-mongo:kv
                                  (cl-mongo:kv (symbol-to-key +class+) (serialize (class-name class)))
                                  (let ((kv (compute-where (car where) (cdr where))))
                                    (if kv
                                        (list kv)
                                        nil))))))
    (lambda ()
      (let ((doc (pop docs)))
        (if doc
            (values (load-object doc) t)
            (values nil nil))))))

(defgeneric compute-where (op args))

(defmethod compute-where ((op null) args)
  nil)

(defmethod compute-where ((op (eql '=)) args)
  (destructuring-bind (slot-name value) args
    (cl-mongo:kv (symbol-to-key slot-name) (serialize value))))


