(in-package :info.read-eval-print.nando)

(defmethod info.read-eval-print.series-ext::scan% ((class persistent-class)
                                                   &key where
                                                     (skip 0)
                                                     (limit 0)
                                                     sort)
  (let* ((query (let ((query (b:bson (symbol-to-key +class+) (serialize (class-name class)))))
                  (let ((kv (compute-where (aand (car where)
                                                 (intern (symbol-name it) :keyword))
                                           (cdr where))))
                    (if kv
                        (b:merge-bson query kv)
                        query))))
         (sort (mapcar (lambda (x)
                         (if (eq :desc x)
                             x
                             (substitute #\space #\. (serialize x))))
                       (alexandria:ensure-list sort)))
         (generator (generator (m:scan-mongo *object-collection*
                                             query
                                             :skip skip
                                             :limit limit
                                             :sort sort))))
    (lambda ()
      (let ((bson (next-in generator nil)))
        (if bson
            (values (load-object bson) t)
            (values nil nil))))))

(defgeneric compute-where (op args))

(defmethod compute-where ((op null) args)
  nil)

(defmethod compute-where ((op (eql :=)) args)
  (destructuring-bind (slot-name value) args
    (b:bson (symbol-to-key slot-name) (serialize value))))

(defmethod compute-where ((op (eql :in)) args)
  (destructuring-bind (slot-name . values) args
    (b:bson (apply #'m:$in (symbol-to-key slot-name)
                   (mapcar #'serialize values)))))

(macrolet ((m (op-key op)
             `(defmethod compute-where ((op (eql ,op-key)) args)
                (destructuring-bind (slot-name value) args
                  (b:bson (,op (symbol-to-key slot-name) (serialize value)))))))
  (m :< m:$<)
  (m :> m:$>)
  (m :<= m:$<=)
  (m :>= m:$>=)
  (m :!= m:$not))

