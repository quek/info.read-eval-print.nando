(in-package :info.read-eval-print.nando)

(defmethod info.read-eval-print.series-ext::scan% ((class persistent-class)
                                                   &key where
                                                     (skip 0)
                                                     (limit 0)
                                                     sort
                                                     for-update)
  (when (and for-update (null *transaction*))
    (error 'transaction-required-error))
  (let* ((query (let ((query (b:bson +class+ (serialize (class-name class)))))
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
         (generator (generator
                     (m:scan-mongo *object-collection*
                                   query
                                   :skip skip
                                   :limit limit
                                   :sort sort
                                   :projection (if for-update
                                                   '(:_id)
                                                   nil)))))
    (lambda ()
      (let ((bson (next-in generator nil)))
        (if bson
            (values (if for-update
                        (%load-object-for-update (b:value bson :_id))
                        (load-object bson))
                    t)
            (values nil nil))))))

(defun %load-object-for-update (id)
  (unless (lock-object-data id)
    (error 'concurrent-modify-error))
  (let ((object (load-object (find-doc-by-id id))))
    (add-dirty-object *transaction* object)
    object))

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
  (m :/= m:$ne))

(defmethod compute-where ((op (eql :and)) args)
  (apply #'b:merge-bson
         (mapcar (lambda (where)
                     (compute-where (aif (car where) (intern (symbol-name it) :keyword))
                                    (cdr where)))
                 args)))
