(in-package :info.read-eval-print.nando)


(defparameter *db* "cl")
(defparameter *db-host* cl-mongo:*mongo-default-host*)
(defparameter *db-port* cl-mongo:*mongo-default-port*)

(defun symbol-to-key (symbol)
  (substitute #\space #\. (serialize symbol)))

(defmacro with-connection ((&key (db *db*)
                              (host *db-host*)
                              (port *db-port*))
                           &body body)
  `(cl-mongo:with-mongo-connection (:db ,db :host ,host :port ,port)
     ,@body))

(defun key (&rest args)
  (format nil "~{~a~^:~}" args))

(defun clear-strage (&optional (*db* *db*))
  (cl-mongo:rm "object" :all))

(defun save-object-data (&rest fields)
  (let ((doc (cl-mongo:make-document)))
    (iterate (((k v) (scan-plist fields)))
      (cl-mongo:add-element k v doc))
    (cl-mongo:db.insert (key "object") doc)
    (cl-mongo::_id doc)))

(defun find-doc-by-id (_id &key (error t))
  (let ((docs (cl-mongo:docs (cl-mongo:db.find (key "object") (cl-mongo:kv "_id" _id)))))
    (cond (docs
           (car docs))
          (error
           (error "Not found. _id: ~a" _id))
          (t nil))))

(defun find-object (kv)
  (cl-mongo:docs
   (cl-mongo:iter
    (cl-mongo:db.find (key "object") kv :limit 0))))

(defun save-slot-value (_id slot value)
  (let ((doc (find-doc-by-id _id)))
    (cl-mongo:add-element slot value doc)
    (cl-mongo:db.save (key "object") doc)))


(defclass query ()
  ((collection :initarg :collection :reader collection-of)
   (query :initarg :query :reader query-of)
   (skip :initarg :skip :initform 0 :reader skip-of)
   (limit :initarg :limit :initform 0 :reader limit-of)
   (order :initarg :order)))

(defmethod order-of ((query query))
  (let ((order (slot-value query 'order)))
    (cond ((null order)
           nil)
          ((atom order)
           (cl-mongo:kv (symbol-to-key order) 1))
          (t
           (apply #'cl-mongo:kv
                  (mapcar (lambda (x)
                            (if (atom x)
                                (cl-mongo:kv (symbol-to-key x) 1)
                                (cl-mongo:kv (symbol-to-key (car x))
                                             (if (eq :desc (cadr x)) -1 1))))
                          order))))))

(defmethod info.read-eval-print.series-ext::scan% ((query query) &key)
  (let* ((limit (limit-of query))
         (count 0)
         (order (order-of query))
         (result (cl-mongo:db.find
                  (collection-of query)
                  (if order
                      (cl-mongo:kv (cl-mongo:kv "query" (query-of query))
                                   (cl-mongo:kv "orderby" order))
                      (query-of query))
                  :skip (skip-of query)
                  :limit limit)))
    (lambda ()
      (labels ((f ()
                 (let ((doc (pop (cadr result))))
                   (cond (doc
                          (values (load-object doc) t))
                         ((progn
                            (push nil (cadr result))
                            (zerop (cl-mongo::db.iterator result)))
                          (end))
                         (t
                          (if (zerop limit)
                              (progn
                                (setf result (cl-mongo:db.iter result :limit 0))
                                (pop (cadr result))
                                (f))
                              (progn
                                (incf count (nth 7 (car result)))
                                (if (<= limit count)
                                    (end)
                                    (progn
                                      (setf result (cl-mongo:db.iter result :limit (- limit count)))
                                      (pop (cadr result))
                                      (f)))))))))
               (end ()
                 (cl-mongo:db.stop result)
                 (values nil nil)))
        (f)))))



#|

(defclass foo ()
  ((a :initarg :a :initform 123)
   (b :initform "あいうあいうあいうあいうあいうあいうあいうあいうあいうあいうあいうあいう"))
  (:index t)
  (:metaclass persistent-class))

(with-connection ()
  ;;(clear-strage)
  ;;(dotimes (i 80001) (make-instance 'foo))
  (collect-length (scan* (make-instance 'query :collection (key "object") :query :all :limit 777777))))




(with-connection ()
  (clear-strage))

(with-connection ()
  (let ((id (_id (make-instance 'foo))))
    (find-doc-by-id id)))

(with-connection ()
  (cl-mongo:docs (cl-mongo:db.find (key "object")
                                   (cl-mongo:kv (cl-mongo:kv "INFO READ-EVAL-PRINT NANDO::+CLASS+"
                                                             "INFO.READ-EVAL-PRINT.NANDO.TEST::FOO")
                                                (cl-mongo:kv "INFO READ-EVAL-PRINT NANDO TEST::A" 7)))))

(with-connection ()
  (multiple-value-bind (iterator collection docs)
      (cl-mongo::db.iterator (cl-mongo:db.find (key "object")
                                               :all))
    (values iterator collection (length docs))))
|#
