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
  (cl-mongo:docs (cl-mongo:db.find (key "object")
                                   kv
                                   :limit 0)))

(defun save-slot-value (_id slot value)
  (let ((doc (find-doc-by-id _id)))
    (cl-mongo:add-element slot value doc)
    (cl-mongo:db.save (key "object") doc)))

#|

(defclass foo ()
  ((a :initarg :a :initform 123))
  (:index t)
  (:metaclass persistent-class))

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
|#
