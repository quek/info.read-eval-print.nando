(in-package :info.read-eval-print.nando)

(defvar *connection*)
(defvar *mongo*)
(defvar *object-collection*)

(defparameter *db* "cl")
(defparameter *db-host* "localhost")
(defparameter *db-port* 27017)

(defun symbol-to-key (symbol)
  (substitute #\space #\. (serialize symbol)))

(defmacro with-connection ((&key (db *db*)
                              (host *db-host*)
                              (port *db-port*))
                           &body body)
  `(m:with-connection (*connection* :host ,host :port ,port)
     (let* ((*mongo* (m:db *connection* ,db))
            (*object-collection* (m:collection *mongo* "object")))
       ,@body)))

(defun key (&rest args)
  (format nil "~{~a~^:~}" args))

(defun clear-strage (&optional (*object-collection* *object-collection*))
  (m:delete *object-collection* nil))

(defun save-object-data (&rest fields)
  (let ((doc (b:bson :_id (b:object-id))))
    (iterate (((k v) (scan-plist fields)))
      (setf (b:value doc k) v))
    (m:insert *object-collection* doc)
    (b:value doc :_id)))

(defun find-doc-by-id (_id &key (error t))
  (let ((doc (m:find-one *object-collection* (b:bson :_id _id))))
    (cond (doc doc)
          (error
           (error "Not found. _id: ~a" _id))
          (t nil))))

(defun find-object (kv)
  (m:find-all *object-collection* kv))

(defun save-slot-value (_id slot value)
  (m:update *object-collection*
            (b:bson :_id _id)
            (b:bson (m:$set slot value))))
