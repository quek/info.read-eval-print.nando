(in-package :info.read-eval-print.nando)

(defvar *mongo* nil "MongoDB database instance")
(defvar *object-collection* nil "MongoDB collection instance")

(defvar *connection* nil "MongoDB connection instance")

(defparameter *db-name* "nando" "MongoDB database name")
(defparameter *collection-name* "nando" "MonogoDB collection name")
(defparameter *mongo-node* "localhost:27017")

(alexandria:define-constant +lock-key+ "__lock_key__" :test #'equal)

(defun symbol-to-key (symbol)
  (substitute #\space #\. (serialize symbol)))

(defmacro with-connection ((&key (db *db-name*)
                                 (mongo-node *mongo-node*))
                           &body body)
  `(m:with-connection (*connection* ,mongo-node)
     (let* ((*mongo* (m:db *connection* ,db))
            (*object-collection* (m:collection *mongo* "object")))
       ,@body)))

(defun key (&rest args)
  (format nil "~{~a~^:~}" args))

(defun clear-strage (&optional (*object-collection* *object-collection*))
  (m:delete *object-collection* nil))

(defun save-object-data (_id &rest fields)
  (let* ((doc (b:bson :_id _id)))
    (iterate (((k v) (scan-plist fields)))
      (setf (b:value doc k) v))
    (m:insert *object-collection* doc)))

(defun update-object-data (_id &rest fields)
  (let ((query (b:bson :_id _id))
        (update (b:bson (apply #'m:$set fields))))
    (m:update *object-collection* query update)))

(defun lock-object-data (_id)
  (m:find-and-modify *object-collection*
                     (b:bson :_id _id (m:$exists +lock-key+ nil))
                     :update (b:bson (m:$set +lock-key+ t))))

(defun unlock-object-data (_id)
  (m:find-and-modify *object-collection*
                     (b:bson :_id _id (m:$exists +lock-key+ t))
                     :update (b:bson (m:$unset +lock-key+))))


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
