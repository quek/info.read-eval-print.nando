(in-package :info.read-eval-print.nando)

(defgeneric root (root key)
  (:documentation "get a value of the key from the root."))

(defgeneric (setf root) (value root key)
  (:documentation "set the value of the key to the root."))

(defgeneric delete-root (root key)
  (:documentation "delete a value of key from the root."))

(defgeneric save-root (root)
  (:documentation "save root to file."))

(defgeneric load-root (root)
  (:documentation "load root from root."))


(defclass root ()
  ((path :initarg :path)
   (heap :initarg :heap)
   (root-table :initform (make-hash-table :test #'equal)
               :documentation "value is address of heap")
   (dirty-p :initform nil)))


(defmethod root ((root root) key)
  (with-slots (heap root-table) root
    (let ((address (gethash key root-table)))
      (when address
        (let ((heap-block (read-block heap address)))
          (deserialize heap-block))))))

(defmethod (setf root) (value (root root) key)
  (with-slots (heap root-table dirty-p) root
    (delete-root root key)              ; delete old value
    (let ((buffer (make-adjustable-buffer)))
      (serialize value buffer)
      (let ((address (write-block heap buffer)))
        (setf (gethash key root-table) address)
        (setf dirty-p t)
        value))))

(defmethod delete-root ((root root) key)
  (with-slots (heap root-table dirty-p) root
    (let ((address (gethash key root-table)))
      (when address
        (free heap address)
        (remhash key root-table)
        (setf dirty-p t)))))


(defmethod save-root ((root root))
  (with-slots (path root-table dirty-p) root
    (when dirty-p
      (with-open-file (stream path
                              :direction :output
                              :element-type 'octet
                              :if-exists :supersede)
        (serialize root-table stream))
      (setf dirty-p nil))))

(defmethod load-root ((root root))
  (with-slots (path root-table) root
    (with-open-file (stream path
                            :direction :input :element-type 'octet)
      (setf root-table (deserialize stream)))))
