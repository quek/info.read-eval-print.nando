(in-package :info.read-eval-print.nando)

(defconstant +hash-table+ '+hash-table+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SERIALIZE/DESERIALIZE/SCAN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric serialize (object)
  (:documentation "Writes a serialized version of an object to the
stream in a serializer."))

(defmethod serialize (object)
  (with-output-to-string (out)
    (with-standard-io-syntax
      (prin1 object out))))

(defmethod serialize ((hash-table hash-table))
  (serialize `(,+hash-table+
               ,(hash-table-test hash-table)
               ,(hash-table-size hash-table)
               ,(hash-table-rehash-size hash-table)
               ,(hash-table-rehash-threshold hash-table)
               ,@(collect-append (multiple-value-bind (k v)
                                       (scan-hash hash-table)
                                     (list k v))))))

;; TODO dot list
(defmethod serialize ((list list))
  (with-output-to-string (out)
    (with-standard-io-syntax
      (format out "(~{~a~^ ~})" (mapcar #'serialize list)))))

(defmethod serialize ((x integer))
  x)

(defmethod serialize ((x float))
  x)

(defmethod serialize ((oid b:object-id))
  oid)

(defmethod deserialize (serialized)
  "Reads the next object from the serialized string."
  (with-standard-io-syntax
    (let ((x (read-from-string serialized)))
      (if (consp x)
          (deserialize-object (car x) x)
          x))))

(defmethod deserialize ((x integer))
  x)

(defmethod deserialize ((x float))
  x)

(defgeneric deserialize-object (tag data))

(defmethod deserialize-object (tag data)
  data)

(defmethod deserialize-object ((tag (eql +hash-table+)) data)
  (let* ((test (nth 1 data))
         (size (nth 2 data))
         (rehash-size (nth 3 data))
         (rehash-threshold (nth 4 data))
         (hash (make-hash-table :test test
                                :size size
                                :rehash-size rehash-size
                                :rehash-threshold rehash-threshold)))
    (loop for (k v) on (nthcdr 5 data) by #'cddr
          do (setf (gethash k hash) v))
    hash))
