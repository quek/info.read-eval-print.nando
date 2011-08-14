(in-package :info.read-eval-print.nando)

(defvar *nando* nil "default nando")

(defun value (key &optional (*nando* *nando*))
  (nando-value *nando* key))

(defun (setf value) (value key &optional (*nando* *nando*))
  (setf (nando-value *nando* key) value))

(defun open-db (directory &key (default t))
  (aprog1 (make-instance 'nando :directory directory)
    (open-nando it)
    (when default
      (setf *nando* it))))

(defun close-db (&optional (*nando* *nando*))
  (close-nando *nando*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric nando-value (nando key))

(defgeneric (setf nando-value) (value nando key))

(defgeneric open-nando (nando))

(defgeneric close-nando (nando))


(defclass nando ()
  ((directory :initarg :directory)
   (root)))


(defmethod initialize-instance :after ((nando nando) &rest args)
  (declare (ignore args))
  (with-slots (directory root) nando
    (ensure-directories-exist directory)
    (setf root (make-instance 'root
                              :path (merge-pathnames "root" directory)
                              :heap (make-instance 'heap
                                                   :path (merge-pathnames "root-heap" directory))))))

(defmethod open-nando ((nando nando))
  (with-slots (root) nando
    (with-slots (path heap) root
      (heap-open heap)
      (when (probe-file path)
        (load-root root)))))

(defmethod close-nando ((nando nando))
  (with-slots (root) nando
    (save-root root)
    (with-slots (heap) root
      (heap-close heap))))

(defmethod nando-value ((nando nando) key)
  (with-slots (root) nando
    (root root key)))

(defmethod (setf nando-value) (value (nando nando) key)
  (with-slots (root) nando
    (setf (root root key) value)))
