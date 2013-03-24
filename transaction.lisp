(in-package :info.read-eval-print.nando)

(defvar *transaction* nil)

(defgeneric commit (transaction))
(defgeneric rollback (transaction))
(defgeneric add-dirty-object (transaction object))

(defclass transaction ()
  ((commitable-errors :initarg :commitable-errors :initform nil)
   (dirty-objects :initarg :dirty-objects :initform nil)))


(defmacro with-transaction ((&key commitable-errors) &body body)
  (alexandria:with-gensyms (result)
    `(let ((*transaction* (make-instance 'transaction :commitable-errors ,commitable-errors))
           (,result ',result))
       (unwind-protect
            (setf ,result (progn ,@body))
         (if (eq ,result ',result)
             (rollback *transaction*)
             (commit *transaction*))))))


(defmethod commit ((transaction transaction))
  (with-slots (dirty-objects) transaction
    (let ((xs (reverse dirty-objects)))
      (mapc #'save-object xs)
      (mapc #'unlock-object xs))))

(defmethod rollback ((transaction transaction))
  (with-slots (dirty-objects) transaction
    (let ((xs (reverse dirty-objects)))
      (mapc #'reload-object xs)
      (mapc #'unlock-object xs))))

(defmethod add-dirty-object ((transaction transaction) object)
  (lock-object object)
  (with-slots (dirty-objects) transaction
    (pushnew object dirty-objects)))
