(in-package :info.read-eval-print.nando)

(defvar *transaction* nil)

(defgeneric commit (transaction))
(defgeneric rollback (transaction))
(defgeneric add-dirty-object (transaction object))

(defclass transaction ()
  ((commitable-errors :initarg :commitable-errors :initform nil)
   (dirty-objects :initarg :dirty-objects :initform nil)
   (rollback-p :initform t :accessor rollback-p)))


(defmacro with-transaction ((&key commitable-errors) &body body)
  (alexandria:with-gensyms (result)
    `(let ((*transaction* (make-instance 'transaction :commitable-errors ',commitable-errors))
           (,result ',result))
       (unwind-protect
            (handler-bind ,(mapcar (lambda (x)
                                     `(,x (lambda (condition)
                                            (declare (ignore condition))
                                            (setf (rollback-p *transaction*) nil))))
                            commitable-errors)
              (setf ,result (progn ,@body)
                    (rollback-p *transaction*) nil))
         (if (rollback-p *transaction*)
             (rollback *transaction*)
             (commit *transaction*)))
       ,result)))

(defmethod commit ((transaction transaction))
  (with-slots (dirty-objects) transaction
    (let ((xs (reverse dirty-objects))
          (*transaction* nil))
      (mapc #'save-object xs)
      (mapc #'unlock-object xs))))

(defmethod rollback ((transaction transaction))
  (with-slots (dirty-objects) transaction
    (let ((xs (reverse dirty-objects)))
      (mapc (lambda (x)
              (unless (new-p x)
                (reload-object x)))
            xs)
      (mapc #'unlock-object xs))))

(defmethod add-dirty-object ((transaction transaction) object)
  (unless (lock-object object)
    (error 'concurrent-modify-error))
  (with-slots (dirty-objects) transaction
    (pushnew object dirty-objects)))
