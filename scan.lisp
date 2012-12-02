(in-package :info.read-eval-print.nando)

(defmethod info.read-eval-print.series-ext::scan% ((class persistent-class) &key where)
  (let ((object-ids (load-class-index class))
        (filter (where-to-function (intern (princ-to-string (car where)) :keyword) (cdr where))))
    (lambda ()
      (prog (object-id object)
       loop
         (setf object-id (pop object-ids))
         (unless object-id
           (return (values nil nil)))
         (setf object (load-object object-id))
         (when (funcall filter object)
           (return (values object t)))
         (go loop)))))


(defgeneric where-to-function (op args))

(defmethod where-to-function ((op (eql :nil)) args)
  (constantly t))

(defmethod where-to-function ((op (eql :=)) args)
  (destructuring-bind (slot value) args
    (lambda (x)
      (= (slot-value x slot) value))))

(defmethod where-to-function ((op (eql :in)) args)
  (destructuring-bind (slot &rest values) args
    (lambda (x)
      (member (slot-value x slot) values))))