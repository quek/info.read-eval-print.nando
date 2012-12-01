(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :info.read-eval-print.nando))

(info.read-eval-print.series-ext:sdefpackage
 :info.read-eval-print.nando.test
 (:use :cl :anaphora :info.read-eval-print.nando))

(in-package :info.read-eval-print.nando.test)

(defclass foo (persistent-object)
  ((a :initarg :a)
   (b :initarg :b))
  (:index t)
  (:metaclass persistent-class))

(with-connection ()
  (clear-strage)
  (let* ((x (make-instance 'foo :a 123 :b "hello"))
         (object-id (object-id x))
         (loaded (load-object object-id)))
    (assert (= 123 (slot-value loaded 'a)))
    (assert (string= "hello" (slot-value loaded 'b)))
    (assert (= 1 (collect-length (scan* 'foo))))
    (let ((first (collect-first (scan* 'foo))))
      (assert (= 123 (slot-value first 'a)))
      (assert (string= "hello" (slot-value first 'b))))))
;;⇒ NIL

