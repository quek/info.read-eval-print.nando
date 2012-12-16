(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :info.read-eval-print.nando))

(info.read-eval-print.series-ext:sdefpackage
 :info.read-eval-print.nando.scranch
 (:use :cl :anaphora :info.read-eval-print.nando))

(in-package :info.read-eval-print.nando.scranch)

(with-connection ()
  (clear-strage))

(defclass foo ()
  ((a :initarg :a)
   (b :initarg :b))
  (:index t)
  (:metaclass persistent-class))

(with-connection ()
  (make-instance 'foo))

(with-connection ()
  (collect (scan* 'foo)))
;;⇒ NIL


(collect (with-finding foo
           (limit 3)
           (offset 2)
           (order 'a)
           (order 'b :desc)
           (where (> 'a 1))))
