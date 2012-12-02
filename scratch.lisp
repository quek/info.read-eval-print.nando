(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :info.read-eval-print.nando))

(info.read-eval-print.series-ext:sdefpackage
 :info.read-eval-print.nando.scranch
 (:use :cl :anaphora :info.read-eval-print.nando))

(in-package :info.read-eval-print.nando.scranch)

(defclass foo ()
  ((a :initarg :a)
   (b :initarg :b))
  (:index t)
  (:metaclass persistent-class))

(ignore-errors (redis:connect))

(collect (scan* 'foo :where `(= a 1)))
(collect (scan* 'foo :where `(in a 1 10)))
(collect (scan* 'foo :where `(< a 10)))
(collect (scan* 'foo :where `(> a 10)))
(collect (scan* 'foo :where `(=~ a ".*hello.*")))

