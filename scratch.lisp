(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :info.read-eval-print.nando))

(info.read-eval-print.series-ext:sdefpackage
 :info.read-eval-print.nando.scrach
 (:use :cl :anaphora :info.read-eval-print.nando))

(in-package :info.read-eval-print.nando.scrach)



(with-connection ()
  (clear-strage))

(defclass foo ()
  ((a :initarg :a)
   (b :initarg :b))
  (:index t)
  (:metaclass persistent-class))

(with-connection ()
  (save (make-instance 'foo)))
;;⇒ #<FOO ObjectId("521080E4BB34623C5B000059")>


(with-connection ()
  (collect (scan* 'foo)))
;;⇒ (#<FOO ObjectId("52108474BB34623C5B00005A")>)



(connect)
;;⇒ #<INFO.READ-EVAL-PRINT.MONGO:COLLECTION {100F67FC83}>
(save (make-instance 'foo))
;;⇒ #<FOO ObjectId("52108F1EBB34623C5B0000B3")>
(collect (scan* 'foo))
;;⇒ (#<FOO ObjectId("52108F1EBB34623C5B0000B3")>)



(collect (with-finding foo
           (limit 3)
           (offset 2)
           (order 'a)
           (order 'b :desc)
           (where (> 'a 1))))
