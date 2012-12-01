(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :info.read-eval-print.nando)
  (ql:quickload :hu.dwim.stefil))

(info.read-eval-print.series-ext:sdefpackage
 :info.read-eval-print.nando.test
 (:use :cl :anaphora :info.read-eval-print.nando)
 (:import-from :hu.dwim.stefil #:is))

(in-package :info.read-eval-print.nando.test)

(hu.dwim.stefil:defsuite* (info.read-eval-print.nando.test
                           :in hu.dwim.stefil:root-suite))


(defclass foo (persistent-object)
  ((a :initarg :a)
   (b :initarg :b))
  (:index t)
  (:metaclass persistent-class))

(hu.dwim.stefil:deftest test-foo ()
  (with-connection ()
    (clear-strage)
    (let* ((x (make-instance 'foo :a 123 :b "hello"))
           (object-id (object-id x))
           (loaded (load-object object-id)))
      (is (= 123 (slot-value loaded 'a)))
      (is (string= "hello" (slot-value loaded 'b)))
      (is (= 1 (collect-length (scan* 'foo))))
      (let ((first (collect-first (scan* 'foo))))
        (is (= 123 (slot-value first 'a)))
        (is (string= "hello" (slot-value first 'b))))
      (make-instance 'foo)
      (is (= 2 (collect-length (scan* 'foo)))))))


(info.read-eval-print.nando.test)
