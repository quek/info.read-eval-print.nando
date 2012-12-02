(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :info.read-eval-print.nando)
  (ql:quickload :hu.dwim.stefil))

(info.read-eval-print.series-ext:sdefpackage
 :info.read-eval-print.nando.test
 (:use :cl :anaphora :info.read-eval-print.nando)
 (:import-from :hu.dwim.stefil #:is #:deftest))

(in-package :info.read-eval-print.nando.test)

(hu.dwim.stefil:defsuite* (info.read-eval-print.nando.test
                           :in hu.dwim.stefil:root-suite))


(defclass foo ()
  ((a :initarg :a :accessor a)
   (b :initarg :b :accessor b))
  (:index t)
  (:metaclass persistent-class))

(deftest test-foo ()
  (with-connection ()
    (clear-strage)
    (let* ((x (make-instance 'foo :a 123 :b "hello"))
           (object-id (object-id x))
           (loaded (load-object object-id)))
      (is (= 123 (a loaded)))
      (is (string= "hello" (b loaded)))

      (setf (b loaded) "こんにちは")
      (setf loaded (load-object object-id))
      (is (string= "こんにちは" (b loaded)))

      (is (= 1 (collect-length (scan* 'foo))))
      (let ((first (collect-first (scan* 'foo))))
        (is (= 123 (a first)))
        (is (string= "こんにちは" (b first))))
      (make-instance 'foo)
      (is (= 2 (collect-length (scan* 'foo)))))))


(defclass not-index-foo ()
  ()
  (:metaclass persistent-class))

(deftest test-not-index-foo ()
  (with-connection ()
    (clear-strage)
    (make-instance 'not-index-foo)
    (is (eql '() (collect (scan* 'not-index-foo))))))


(deftest test-where-= ()
  (with-connection ()
    (clear-strage)
    (collect-ignore (make-instance 'foo :a (scan-range :length 10)))
    (let ((x (collect-first (scan* 'foo :where '(= a 7)))))
      (is (= 7 (a x))))))

(deftest test-where-in ()
  (with-connection ()
    (clear-strage)
    (collect-ignore (make-instance 'foo :a (scan-range :length 10)))
    (assert (= 4 (collect-length (scan* 'foo :where '(in a 2 3 7 9)))))
    (iterate ((x (scan* 'foo :where '(in a 2 3 7 9))))
      (assert (member (a x) '(2 3 7 9))))))



(info.read-eval-print.nando.test)
