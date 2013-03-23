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


(defclass not-index-foo ()
  ()
  (:metaclass persistent-class))

(deftest test-not-index-foo ()
  (with-connection ()
    (clear-strage)
    (make-instance 'not-index-foo)
     (is (eql '() (collect (scan* 'not-index-foo))))))


(defclass foo ()
  ((a :initarg :a :accessor a :index t)
   (b :initarg :b :accessor b :index string)
   (c :initarg :c :accessor c :initform 0))
  (:index t)
  (:metaclass persistent-class))

(deftest test-foo ()
  (with-connection ()
    (clear-strage)
    (let* ((x (make-instance 'foo :a 123 :b "こんにちは"))
           (_id (_id x))
           (loaded (load-object _id)))
      (is (= 123 (a loaded)))
      (is (string= "こんにちは" (b loaded))))))

(deftest test-foo-setf ()
  (with-connection ()
    (clear-strage)
    (let ((foo (make-instance 'foo :a 1 :b "a")))
      (setf (a foo) 100)
      (setf (b foo) "まみむめも♪")
      (let ((loaded (load-object (_id foo))))
        (is (= 100 (a loaded)))
        (is (string= "まみむめも♪" (b loaded)))))
    (is (= 1 (collect-length (scan* 'foo))))))


(deftest test-where-= ()
  (with-connection ()
    (clear-strage)
    (collect-ignore (make-instance 'foo :a (scan-range :length 10)))
    (let ((x (collect-first (scan* 'foo :where '(= a 7)))))
      (is (= 7 (a x))))))

(deftest test-were-=-string ()
  (with-connection ()
    (clear-strage)
    (collect-ignore (make-instance 'foo :b (format nil "あい~a" (scan-range :length 10))))
    (let ((x (collect-first (scan* 'foo :where '(= b "あい7")))))
      (is (string= "あい7" (b x))))))

(deftest test-where-in ()
  (with-connection ()
    (clear-strage)
    (collect-ignore (make-instance 'foo :a (scan-range :length 10)))
    (is (= 4 (collect-length (scan* 'foo :where '(in a 2 3 7 9)))))
    (iterate ((x (scan* 'foo :where '(in a 2 3 7 9))))
      (is (member (a x) '(2 3 7 9))))))

(deftest test-where-> ()
  (with-connection ()
    (clear-strage)
    (collect-ignore (make-instance 'foo :a (scan-range :length 10)))
    (is (= 2 (collect-length (scan* 'foo :where '(> a 7)))))
    (iterate ((x (scan* 'foo :where '(> a 8))))
      (is (= (a x) 9)))))

(deftest test-where-/= ()
  (with-connection ()
    (clear-strage)
    (make-instance 'foo :a 10)
    (make-instance 'foo :a 5)
    (let ((foos (collect (scan* 'foo :where '(/= a 10)))))
      (is (= 1 (length foos)))
      (is (= 5 (a (car foos)))))))

(deftest test-proxy ()
  (with-connection ()
    (clear-strage)
    (let ((foo (load-object (_id (make-instance 'foo :a (make-instance 'foo :a :inner))))))
      (is (eq :inner (a (a foo)))))))

(deftest test-skip-limit ()
  (with-connection ()
    (clear-strage)
    (collect-ignore (make-instance 'foo :a (scan-range :from 6 :upto 10)))
    (collect-ignore (make-instance 'foo :a (scan-range :from 1 :upto 5)))
    (is (= 3 (collect-length (scan* 'foo :skip 4 :limit 3 :sort 'a))))
    (iterate ((foo (scan* 'foo :skip 1 :limit 3 :sort 'a))
              (a (scan-range :from 2 :length 3)))
      (is (= a (a foo))))))

(deftest test-complex-sort ()
  (with-connection ()
    (clear-strage)
    (let ((bs (collect (scan-range :from 1 :upto 10))))
      (collect-ignore (make-instance 'foo :a 10.1 :b (scan bs)))
      (collect-ignore (make-instance 'foo :a 9.99 :b (scan bs)))
      (iterate ((foo (scan* 'foo :sort '(a b :desc)))
                (a (catenate (subseries (series 9.99) 0 10) (subseries (series 10.1) 0 10)))
                (b (scan '(10 9 8 7 6 5 4 3 2 1 10 9 8 7 6 5 4 3 2 1))))
        (is (= a (a foo)))
        (is (= b (b foo)))))))


(info.read-eval-print.nando.test)
