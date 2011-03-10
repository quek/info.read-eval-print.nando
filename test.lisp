(macrolet ((m ()
             `(defpackage :info.read-eval-print.nando.test
                (:use :cl :anaphora :series
                      :info.read-eval-print.nando)
                (:shadowing-import-from :series ,@series::/series-forms/)
                (:import-from :info.read-eval-print.nando
                              ,@(loop for symbol being the present-symbols
                                        in :info.read-eval-print.nando
                                      unless (member symbol series::/series-forms/)
                                        collect (symbol-name symbol))))))
  (m))

(series::install :pkg :info.read-eval-print.nando.test :implicit-map t)

(in-package :info.read-eval-print.nando.test)

(defmacro with-test-db ((&optional (path #p"/tmp/nando.test/")) &body body)
  `(progn
     (open-db ,path)
     (unwind-protect (progn ,@body)
       (close-db))))

(let ((path #p"/tmp/nando.test/"))
  (with-test-db ()
    (setf (value 'key1) 'value1)
    (setf (value "key2") "value2"))
  (with-test-db ()
    (assert (eq 'value1 (value 'key1)))
    (assert (string= "value2" (value "key2")))))
