;;;; package.lisp

(macrolet ((m ()
             `(defpackage :info.read-eval-print.nando
                (:use :cl :anaphora :closer-mop :series)
                (:shadowing-import-from :series ,@series::/series-forms/)
                (:shadowing-import-from :cl
                                        #:defmethod
                                        #:defgeneric
                                        #:standard-generic-function)
                (:export :value
                         :open-db
                         :close-db))))
  (m))

(series::install :pkg :info.read-eval-print.nando :implicit-map t)
