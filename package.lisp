;;;; package.lisp

(macrolet ((m ()
             `(defpackage :info.read-eval-print.nando
                (:use :cl :anaphora :series)
                (:shadowing-import-from :series ,@series::/series-forms/)
                (:export :value
                         :open-db
                         :close-db))))
  (m))

(series::install :pkg :info.read-eval-print.nando :implicit-map t)
