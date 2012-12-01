(info.read-eval-print.series-ext:sdefpackage
 :info.read-eval-print.nando
 (:use :cl :anaphora)
 (:import-from :hu.dwim.defclass-star #:defclass*)
 (:import-from :redis #:*connection* #:with-connection)
 (:export #:*connection*
          #:with-connection
          #:clear-strage

          #:persistent-class

          #:object-id
          #:load-object))

