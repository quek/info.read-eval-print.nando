(info.read-eval-print.series-ext:sdefpackage
 :info.read-eval-print.nando
 (:use :cl :anaphora)
 (:export #:*connection*
          #:with-connection
          #:clear-strage

          #:persistent-class

          #:_id
          #:load-object))

