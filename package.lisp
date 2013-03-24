(info.read-eval-print.series-ext:sdefpackage
 :info.read-eval-print.nando
 (:use :cl :anaphora)
 (:local-nicknames (:m :info.read-eval-print.mongo)
                   (:b :info.read-eval-print.bson))
 (:export #:*connection*
          #:with-connection
          #:with-transaction
          #:clear-strage

          #:persistent-class

          #:_id
          #:load-object))
