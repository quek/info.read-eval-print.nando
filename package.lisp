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
          #:create-instance
          #:load-object
          #:save-object

          #:nando-error
          #:concurrent-modify-error
          #:transaction-required-error))
