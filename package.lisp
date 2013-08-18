(info.read-eval-print.series-ext:sdefpackage
 :info.read-eval-print.nando
 (:use :cl :anaphora)
 (:local-nicknames (:m :info.read-eval-print.mongo)
                   (:b :info.read-eval-print.bson))
 (:import-from :info.read-eval-print.mongo #:*default-connection*)
 (:export #:*db-name*
          #:*collection-name*
          #:*db-host*
          #:*db-port*

          #:*default-connection*
          #:connect
          #:with-connection
          #:with-transaction
          #:clear-strage

          #:persistent-class

          #:_id
          #:create-instance
          #:load-object
          #:save

          #:nando-error
          #:concurrent-modify-error
          #:transaction-required-error))
