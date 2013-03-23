;;;; info.read-eval-print.nando.asd

(asdf:defsystem :info.read-eval-print.nando
  :serial t
  :components ((:file "package")
               (:file "type")
               (:file "util")
               (:file "thread")
               (:file "serialize")
               (:file "strage")
               (:file "root")
               (:file "mop")
               (:file "objects")
               (:file "persistent-object")
               (:file "scan")
               (:file "nando"))
  :depends-on (:bordeaux-threads
               :anaphora
               :closer-mop
               :info.read-eval-print.mongo
               :info.read-eval-print.series-ext))

