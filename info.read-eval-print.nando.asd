;;;; info.read-eval-print.nando.asd

(asdf:defsystem :info.read-eval-print.nando
  :serial t
  :components ((:file "package")
               (:file "type")
               (:file "util")
               (:file "thread")
               (:file "serialize")
               (:file "heap")
               (:file "root")
               (:file "mop")
               (:file "objects")
               (:file "p-btrees")
               (:file "nando"))
  :depends-on (:bordeaux-threads
               :anaphora
               :series
               :closer-mop))

