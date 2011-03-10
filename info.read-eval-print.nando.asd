;;;; info.read-eval-print.nando.asd

(asdf:defsystem #:info.read-eval-print.nando
  :serial t
  :components ((:file "package")
               (:file "type")
               (:file "util")
               (:file "thread")
               (:file "serialize")
               (:file "heap")
               (:file "root")
               (:file "nando"))
  :depends-on (#:bordeaux-threads
               #:anaphora
               #:series))

