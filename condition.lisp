(in-package :info.read-eval-print.nando)

(define-condition nando-error (error)
  ())

(define-condition concurrent-modify-error (nando-error)
  ())

(define-condition transaction-required-error (nando-error)
  ())
