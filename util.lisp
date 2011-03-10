(in-package #:info.read-eval-print.nando)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun progs-body (var body)
    (let ((form (cond ((atom (car body))
                       (list (car body) var))
                      ((collect (choose-if (lambda (x) (eq var x))
                                           (scan-lists-of-lists-fringe (cdar body))))
                       (car body))
                      (t
                       (append (car body) (list var))))))
      (if (endp (cdr body))
          form
          `(let ((,var ,form))
             ,(progs-body var (cdr body)))))))

(defmacro progs ((&optional (var (gensym))) &body body)
  `(let ((,var ,(car body)))
     ,(progs-body var (cdr body))))


(defun make-buffer (size &rest args)
  (apply #'make-array size :element-type 'octet args))

(defun make-adjustable-buffer ()
  (make-array 64 :element-type 'octet :adjustable t :fill-pointer 0))

(defun unsigned-byte-into-vector (unsigned-byte buffer size)
  (dotimes (i size)
    (setf (aref buffer i)
          (ldb (byte 8 (* i 8)) unsigned-byte)))
  buffer)

(defun unsigned-byte-to-vector (unsigned-byte size)
  (let ((buffer (make-buffer size)))
    (unsigned-byte-into-vector unsigned-byte buffer size)))

(defun vector-to-unsigned-byte (vector &optional (size (length vector)))
  (let ((n 0))
    (dotimes  (i size)
      (setf n (dpb (aref vector i) (byte 8 (* i 8)) n)))
    n))

