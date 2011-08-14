(in-package :info.read-eval-print.nando)

(defstruct rw-lock
  (mutex (bordeaux-threads:make-lock))
  (read-count 0)
  (write-lock 0)
  (write-thread nil))

(defun read-lock (rw-lock)
  (loop
    (bordeaux-threads:with-lock-held ((rw-lock-mutex rw-lock))
      (unless (rw-lock-write-lock rw-lock)
        (incf (rw-lock-read-count rw-lock))
        (return)))
    (bordeaux-threads:thread-yield)))

(defun write-lock (rw-lock)
  (loop
    (bordeaux-threads:with-lock-held ((rw-lock-mutex rw-lock))
      (unless (and (zerop (rw-lock-read-count rw-lock))
                   (rw-lock-write-lock rw-lock))
        (setf (rw-lock-write-lock rw-lock) t
              (rw-lock-write-thread rw-lock) (bordeaux-threads:current-thread))
        (return)))))

(defun unlock (rw-lock)
  (bordeaux-threads:with-lock-held ((rw-lock-mutex rw-lock))
    (aif (rw-lock-write-lock rw-lock)
         (setf it nil
               (rw-lock-write-thread rw-lock) nil)
         (decf (rw-lock-read-count rw-lock)))))

