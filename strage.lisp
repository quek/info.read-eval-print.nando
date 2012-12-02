(in-package :info.read-eval-print.nando)

(defparameter *key-prefix* "cl:")

(defun clear-strage ()
  (iterate ((key (scan (redis:red-keys (key "*")))))
    (redis:red-del key)))

(defun key (key)
  (with-standard-io-syntax
    (concatenate 'string *key-prefix* (princ-to-string key))))

;; (redis:connect)

(defun new-object-id ()
  (red:incr (key "*object-id*")))

(defun save-object-data (object-id &rest fields)
  (apply #'red:hmset (key object-id)
         fields))

(defun load-object-data (object-id)
  (let ((key (key object-id)))
    (redis:red-hgetall key)))

(defun add-class-index (class object-id)
  (redis:red-sadd (key (class-name class)) object-id ))

(defun load-class-index (class)
  (redis:red-smembers (key (class-name class))))

(defun save-slot-value (object-id slot value)
  (redis:red-hset (key object-id) slot value ))

(defun slot-index-key (class-name slot-name)
  (with-standard-io-syntax
    (key (concatenate 'string (princ-to-string class-name) ":" (princ-to-string slot-name)))))

(defun add-slot-index (object-id class-name slot-name slot-value)
  (redis:red-zadd (slot-index-key class-name slot-name) slot-value object-id))

(defun delete-slot-index (object-id class-name slot-name)
  (redis:red-zrem (slot-index-key class-name slot-name) object-id))

(defun find-slot-index (class-name slot-name min &optional (max min))
  (redis:red-zrangebyscore (slot-index-key class-name slot-name) min max))


#|
(redis:connect)
;;⇒ #<REDIS:REDIS-CONNECTION {100D676B53}>

(defclass foo ()
  ())
(redis:red-set 1 1)
;;⇒ "OK"
(redis:red-get 1)
;;⇒ "1"

;;⇒ "1"
(redis:red-set "1" "a")
;;⇒ "OK"
(redis:red-get "1")
;;⇒ "a"

;;⇒ "1"
(symbol-name '|a b|)
;;⇒ "a b"
(find-symbol "a b")
;;⇒ |a b|
;;   :INTERNAL

(redis:red-hmset "foox" "s1" "a" "s2" "b")
;;⇒ "OK"
(redis:red-hkeys "foox")
;;⇒ ("s1" "s2")
(redis:red-hmget "foox" "s1" "s2")
;;⇒ ("a" "b")

(with-standard-io-syntax
  (princ "hello"))
;;→ hello
;;⇒ "hello"
(format nil "~s" "hollo")
;;⇒ "\"hollo\""
(with-output-to-string (*standard-output*) (prin1 "hello"))
;;⇒ "\"hello\""
(with-output-to-string (*standard-output*) (prin1 1))
;;⇒ "1"
|#
