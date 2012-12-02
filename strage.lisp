(in-package :info.read-eval-print.nando)

(defparameter *key-prefix* "cl;")

(defun clear-strage ()
  (iterate ((key (scan (redis:red-keys (key "*")))))
    (redis:red-del key)))

(defun key (&rest args)
  (with-standard-io-syntax
    (format nil "~a~{~a~^;~}" *key-prefix* (mapcar (lambda (x)
                                                     (typecase x
                                                       (string x)
                                                       (t (prin1-to-string x))))
                                                   args))))

(defun new-object-id ()
  (red:incr (key ";*object-id*")))

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

(defmethod add-slot-index (object-id class-name slot-name index-type slot-value)
  (redis:red-zadd (key class-name slot-name) slot-value object-id))

(defmethod add-slot-index (object-id class-name slot-name (index-type (eql 'string)) slot-value)
  (redis:red-sadd (key class-name slot-name slot-value) object-id))

(defmethod delete-slot-index (object-id class-name index-type slot-name slot-value)
  (redis:red-zrem (key class-name slot-name) object-id))

(defmethod delete-slot-index (object-id class-name (index-type (eql 'string)) slot-name slot-value)
  (redis:red-zrem (key class-name slot-name slot-value) object-id))

(defmethod find-slot-index (class-name slot-name index-type min &optional (max min))
  (redis:red-zrangebyscore (key class-name slot-name) min max))

(defmethod find-slot-index (class-name slot-name (index-type (eql 'string)) min &optional max)
  (declare (ignore max))
  (redis:red-smembers (key class-name slot-name min)))


#|
(redis:connect)
;;⇒ #<REDIS:REDIS-CONNECTION {100D676B53}>

(redis:red-del 1)
(redis:red-sadd 1 1)
;;⇒ T
(redis:red-exists 1)
;;⇒ T
(redis:red-srem 1 1)
;;⇒ T
(redis:red-exists 1)
;;⇒ NIL


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
