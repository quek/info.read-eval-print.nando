;; $Id: objects.lisp,v 1.22 2008-02-19 22:44:06 alemmens Exp $

(in-package :info.read-eval-print.nando)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent objects API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Conventions:
;;  Persistent equivalents of CL functions always have a "p-" prefix.

(defgeneric _id (object)
  (:documentation "Returns the object id of a persistent-object or
persistent-data."))

(defgeneric p-eql (x y)
  (:documentation "The persistent equivalent of EQL."))

#|
persistent-object
persistent-data
  persistent-cons
  persistent-array

p-cons
p-car
p-cdr
(setf p-car)
(setf p-cdr)
p-list

p-make-array
p-aref
(setf p-aref)
p-array-dimensions

p-length
p-find
p-replace
p-position
|#



(defmethod p-eql (a b)
  ;; Default method.
  (eql a b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Proxy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass proxy ()
  ((_id :initarg :_id :reader _id))
  (:documentation "Proxies are some kind of in-memory forwarding pointer
to data in the cache.  They are never saved on disk."))

(defparameter *dont-dereference-proxies* nil)

(defmethod maybe-dereference-proxy ((proxy proxy))
  (if *dont-dereference-proxies*
      proxy
      (load-object (_id proxy))))

(defmethod maybe-dereference-proxy (object)
  ;; Default: just return the object.
  object)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low level persistent data structures.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass persistent-data ()
  ((_id :initarg :_id :reader _id)
   (contents :initarg :contents :accessor contents))
  (:documentation
 "PERSISTENT-DATA classes do not have PERSISTENT-CLASS as metaclass
because we don't want to specialize SLOT-VALUE-USING-CLASS & friends
for persistent-data instances.  Their contents are accessed by special
functions like P-CAR instead."))

(defmethod print-object ((object persistent-data) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "#~D" (slot-value object '_id))))

(defmethod compute-persistent-slot-names ((class standard-class)
                                          (object persistent-data))
  ;; Tell the schema table that instances of persistent-data have
  ;; one persistent slot: the CONTENTS slot.
  '(contents))


(defmethod p-eql ((a persistent-data) (b persistent-data))
  (= (_id a) (_id b)))

(defmethod persistent-data-read (function (data persistent-data) &rest args)
  (let ((value (apply function (contents data) args)))
    (if (typep value 'proxy)
        (maybe-dereference-proxy value)
      value)))

(defmethod persistent-data-write (function (data persistent-data) value
                                           &rest args)
  (apply function value (contents data) args)
  ;; (cache-touch-object data (cache data))
  value)

(defun make-persistent-data (class contents
                             &optional (rucksack (current-rucksack)))
  (let ((object (make-instance class :contents contents)))
    (let ((_id (cache-create-object object)))
      (setf (slot-value object '_id) _id))
    object))



;;
;; Array
;;

(defclass persistent-array (persistent-data)
  ())

(defun p-make-array (dimensions &rest options &key &allow-other-keys)
  (let ((contents (apply #'make-array dimensions options)))
    (make-persistent-data 'persistent-array contents)))

(defmethod p-aref ((array persistent-array) &rest indices)
  (apply #'persistent-data-read #'aref array indices))

(defmethod (setf p-aref) (new-value (array persistent-array) &rest indices)
  (persistent-data-write (lambda (new-value contents)
                           (setf (apply #'aref contents indices) new-value))
                         array
                         new-value))

(defmethod p-array-dimensions ((array persistent-array))
  (persistent-data-read #'array-dimensions array))

;; DO: Other array functions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Basics
;;

(defclass persistent-cons (persistent-data)
  ())

(defun p-cons (car cdr)
  (make-persistent-data 'persistent-cons (cons car cdr)))

(defmethod p-car ((cons persistent-cons))
  (persistent-data-read #'car cons))

(defmethod p-car ((x (eql nil)))
  nil)

(defmethod (setf p-car) (value (cons persistent-cons))
  (persistent-data-write (lambda (new-value contents)
                           (setf (car contents) new-value))
                         cons
                         value))

(defmethod p-cdr ((cons persistent-cons))
  (persistent-data-read #'cdr cons))

(defmethod p-cdr ((x (eql nil)))
  nil)

(defmethod (setf p-cdr) (value (cons persistent-cons))
  (persistent-data-write (lambda (new-value contents)
                           (setf (cdr contents) new-value))
                         cons
                         value))

(defun p-list (&rest objects)
  (if (endp objects)
      objects
    (p-cons (car objects)
            (apply #'p-list (cdr objects)))))

(defun unwrap-persistent-list (list)
  "Converts a persistent list to a 'normal' Lisp list."
  (loop until (p-endp list)
        collect (p-car list)
        do (setq list (p-cdr list))))

;;
;; Other functions from chapter 14 of the spec.
;;

(defun p-caar (object)
  "The persistent equivalent of CAAR."
  (p-car (p-car object)))

(defun p-cadr (object)
  "The persistent equivalenet of CADR."
  (p-car (p-cdr object)))

(defun p-cdar (object)
  "The persistent equivalent of CDAR."
  (p-cdr (p-car object)))

(defun p-cddr (object)
  "The persistent equivalent of CDDR."
  (p-cdr (p-cdr object)))


(defmethod p-consp ((object persistent-cons))
  t)

(defmethod p-consp ((object t))
  nil)


(defmethod p-endp ((object (eql nil)))
  t)

(defmethod p-endp ((object persistent-cons))
  nil)

(defmethod p-endp ((object t))
  (error 'type-error
         :datum object
         :expected-type '(or null persistent-cons)))


(defun p-last (list &optional (n 1))
  "Returns the last persistent cons cell of a persistent list (or
NIL if the list is empty)."
  (unless (= n 1)
    ;; DO: Implement this case.
    (error "The optional argument for P-LAST isn't implemented yet."))
  (let ((result list)
        (tail (p-cdr list)))
    (loop until (p-endp tail)
          do (shiftf result tail (p-cdr tail)))
    result))


(defun p-mapcar (function list)
  ;; DO: Accept more than one list argument.
  (let ((result '()))
    (loop while list do
          (setq result (p-cons (funcall function (p-car list))
                               result)
                list (p-cdr list)))
    (p-nreverse result)))

(defun p-mapc (function list)
  ;; DO: Accept more than one list argument.
  (let ((tail list))
    (loop while tail do
          (funcall function (p-car tail))
          (setq tail (p-cdr tail)))
    list))

(defun p-maplist (function list)
  ;; DO: Accept more than one list argument.
  (let ((result '()))
    (loop while list do
          (setq result (p-cons (funcall function list) result)
                list (p-cdr list)))
    (p-nreverse result)))

(defun p-mapl (function list)
  ;; DO: Accept more than one list argument.
  (let ((tail list))
    (loop while tail do
          (funcall function tail)
          (setq tail (p-cdr tail)))
    list))
  
(defun p-member-if (predicate list &key key)
  (unless key
    (setq key #'identity))
  (p-mapl (lambda (tail)
            (when (funcall predicate (funcall key (p-car tail)))
              (return-from p-member-if tail)))
          list)
  nil)


(defmacro p-pop (place &environment env)
  "Pop an item from the persistent list specified by PLACE."
  (multiple-value-bind (dummies vals new setter getter)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list dummies vals) (,(car new) ,getter))
       (prog1 (p-car ,(car new))
         (setq ,(car new) (p-cdr ,(car new)))
         ,setter))))


(defmacro p-push (item place &environment env)
  "Push ITEM onto the persistent list specified by PLACE.  Return the
modified persistent list. ITEM is evaluated before place."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (let ((item-var (gensym "ITEM")))
      `(let* ((,item-var ,item)
              ,@(mapcar #'list dummies vals)
              (,(car newval) (p-cons ,item-var ,getter)))
         ,setter))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent sequence functions
;; (Just a start...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-p-vector (persistent-array function-name)
  (unless (= 1 (length (p-array-dimensions persistent-array)))
    (error "~S expected a persistent vector instead of ~S."
           function-name
           persistent-array)))

(defmethod p-length ((vector persistent-array))
  (check-p-vector vector 'p-length)
  (first (p-array-dimensions vector)))

(defmethod p-length ((list persistent-cons))
  ;; DO: Check for circular lists.
  (let ((result 0))
    (p-mapc (lambda (pair)
              (declare (ignore pair))
              (incf result))
            list)
    result))

(defmethod p-find (value (vector persistent-array)
                         &key (key #'identity) (test #'p-eql)
                         (start 0) (end nil))
  (check-p-vector vector 'p-find)
  (loop for i from start below (or end (p-length vector))
        do (let ((elt (funcall key (p-aref vector i))))
             (when (funcall test value elt)
               (return-from p-find (p-aref vector i)))))
  ;; Return nil if not found
  nil)

(defmethod p-find (value (list persistent-cons)
                         &key (key #'identity) (test #'p-eql)
                         (start 0) (end nil))
  ;; Move list to start position.
  (loop repeat start
        do (setq list (p-cdr list)))
  ;; The real work.
  (loop for i from start do
        (if (or (p-endp list) (and end (= i end)))
            (return-from p-find nil)
          (let ((elt (funcall key (p-car list))))
            (if (funcall test value elt)
                (return-from p-find (p-car list))
              (setq list (p-cdr list))))))
  ;; Return nil if not found.
  nil)

(defmethod p-find (value (list (eql nil)) &key &allow-other-keys)
  nil)

(defmethod p-position (value (vector persistent-array)
                             &key (key #'identity) (test #'p-eql)
                             (start 0) (end nil))
  (check-p-vector vector 'p-position)
  (loop for i from start below (or end (p-length vector))
        do (let ((elt (funcall key (p-aref vector i))))
             (when (funcall test value elt)
               (return-from p-position i))))
  ;; Return nil if not found
  nil)


(defmethod p-position (value (list persistent-cons)
                             &key (key #'identity) (test #'p-eql)
                             (start 0) (end nil))
  ;; Move list to start position.
  (loop repeat start
        do (setq list (p-cdr list)))
  ;; The real work.
  (loop for i from start do
        (if (or (p-endp list) (and end (= i end)))
            (return-from p-position nil)
          (let ((elt (funcall key (p-car list))))
            (if (funcall test value elt)
                (return-from p-position i)
              (setq list (p-cdr list))))))
  ;; Return nil if not found.
  nil)


(defmethod p-replace ((vector-1 persistent-array)
                      (vector-2 persistent-array)
                      &key (start1 0) end1 (start2 0) end2)
  ;; We don't need to look at the cached sequence elements,
  ;; so we can just use CL:REPLACE on the vector contents and bypass
  ;; the p-aref calls.
  (replace (contents vector-1) (contents vector-2)
           :start1 start1
           :end1 end1
           :start2 start2
           :end2 end2)
  ;; Touch the vector because it has changed.
  (cache-touch-object vector-1 (cache vector-1))
  vector-1)


(defmethod p-delete-if (test (list persistent-cons)
                        &key (from-end nil) (start 0) end count key)
  ;; DO: Implement FROM-END.
  ;; DO: Write tests.
  (declare (ignore from-end))
  (unless key
    (setq key #'identity))
  ;; Move list to start position.
  (let ((tail list)
        (prev nil))
    (loop repeat start
          do (setq prev tail
                   tail (p-cdr tail)))
    ;; The real work.
    (let ((nr-deleted 0))
      (loop for i from start do
            (if (or (p-endp tail)
                    (and end (= i end))
                    (and count (>= nr-deleted count)))
                (return-from p-delete-if list)
              (if (funcall test (funcall key (p-car tail)))
                  ;; Delete the element.
                  (progn
                    (if prev
                        (setf (p-cdr prev) (p-cdr tail))
                      (setq list (p-cdr tail)))
                    ;; Keep count.
                    (incf nr-deleted))
                ;; Don't delete anything.
                (setq prev tail)))
            ;; Keep moving.
            (setq tail (p-cdr tail)))))
  ;; Return the (possibly modified) list.
  list)


(defmethod p-nreverse ((object (eql nil)))
  nil)

(defmethod p-nreverse ((object persistent-cons))
  (let* ((previous object)
         (current (p-cdr previous)))
    (setf (p-cdr previous) '())
    (loop until (p-endp current)
          do (let ((next (p-cdr current)))
               (setf (p-cdr current) previous
                     previous current
                     current next)))
    previous))

;; DO: Implement P-NREVERSE for persistent vectors.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Serializing/deserializing cached data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod serialize ((object persistent-data))
  ;; When the serializer meets a persistent-data object, it only needs to save
  ;; the object id.  The cache will make sure that the cached object is saved
  ;; elsewhere.
  (_id object))

(defmethod serialize ((object proxy))
  ;; Proxies are serialized like the cached objects they stand for.
  (_id object))

(defmethod deserialize ((oid b:object-id))
  (make-instance 'proxy :_id oid))
