(in-package :info.read-eval-print.nando)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Full fledged persistent objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass persistent-object ()
  ((_id :initarg :_id :reader _id
        :persistence nil :index nil)
   (dirty :initform nil :reader dirty-p :persistence nil)
   (new :initform t :reader new-p :persistence nil))
  (:metaclass persistent-class)
  (:documentation "Classes of metaclass PERSISTENT-CLASS automatically
inherit from this class."))


(defparameter *initializing-instance*
  ;; A hack to paper over some MOP differences.  Maybe a cleaner way
  ;; to solve this would be to write our own method for SHARED-INITIALIZE,
  ;; as suggested by Pascal Costanza.
  ;; See emails of 2006-09-03/04 on rucksack-devel@common-lisp.net.
  nil)

(defmethod initialize-instance :around ((object persistent-object)
                                        &key)
  (let* ((*initializing-instance* t))
    (call-next-method)))


(alexandria:define-constant +class+ "c" :test #'string=)
(defconstant +unbound+ '+unbound+)

(defun create-instance (class &rest initargs)
  (aprog1 (apply #'make-instance class initargs)
    (save it)))

(defun save (object)
  (if (new-p object)
      (create-object object)
      (update-object object)))

(defun create-object (object)
  (unless (slot-boundp object '_id)
    (setf (slot-value object '_id) (b:object-id)))
  (if *transaction*
      (add-dirty-object *transaction* object)
      (progn
        (setf (slot-value object 'dirty) nil)
        (apply #'save-object-data
               (_id object)
               +class+ (serialize (class-name (class-of object)))
               (%slot-values object))))
  object)

(defun update-object (object)
  (if *transaction*
      (add-dirty-object *transaction* object)
      (progn
        (setf (slot-value object 'dirty) nil)
        (apply #'update-object-data (slot-value object '_id) (%slot-values object))))
  object)

(defun lock-object (object)
  (if (new-p object)
      t
      (lock-object-data (_id object))))

(defun unlock-object (object)
  (if (new-p object)
      t
      (unlock-object-data (_id object))))

(defun %slot-values (object)
  (loop for slot in (c2mop:class-slots (class-of object))
        for slot-name = (c2mop:slot-definition-name slot)
        if (slot-persistence-p slot)
          append (list (symbol-to-key slot-name)
                       (if (slot-boundp object slot-name)
                           (serialize (slot-value object slot-name))
                           #.(serialize +unbound+)))))



(defmethod print-object ((object persistent-object) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~a" (if (slot-boundp object '_id)
                            (slot-value object '_id)
                            ":new"))))


;; It's a bit stupid that we have to write the same code for three
;; P-EQL methods, but we don't seem to have much choice.

(defmethod p-eql ((a persistent-object) (b persistent-object))
  (= (_id a) (_id b)))

(defmethod p-eql ((a persistent-data) (b persistent-object))
  (= (_id a) (_id b)))

(defmethod p-eql ((a persistent-object) (b persistent-data))
  (= (_id a) (_id b)))



(defmethod c2mop:slot-value-using-class :around ((class persistent-class)
                                                 object
                                                 slot)
  (maybe-dereference-proxy (call-next-method)))

(macrolet ((m ()
             `(prog1 (call-next-method)
                (when (slot-persistence-p slot-def)
                  (setf (slot-value object 'dirty) t)))))

  (defmethod (setf c2mop:slot-value-using-class) :around (new-value
                                                          (class persistent-class)
                                                          object
                                                          slot-def)
    (m))


  (defmethod c2mop:slot-makunbound-using-class :around ((class persistent-class)
                                                        object
                                                        slot-def)
    (m)))



(defmethod serialize ((object persistent-object))
  ;; When the serializer meets a persistent object, it only needs to save the
  ;; object id.  The cache will make sure that the object is saved elsewhere.
  (cond  ((new-p object)
          (create-object object))
         ((dirty-p object)
          (update-object object)))
  (_id object))

;;
;; Loading objects
;;
(defmethod load-object (id)
  (load-object (find-doc-by-id id)))

(defmethod load-object ((doc b:bson))
  (let* ((class (find-class (deserialize (b:value doc +class+))))
         (object (allocate-instance class))
         (*initializing-instance* t))
    (setf (slot-value object '_id) (b:value doc :_id))
    (setf (slot-value object 'new) nil)
    (setf (slot-value object 'dirty) nil)
    (%store-slots object doc)))

(defun reload-object (object)
  (setf (slot-value object 'new) nil)
  (setf (slot-value object 'dirty) nil)
  (%store-slots object (find-doc-by-id (_id object))))

(defun %store-slots (object doc)
  (iterate ((slot (scan (c2mop:class-slots (class-of object)))))
    (let ((slot-name (c2mop:slot-definition-name slot)))
      (multiple-value-bind (value ok)
          (b:value doc (substitute #\space #\. (serialize slot-name)))
        (when ok
          (let ((value (deserialize value)))
            (when (not (eq value +unbound+))
              (setf (slot-value object slot-name) value)))))))
  object)
