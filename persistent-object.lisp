(in-package :info.read-eval-print.nando)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Full fledged persistent objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass persistent-object ()
  ((_id :initarg :_id :reader _id
        :persistence nil :index nil))
  (:metaclass persistent-class)
  (:index nil)
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
  (let* ((*initializing-instance* t)
         (result (call-next-method)))
    (when (class-index (class-of object))
      (if *transaction*
          (add-dirty-object *transaction* object)
          (create-object object)))
    result))

(defmethod update-slot-index (class object slot
                              old-value new-value
                              old-boundp new-boundp)
  "SLOT is a slot-definition, not a slot name."
  (let ((_id (_id object))
        (class-name (class-name class))
        (slot-name (c2mop:slot-definition-name slot))
        (index-type (slot-index slot)))
    (when old-boundp
      (delete-slot-index _id class-name slot-name index-type old-value))
    (when new-boundp
      (add-slot-index _id class-name slot-name index-type new-value))))



(defconstant +class+ '+class+)
(defconstant +unbound+ '+unbound+)

(defun new-object-p (object)
  (not (slot-boundp object '_id)))

(defun save-object (object)
  (if (new-object-p object)
      (create-object object)
      (update-object object)))

(defun create-object (object)
  (setf (slot-value object '_id)
        (apply #'save-object-data
               (symbol-to-key +class+) (serialize (class-name (class-of object)))
               (%slot-values object))))

(defun update-object (object)
  (apply #'update-object-data
         (slot-value object '_id)
         (%slot-values object)))

(defun lock-object (object)
  (if (new-object-p object)
      t
      (lock-object-data (_id object))))

(defun unlock-object (object)
  (if (new-object-p object)
      t
      (unlock-object-data (_id object))))

(defun %slot-values (object)
  (loop for slot in (c2mop:class-slots (class-of object))
        for slot-name = (c2mop:slot-definition-name slot)
        if (slot-persistence slot)
          append (list (symbol-to-key slot-name)
                       (if (slot-boundp object slot-name)
                           (serialize (slot-value object slot-name))
                           #.(serialize +unbound+)))))



(defmethod print-object ((object persistent-object) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~a" (slot-value object '_id))))


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
  (maybe-update-slot-info class)
  ;; Automatically dereference proxies.
  (maybe-dereference-proxy (call-next-method)))


(defmethod (setf c2mop:slot-value-using-class) :around (new-value
                                                        (class persistent-class)
                                                        object
                                                        slot-name-or-def)
  (maybe-update-slot-info class)
  ;; If this is a persistent slot, tell the cache that this object
  ;; has changed. The cache will save it when necessary.
  (multiple-value-bind (slot slot-name) (slot-def-and-name class slot-name-or-def)
    (if (slot-persistence slot)
        (let* ((old-boundp (c2mop:slot-boundp-using-class class object slot-name-or-def))
               (old-value (and old-boundp
                               (c2mop:slot-value-using-class class object slot-name-or-def)))
               (result (call-next-method)))
          (unless *initializing-instance*
            (if *transaction*
                (add-dirty-object *transaction* object)
                (save-slot-value (_id object)
                                 (substitute #\space #\. (serialize slot-name))
                                 (serialize new-value))))
          result)
        (call-next-method))))


(defmethod c2mop:slot-makunbound-using-class :around ((class persistent-class)
                                                      object
                                                      slot-name-or-def)
  (maybe-update-slot-info class)
  ;; If this is a persistent slot, tell the cache that this object
  ;; has changed. Rely on the cache to save it when necessary.
  (let ((slot (slot-def-and-name class slot-name-or-def)))
    (if (and (slot-persistence slot)
             ;; If the RUCKSACK slot isn't bound yet, the object is
             ;; just being loaded from disk and we don't need to
             ;; do anything special.
             (slot-boundp object 'rucksack))
        (let* ((old-boundp (c2mop:slot-boundp-using-class class object slot-name-or-def))
               (old-value
                 (and old-boundp
                      (c2mop:slot-value-using-class class object slot-name-or-def)))
               (result (call-next-method)))
          (cache-touch-object object)
          (rucksack-maybe-index-changed-slot (rucksack object)
                                             class object slot
                                             old-value nil
                                             old-boundp nil)
          result)
        (call-next-method))))


(defun slot-def-and-name (class slot-name-or-def)
  "Returns (1) slot definition and (2) slot name."
  #+lispworks(values (find slot-name-or-def (class-slots class)
                           :key #'slot-definition-name)
                     slot-name-or-def)
  #-lispworks(values slot-name-or-def
                     (c2mop:slot-definition-name slot-name-or-def)))



(defmethod serialize ((object persistent-object))
  ;; When the serializer meets a persistent object, it only needs to save the
  ;; object id.  The cache will make sure that the object is saved elsewhere.
  (_id object))

;;
;; Loading objects
;;
(defmethod load-object (id)
  (load-object (find-doc-by-id id)))

(defmethod load-object ((doc b:bson))
  (let* ((class (find-class (deserialize (b:value
                                          doc
                                          (substitute #\space #\. (serialize +class+))))))
         (object (allocate-instance class))
         (*initializing-instance* t))
    (setf (slot-value object '_id) (b:value doc :_id))
    (%store-slots object doc)))

(defun reload-object (object)
  (%store-slots object (find-doc-by-id (_id object))))

(defun %store-slots (object doc)
  (iterate ((slot (scan (c2mop:class-slots (class-of object)))))
    (let ((slot-name (c2mop:slot-definition-name slot)))
      (multiple-value-bind (value ok)
          (b:value doc (substitute #\space #\. (serialize slot-name)))
        (when ok
          (let ((value (deserialize value)))
            (when (and (not (eq value +class+))
                       (not (eq value +unbound+)))
              (setf (slot-value object slot-name) value)))))))
  object)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Updating persistent instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When a persistent object must be loaded from disk, Rucksack loads the
;; schema nr and finds the corresponding schema.  If the schema is obsolete
;; (i.e. there is a schema for the same class with a higher version number),
;; Rucksack calls the generic function UPDATE-PERSISTENT-INSTANCE-FOR-REDEFINED-CLASS
;; after calling ALLOCATE-INSTANCE for the current class version.  The generic
;; function is very similar to UPDATE-INSTANCE-FOR-REDEFINED-CLASS: it takes a
;; list of added slots, a list of deleted slots and a property list containing
;; the slot names and values for slots that were discarded and had values.

(defgeneric update-persistent-instance-for-redefined-class
    (instance added-slots discarded-slots property-list &key)
  (:method ((instance persistent-object) added-slots discarded-slots plist
            &key)
   ;; Default method: ignore the discarded slots and initialize added slots
   ;; according to their initforms.  We do this 'by hand' and not by calling
   ;; SHARED-INITIALIZE because slot indexes may need to be updated too.
    (let ((slots (c2mop:class-slots (class-of instance))))
     (loop for slot-name in added-slots
           for slot = (find slot-name slots :key #'c2mop:slot-definition-name)
           for initfunction = (and slot
                                   (c2mop:slot-definition-initfunction slot))
           when initfunction
           ;; NOTE: We don't handle initargs, and I think we don't need to.
           ;; We follow the CLHS description of UPDATE-INSTANCE-FOR-REDEFINED-CLASS,
           ;; which says: "When it is called by the system to update an
           ;; instance whose class has been redefined, no initialization
           ;; arguments are provided."
           do (setf (slot-value instance slot-name) (funcall initfunction))))))


(defmethod update-instance-for-redefined-class
           ((object persistent-object) added-slots discarded-slots plist
            &rest initargs &key)
  ;; This method exists for updating in-memory persistent objects
  ;; of which the class definition has changed.
  (declare (ignore initargs)) ; there shouldn't be any, anyway
  (cache-touch-object object)
  (update-persistent-instance-for-redefined-class object added-slots
                                                  discarded-slots plist))
