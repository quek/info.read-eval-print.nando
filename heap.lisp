(in-package :info.read-eval-print.nando)

(defgeneric heap-open (heap))

(defgeneric heap-close (heap))

(defgeneric alloc (heap size))

(defgeneric free (heap address))

(defgeneric write-block (heap heap-block))

(defconstant +pointer-size+ 8)
(defconstant +min-block-size+ 16)
(defconstant +free-block-class-count+ 32)

(defstruct heap-stream
  stream
  (lock (bordeaux-threads:make-lock))
  (buffer (make-buffer +pointer-size+)))

(defmacro with-heap-stream-lock ((heap-stream) &body body)
  `(bordeaux-threads:with-lock-held ((heap-stream-lock ,heap-stream))
     ,@body))

(defun read-pointer (heap-stream address)
  (with-heap-stream-lock (heap-stream)
    (file-position (heap-stream-stream heap-stream) address)
    (read-sequence (heap-stream-buffer heap-stream)
                   (heap-stream-stream heap-stream))
    (vector-to-unsigned-byte (heap-stream-buffer heap-stream))))

(defun write-pointer (heap-stream address value)
  (with-heap-stream-lock (heap-stream)
    (unsigned-byte-into-vector value
                               (heap-stream-buffer heap-stream)
                               +pointer-size+)
    (file-position (heap-stream-stream heap-stream) address)
    (write-sequence (heap-stream-buffer heap-stream)
                    (heap-stream-stream heap-stream))))

(defun read-seq (heap-stream address buffer size)
  (with-heap-stream-lock (heap-stream)
    (file-position (heap-stream-stream heap-stream)
                   address)
    (read-sequence buffer
                   (heap-stream-stream heap-stream)
                   :end size)))

(defun write-seq (heap-stream address buffer size)
  (with-heap-stream-lock (heap-stream)
    (file-position (heap-stream-stream heap-stream)
                   address)
    (write-sequence buffer
                    (heap-stream-stream heap-stream)
                    :end size)))

(defclass heap ()
  ((lock        :initform (bordeaux-threads:make-lock))
   (path        :initarg :path)
   (stream      :initform nil)
   (end         :initform #.(* 8 (1+ +free-block-class-count+)))
   (free-blocks :initform (make-array +free-block-class-count+
                                      :element-type 'fixnum
                                      :initial-element 0))
   (cell-buffer :initform (make-buffer +pointer-size+)))
  (:documentation "
0   heap end
8   class 0:  16 byte free block
16  class 1:  32 byte free block
32  class 2:  64 byte free block
264 class 31: 34359738368 byte free block

使用中のブロックの - +pointer-size+ には size-class が設定されている。
未使用のブロックの - +pointer-size+ には、次の未使用のブロックのアドレスが設定されている。
"))

(defstruct heap-block
  address
  buffer
  (position 0))

(defmethod serialize-byte (byte (stream heap-block))
  (setf (aref (heap-block-buffer stream) (heap-block-position stream))
        byte)
  (incf (heap-block-position stream)))

(defmethod deserialize-byte ((stream heap-block) &optional (eof-error-p t))
  (if (< (heap-block-position stream) (length (heap-block-buffer stream)))
      (prog1 (aref (heap-block-buffer stream)
                   (heap-block-position stream))
        (incf (heap-block-position stream)))
      (and eof-error-p
           (error "Unexpected end of serialization buffer at ~D."
                  (heap-block-position stream)))))


(defmacro with-heap-lock ((heap) &body body)
  `(bordeaux-threads:with-lock-held ((slot-value ,heap 'lock))
     ,@body))

(defmethod heap-open ((heap heap))
  (with-slots (stream path end free-blocks) heap
    (setf stream (make-heap-stream
                  :stream (open path
                                :direction :io
                                :element-type 'octet
                                :if-exists :overwrite
                                :if-does-not-exist :create)))
    (if (zerop (file-length (heap-stream-stream stream)))
        (progn
          (setf end (+ +pointer-size+ (* +pointer-size+ +free-block-class-count+)))
          (write-pointer stream 0 end)
          (iterate ((size-class (scan-range :below +free-block-class-count+)))
            (setf (free-block heap size-class) 0)))
        (progn
          (setf end (read-pointer stream 0))
          (iterate ((size-class (scan-range :below +free-block-class-count+)))
            (setf (aref free-blocks size-class)
                  (read-pointer stream (free-block-address size-class))))))))

(defmethod heap-close ((heap heap))
  (with-slots (stream) heap
    (close (heap-stream-stream stream))))

(defmethod free-block ((heap heap) size-class)
  (with-slots (free-blocks) heap
    (aref free-blocks size-class)))

(defmethod (setf free-block) (address (heap heap) size-class)
  (with-slots (stream free-blocks) heap
    (write-pointer stream
                   (free-block-address size-class)
                   address)
    (setf (aref free-blocks size-class) address)))

(defun free-block-address (size-class)
  (+ +pointer-size+                     ; heap end
     (* +pointer-size+ (1+ size-class))))

(defun size-class (size)
  (integer-length (ash (1- size)
                       #.(- 1 (integer-length +min-block-size+)))))

(defun size-class-block-size (size-class)
  (* +min-block-size+ (ash 1 size-class)))

(defun expansion-size (size-class)
  (size-class-block-size size-class))

(defun block-header (address)
  (- address +pointer-size+))

(defun initialize-block (heap address size-class)
  (with-slots (stream) heap
    (let ((header (block-header address)))
      (write-pointer stream header size-class)))
  address)


(defun find-free-block (heap size-class)
  (with-slots (stream) heap
    (let ((address (free-block heap size-class)))
      (unless (zerop address)
        (let ((next-address (read-pointer stream (block-header address))))
          (setf (free-block heap size-class) next-address))
        (initialize-block heap address size-class)))))

(defun expand-heap (heap size-class)
  (with-slots (end stream) heap
    (let ((old-end end)
          (expansion-size (expansion-size size-class)))
      (incf end expansion-size)
      (write-pointer stream 0 end)
      ;; +pointer-size+ はブロックヘッダ
      (initialize-block heap (+ +pointer-size+ old-end) size-class))))


(defun alloc-address (heap size)
  (with-heap-lock (heap)
    (with-slots (stream end) heap
      ;; ブロックヘッダのために +pointer-size+ 多く確保する。
      (let ((size-class (size-class (+ size +pointer-size+))))
        (or (find-free-block heap size-class)
            (expand-heap heap size-class))))))

(defmethod alloc ((heap heap) size)
  (make-heap-block :address (alloc-address heap size)
                   :buffer (make-buffer size)))

(defmethod free ((heap heap) (heap-block heap-block))
  (free heap (heap-block-address heap-block)))

(defmethod free ((heap heap) address)
  (with-heap-lock (heap)
    (with-slots (stream) heap
      (let* ((size-class (read-pointer stream (block-header address)))
             (free-address (free-block heap size-class)))
        (write-pointer stream (block-header address)
                       free-address)
        (setf (free-block heap size-class) address)))))

(defmethod read-block ((heap heap) address)
  (with-slots (stream) heap
   (let* ((size-class (read-pointer stream (block-header address)))
          (size (- (size-class-block-size size-class)
                   +pointer-size+))     ; ブロックヘッダ分引く
          (buffer (make-buffer size)))
     (read-seq stream address buffer size)
     (make-heap-block :address address
                      :buffer buffer))))

(defmethod write-block ((heap heap) (heap-block heap-block))
  (with-slots (stream) heap
    (let ((address (heap-block-address heap-block)))
      (write-seq stream
                 address
                 (heap-block-buffer heap-block)
                 (length (heap-block-buffer heap-block)))
      address)))

(defmethod write-block ((heap heap) (buffer vector))
  (with-slots (stream) heap
    (let* ((size (length buffer))
           (address (alloc-address heap size)))
      (write-seq stream
                 address
                 buffer
                 size)
      address)))

#|
(setf heap (make-instance 'heap :path "/tmp/heap"))
(heap-open heap)
(setf b (alloc heap 8))
(dotimes (i (length (heap-block-buffer b)))
  (setf (aref (heap-block-buffer b) i) (mod (* i i) 255)))
(write-block heap b)
(free heap b)
(heap-close heap)
|#
