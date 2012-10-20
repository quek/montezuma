(in-package #:montezuma)

(let ((default-buffer-size 16))
  (defclass term-buffer ()
    ((buffer :initform (make-array default-buffer-size :element-type '(unsigned-byte 8)))
     (position :initform 0)
     (buffer-size :initform default-buffer-size)
     (field :initform nil :reader field)
     (term :initform nil)
     (text-cache :initform nil))))

(defmethod print-object ((self term-buffer) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (with-slots (field buffer position) self
      (format stream "field:~S text:~S"
	      field
              (text self)))))

(defmethod initialize-copy :after ((self term-buffer) other)
  (set-from-term-buffer self other))


(defgeneric text (term-buffer))

(defmethod text ((self term-buffer))
  (with-slots (text-cache buffer position) self
    (if text-cache
	text-cache
	(setf text-cache (bytes-to-string buffer :start 0 :end position)))))

(defmethod text-length ((self term-buffer))
  (length (text self)))

(defgeneric read-term-buffer (term-buffer input field-infos))

(defmethod read-term-buffer ((self term-buffer) input field-infos)
  (with-slots (term buffer position text-cache field) self
    (setf term nil)
    (let* ((start (read-vint input))
	   (length (read-vint input))
	   (total-length (+ start length)))
      (ensure-text-buf-length self total-length)
      (read-chars input buffer start length)
      (setf position total-length)
      (setf field (field-name (get-field field-infos (read-vint input)))))
    (setf text-cache nil))
  self)

(defgeneric ensure-text-buf-length (term-buffer len))

(defmethod ensure-text-buf-length ((self term-buffer) len)
  (with-slots (buffer position buffer-size text-cache) self
    (unless (>= buffer-size len)
      (let* ((new-size (+ len 10))
             (new-buf (make-array new-size :element-type '(unsigned-byte 8))))
	(replace new-buf buffer :start2 0 :end2 position)
	(setf text-cache nil)
	(setf buffer new-buf)
        (setf buffer-size new-size)))))

(defmethod reset ((self term-buffer))
  (with-slots (field position text-cache term) self
    (setf field nil
          position 0
	  text-cache nil
	  term nil)))

(defgeneric (setf term) (term term-buffer))
(defmethod (setf term) (term (self term-buffer))
  (if (null term)
      (progn (reset self) nil)
      (with-slots (buffer buffer-size position text-cache field) self
        (setf buffer (string-to-bytes (term-text term)))
        (setf buffer-size (length buffer))
        (setf position buffer-size)
        (setf text-cache nil)
	(setf field (term-field term))
	(setf (slot-value self 'term) term))))


(defgeneric to-term (term-buffer))

(defmethod to-term ((self term-buffer))
  (with-slots (field term text-buf text-length) self
    (if (null field)
	nil
	(if (not (null term))
	    term
	    (setf term (make-term field (text self)))))))

(defmethod term ((self term-buffer))
  (to-term self))

(defun term-buffer-compare (tb1 tb2)
  (declare (cl:optimize (speed 3) (safety 0)))
  (let ((f1 (field tb1))
	(f2 (field tb2)))
    (if (string= f1 f2)
        (let ((buffer1 (slot-value tb1 'buffer))
              (buffer2 (slot-value tb2 'buffer))
              (len1 (slot-value tb1 'position))
              (len2 (slot-value tb2 'position)))
          (declare (type (simple-array (unsigned-byte 8) (*)) buffer1 buffer2)
                   (type fixnum len1 len2))
          (loop for a fixnum across buffer1
                for b fixnum across buffer2
                repeat (min len1 len2)
                do (cond ((< a b)
                          (return-from term-buffer-compare -1))
                         ((> a b)
                          (return-from term-buffer-compare 1))))
          (cond ((= len1 len2) 0)
                ((< len1 len2) -1)
                (t 1)))
        (string-compare f1 f2))))

(defun term-buffer> (tb1 tb2)
  (= (term-buffer-compare tb1 tb2) 1))

(defun term-buffer< (tb1 tb2)
  (= (term-buffer-compare tb1 tb2) -1))

(defun term-buffer= (tb1 tb2)
  (zerop (term-buffer-compare tb1 tb2)))

(defgeneric set-from-term-buffer (term-buffer other))
(defmethod set-from-term-buffer ((self term-buffer) other)
  (with-slots (buffer buffer-size position text-cache field term) self
    (setf buffer-size (slot-value other 'buffer-size))
    (setf position (slot-value other 'position))
    (setf text-cache nil)
    (when (slot-value other 'buffer)
      (setf buffer (clone (slot-value other 'buffer))))
    (setf field (slot-value other 'field))
    (setf term (slot-value other 'term))))


(defmethod term-compare ((t1 term) (t2 term-buffer))
  (term-compare t1 (to-term t2)))

(defmethod term= ((t1 term) (t2 term-buffer))
  (and (string= (term-field t1) (field t2))
       (string= (term-text t1) (text t2))))
