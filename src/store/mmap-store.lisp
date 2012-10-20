(in-package #:montezuma)


(defclass mmap-directory (fs-directory)
  ())

(defclass mmap-index-input (index-input)
  ((addr)
   (size :reader size :type fixnum)
   (position :initform 0 :type fixnum)
   (is-clone-p :initform nil)))


(defmethod open-input ((self mmap-directory) file)
  (make-instance 'mmap-index-input :path (full-path-for-file self file)))


(defmethod initialize-instance :after ((self mmap-index-input) &key path)
  (with-slots (addr size) self
    (setf size (osicat-posix:stat-size (osicat-posix:stat path)))
    (let ((fd (osicat-posix:open path osicat-posix:o-rdonly)))
      (unwind-protect
           (setf addr (osicat-posix:mmap (cffi-sys:null-pointer)
                                         size
                                         (logior osicat-posix:prot-read)
                                         (logior osicat-posix:map-private)
                                         fd
                                         0)))
      (osicat-posix:close fd))))

(defmethod initialize-copy :after ((self mmap-index-input) other)
  (declare (ignore other))
  (with-slots (is-clone-p) self
    (setf is-clone-p t)))


(defmethod read-byte ((self mmap-index-input))
  (declare (cl:optimize (speed 3) (safety 0)))
  (with-slots (addr position) self
    (prog1 (cffi:mem-ref addr :unsigned-char position)
      (incf (the fixnum position)))))

(defmethod read-bytes ((self mmap-index-input) buffer offset length)
  (declare (fixnum offset length))
  (with-slots (addr position) self
    (cffi-sys:with-pointer-to-vector-data (dest buffer)
      (osicat-posix:memcpy
       (cffi-sys:inc-pointer dest offset)
       (cffi-sys:inc-pointer addr position)
       length))
    (incf position length))
  buffer)

(defmethod seek ((self mmap-index-input) pos)
  (declare (fixnum pos))
  (with-slots (position) self
    (setf position pos)))

(defmethod close ((self mmap-index-input))
  (with-slots (addr size is-clone-p) self
    (unless is-clone-p
     (osicat-posix:munmap addr size))))

(defmethod size ((self mmap-index-input))
  (with-slots (size) self
    size))

(defmethod pos ((self mmap-index-input))
  (with-slots (position) self
    position))

(defmethod build-slice ((self mmap-index-input) offset length)
  (let ((clone (clone self)))
    (with-slots (addr size position) clone
      (cffi:incf-pointer addr offset)
      (setf size length)
      (setf position 0)
      clone)))
