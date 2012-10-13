(in-package #:montezuma)


(defclass mmap-directory (fs-directory)
  ())

(defclass mmap-index-input (index-input)
  ((addr)
   (size :reader size :type 'fixnum)
   (position :initform 0 :type 'fixnum)))


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

(defmethod read-byte ((self mmap-index-input))
  (with-slots (addr position) self
    (prog1 (cffi:mem-ref addr :unsigned-char position)
      (incf position))))

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
  (with-slots (addr size) self
    (osicat-posix:munmap addr size)))

(defmethod size ((self mmap-index-input))
  (with-slots (size) self
    size))
