(in-package #:montezuma)


(defclass mmap-directory (fs-directory)
  ())

(defclass mmap-index-input (fs-index-input)
  ()
  (:default-initargs :buffer-size 0))


(defmethod open-input ((self mmap-directory) file)
  (make-instance 'mmap-index-input :path (full-path-for-file self file)))


(defmethod initialize-instance :after ((self mmap-index-input) &key)
  (with-slots (buffer buffer-size buffer-length file size) self
    (let* ((fd (osicat-sys::get-stream-fd file)))
      (setf buffer-size size)
      (setf buffer-length size)
      (setf buffer (osicat-posix:mmap (cffi-sys:null-pointer)
                                      buffer-size
                                      (logior osicat-posix:prot-read)
                                      (logior osicat-posix:map-private)
                                      fd
                                      0))
      (cl:close file))))

(defmethod read-byte ((self mmap-index-input))
  (with-slots (buffer buffer-position) self
    (prog1 (cffi:mem-ref buffer :unsigned-char buffer-position)
      (incf buffer-position))))

(defmethod read-bytes ((self buffered-index-input) buffer offset length)
  (with-slots (buffer-length) self
    (dotimes (i length)
      (setf (aref buffer (+ i offset)) (read-byte self))))
  buffer)

(defmethod close ((self mmap-index-input))
  (with-slots (buffer buffer-size) self
    (osicat-posix:munmap buffer buffer-size)))
