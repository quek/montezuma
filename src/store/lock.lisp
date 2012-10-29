(in-package #:montezuma)

(defconstant +lock-poll-interval+ 1)
(defconstant +lock-obtain-wait-forever+ -1)

(defclass lock ()
  ())

(defmethod obtain ((lock lock) &optional (lock-wait-timeout 0))
  (loop with max-sleep-count = (/ lock-wait-timeout +lock-poll-interval+)
        for sleep-count from 0
        for locked = (%obtain lock)
        if locked
          do (return-from obtain t)
        if (and (/= lock-wait-timeout +lock-obtain-wait-forever+)
                (>= sleep-count max-sleep-count))
          do (error "Lock obtain timed out.")
        do (sleep +lock-poll-interval+)))

(defclass simple-fs-lock (lock)
  ((lock-dir :initarg :lock-dir)
   (lock-file)))

(defmethod initialize-instance :after ((lock simple-fs-lock) &key lock-dir lock-name)
  (setf (slot-value lock 'lock-file)
        (merge-pathnames lock-name lock-dir)))

(defmethod %obtain ((lock simple-fs-lock))
  (with-slots (lock-dir lock-file) lock
    (unless (ensure-directories-exist lock-dir)
      (error "Cannot create directory: ~a" lock-dir))
    (with-open-file (x lock-file :direction :output :if-exists nil :if-does-not-exist :create)
      x)))

(defmethod release ((lock simple-fs-lock))
  (with-slots (lock-file) lock
    (cl:delete-file lock-file)))


(defclass ram-lock (lock)
  ((lock :initform (bt:make-lock))))

(defmethod %obtain ((self ram-lock))
  (with-slots (lock) self
    (bt:acquire-lock lock nil)))

(defmethod release ((self ram-lock))
  (with-slots (lock) self
    (bt:release-lock lock)))