(in-package #:montezuma)

(declaim (inline length-1-list-p))
(defun length-1-list-p (x) 
  "Is x a list of length 1? Note that this is better than the naive \(= \(length x\) 1\) because we don't need to traverse the entire list..."
  (and (consp x) (null (cdr x))))

;;; ---------------------------------------------------------------------------
;;; each method
;;; ---------------------------------------------------------------------------

(defgeneric each (sequence function))

(defmethod each ((self list) block)
  (mapc block self))


(defun parse-float (string)
  (with-standard-io-syntax
    (read-from-string string)))


;;; ---------------------------------------------------------------------------
;;; thread local
;;; ---------------------------------------------------------------------------

(defvar *thread-local* (make-hash-table :synchronized t :weakness :key))

(defun thread-local (key)
  "sb-thread:symbol-value-in-thread がうまく動かないみたい。"
  ;; (getf (sb-thread:symbol-value-in-thread '*thread-local* sb-thread:*current-thread* nil) key)
  (getf (gethash sb-thread:*current-thread* *thread-local*) key))

(defun (setf thread-local) (value key)
  "sb-thread:symbol-value-in-thread がうまく動かないみたい。"
  ;; (setf (getf (sb-thread:symbol-value-in-thread '*thread-local* sb-thread:*current-thread* nil) key) value))
  (setf (getf (gethash sb-thread:*current-thread* *thread-local*) key) value))

#|
(sb-thread:make-thread (lambda ()
                         (print (thread-local :foo))
                         (sleep 10)
                         (setf (thread-local :foo) 111)
                         (print (thread-local :foo))))
(sb-thread:make-thread (lambda ()
                         (print (thread-local :foo))
                         (sleep 10)
                         (setf (thread-local :foo) 222)
                         (print (thread-local :foo))))

(progn
  (print (thread-local :foo))
  (print (setf (thread-local :foo) :bar))
  (print (thread-local :foo))
  (print (setf (thread-local :hoge) :baha))
  (print (thread-local :hoge)))
|#
