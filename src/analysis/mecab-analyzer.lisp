(in-package #:montezuma)

;;; tokenizer
(defclass mecab-tokenizer (tokenizer)
  ((tokens :initform :unbound)
   (position :initform 0)))

(defmethod next-token :before ((self mecab-tokenizer))
  (with-slots (input tokens) self
    (when (eq tokens :unbound)
      (setf tokens (info.read-eval-print.mecab:mecab
                    (etypecase input
                      (string input)
                      (stream (series:collect 'string
                                (series:scan-stream input #'read-char)))))))))

(defmethod next-token ((self mecab-tokenizer))
  (with-slots (tokens position) self
    (let ((token (pop tokens)))
      (if token
          (let* ((表層形 (info.read-eval-print.mecab:node-表層形 token))
                 (new-position (+ position (length 表層形)))
                 (token (or (info.read-eval-print.mecab:node-原形 token) 表層形)))
            (prog1 (make-token
                    (normalize self token)
                    position new-position)
              (setf position new-position)))
          nil))))

(defmethod normalize ((self mecab-tokenizer) str)
  str)


;;; analyzer
(defclass mecab-analyzer (standard-analyzer)
  ())

(defmethod token-stream ((self mecab-analyzer) field string-or-stream)
  (declare (ignore field))
  (reduce (lambda (acc x) (make-instance x :input acc))
          '(mecab-tokenizer
            lowercase-filter
            stop-filter)
          :initial-value string-or-stream))
