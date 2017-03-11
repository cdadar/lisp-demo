(define-condition malformed-log-entry-error (error)
  ((test :initarg :text :reader text)))

(defun parse-log-entry (text)
  (if (evenp text)
      text
      (error 'malformed-log-entry-error :text text)))

(defun parse-log-file()
  (loop for i from 1 to 10 do
       (handler-case (parse-log-entry i)
         (malformed-log-entry-error (var) (text var)))))




(define-condition malformed-log-entry-error (error)
  ((test :initarg :text :reader text)))

(defun parse-log-entry (text)
  (if (evenp text)
      (list text "even")
      (error 'malformed-log-entry-error :text text)))

(defun parse-log-file ()
  (loop for i from 1 to 10
     for entry = (restart-case (parse-log-entry i);;提供三个restart-case
      (skip-log-entry () "a")
      (use-value (v) (list v "odd"))
      (reparse-entry (fixed-text) (parse-log-entry fixed-text)))
     when entry collect it))

(defun analyze-log ()
  (print (parse-log-file)))

(defun skip-log-entry (c)
  (let ((restart (find-restart 'skip-log-entry)))
    (when restart
      (invoke-restart restart))))

(defun use-value-p (c)
  (use-value (text c)))

(defun reparse-log-entry (c)
  (let ((reparse (find-restart 'reparse-entry)))
    (when reparse
      (invoke-restart reparse (+ (text c) 11)))))

;;在高层函数中，根据不同的情况选择不同的再启动策略
(defun log-analyzer-skip ()
  (handler-bind ((malformed-log-entry-error #'skip-log-entry))
    (analyze-log)))

(defun log-analyzer-use ()
  (handler-bind ((malformed-log-entry-error #'use-value-p))
    (analyze-log)))

(defun log-analyzer-reparse ()
  (handler-bind ((malformed-log-entry-error #'reparse-log-entry))
    (analyze-log)))
