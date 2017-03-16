(in-package :cl-user)
(ql:quickload 'cl-ppcre)
(defpackage :com.chens.spam
  (:use :common-lisp :com.chens.pathnames :cl-ppcre))


(in-package :com.chens.spam)

;; 主函数
(defun classify (text)
  (classification (score (extract-features text))))

(defparameter *max-ham-score* 0.4)
(defparameter *min-spam-score* 0.6)

;; 分类 垃圾邮件,有用信息或不确定
(defun classification (score)
  (cond
    ((<= score *max-ham-score*) 'ham)
    ((>= score *min-spam-score*) 'spam)
    (t 'unsure)))

;; 跟踪在垃圾邮件中出现的次数和正常邮件中出现的次数

(defclass word-feature ()
  ((word
    :initarg :word
    :accessor word
    :initform (error "Must supply :word")
    :documentation "The word this feature represents.")
   (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "Number of spams we have seen the feature in.")
   (ham-count
    :initarg :ham-count
    :accessor ham-count
    :initform 0
    :documentation "Number of hams we have seen the feature in.")))

;; 特征数据库
(defvar *feature-database* (make-hash-table :test #'equal))

;; 清空数据库
(defun clear-database()
  (setf
   *feature-database* (make-hash-table :test #'equal)
   *total-hams* 0
   *total-spams* 0))


;; 接受一个单词并且返回对应的特征
(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))

;; 使用正则从邮件文本中提取某个词

(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-z]{3,}" text)
   :test #'string=))

(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))

(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))

;; 接受一些文本和一个指示邮件类型(ham或spam)的符号
(defun train (text type)
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))

;; 接受一个word-feature 和 一个邮件类型并递增该特征的相应槽
(defun increment-count (feature type)
  (ecase type
    (ham (incf (ham-count feature)))
    (spam (incf (spam-count feature)))))

(defvar *total-spams* 0)
(defvar *total-hams* 0)


(defun increment-total-count (type)
  (case type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))


;;; 按单词统计


(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
    (/ spam-count (+ spam-count ham-count))))

(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
    (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
          (ham-frequency (/ ham-count (max 1 *total-hams*))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))


(defun bayesian-spam-probability (feature &optional
                                            (assumed-probability 1/2)
                                            (weight 1))
  (let ((basic-probability (spam-probability feature))
        (data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

;;; 合并概率

(defun score (features)
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

;; 跳过从邮件中提取的从在训练中出现过的特征
(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))


(defun fisher (probs number-of-probs)
  "The Fisher computation described by Robinson"
  (inverse-chi-square
   (* -2 (log (reduce #'* probs)))
   (* 2 number-of-probs)))

;; 防止概率的乘积下溢
(defun fisher (probs number-of-probs)
  "The Fisher computation described by Robinson"
  (inverse-chi-square
   (* -2 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))

;; 反向卡方分布函数

(defun inverse-chi-square (value degress-of-freedom)
  (assert (evenp degress-of-freedom))
  (min (loop with m = (/ value 2)
          for i below (/ degress-of-freedom 2)
          for prob = (exp (- m)) then (* prob (/ m i))
          summing prob)
       1.0))

;; 多值返回
(defun classification (score)
  (values
   (cond
     ((<= score *max-ham-score*) 'ham)
     ((>= score *min-spam-score*) 'spam)
     (t 'unsure))
   score))

;; 测试过滤器

(defun add-file-to-corpus (filename type corpus)
  (vector-push-extend (list filename type) corpus))

;; 带有填充指针的可调整向量
(defparameter *corpus* (make-array 1000 :adjustable t :fill-pointer 0))

(defun add-directory-to-corpus (dir type corpus)
  (dolist (filename (list-directory dir))
    (add-file-to-corpus filename type corpus)))

;; 主测试函数
(defun test-classifier (corpus tesing-fraction)
  (clear-database)
  (let* ((shuffled (shuffle-vector corpus))
         (size (length corpus))
         (train-on (floor (* size (- 1 tesing-fraction)))))
    (train-from-corpus shuffled :start 0 :end train-on)
    (test-from-corpus shuffled :start train-on)))

(defparameter *max-chars* (* 10 1024))

(defun train-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) do
       (destructuring-bind (file type) (aref corpus idx)
         (train (start-of-file file *max-chars*) type))))

(defun test-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) collect
       (destructuring-bind (file type) (aref corpus idx)
         (multiple-value-bind (classification score)
             (classify (start-of-file file *max-chars*))
           (list
            :file file
            :type type
            :classification classification
            :score score)))))

;; 工具函数

;; Fisher-Yates 算法
(defun nshuffle-vetor (vector)
  (loop for idx downfrom (1- (length vector)) to 1
     for other = (random (1+ idx))
     do (unless (= idx other)
          (rotatef (aref vector idx) (aref vector other))))
  vector)

(defun shuffle-vector (vector)
  (nshuffle-vetor (copy-seq vector)))

(defun start-of-file (file max-chars)
  (with-open-file (in file)
    (let* ((length (min (file-length in) max-chars))
           (text (make-string length))
           (read (read-sequence text in)))
      (if (< read length)
          (subseq text 0 read)
          text))))


;; 分析结果
(defun result-type (result)
  (destructuring-bind (&key type classification &allow-other-keys) result
    (ecase type
      (ham
       (ecase classification
         (ham 'correct)
         (spam 'false-positive)
         (unsure 'missed-ham)))
      (spam
       (ecase classification
         (ham 'false-negative)
         (spam 'correct)
         (unsure 'messed-spam))))))


(defun false-positive-p (result)
  (eql (result-type result) 'false-positive))

(defun false-negative-p (result)
  (eql (result-type result) 'false-negative))

(defun missed-ham-p (result)
  (eql (result-type result) 'missed-ham))

(defun missed-spam-p (result)
  (eql (result-type result) 'missed-spam))

(defun correct-p (result)
  (eql (result-type result) 'correct))


;;
(defun analyze-results (results)
  (let* ((keys '(total correct false-positive
                 false-negative missed-ham missed-spam))
         (counts (loop for x in keys collect (cons x 0))))
    (dolist (item results)
      (incf (cdr (assoc 'total counts)))
      (incf (cdr (assoc result-type item) counts)))
    (loop with total = (cdr (assoc 'total counts))
       for (label . count) in counts
       do (format t "~&~@(~a~):20t~5d~,5t: ~6.2f%~%"
                  label count (* 100 (/ count total))))))

(defun explain-calssification (file)
  (let* ((text (start-of-file file *max-chars*))
         (features (extract-features text))
         (score (score features))
         (classification (classification score)))
    (show-summary file text classification score)
    (dolist (feature (sorted-interesting features))
      (show-feature feature))))

(defun show-summary (file text classification score)
  (format t "~&~a" file)
  (format t "~2%~a~2%" text)
  (format t "Classified as ~a with score of ~,5f~%" classification score))

(defun show-feature (feature)
  (with-slots (word ham-count spam-count) feature
    (format t "~&~2T~a~30t hams: ~5d; spams: ~5d;~,10t prob:~,f~%"
            word ham-count spam-count (bayesian-spam-probability feature))))

(defun sorted-interesting (features)
  (sort (remove-if #'untrained-p features) #'< :key #'bayesian-spam-probability))
