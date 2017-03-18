(in-package :cl-user)

(defpackage :com.chens.binary-data
  (:use :common-lisp  :com.chens.macro-utilities)
  (:export :define-binary-class
           :define-tagged-binary-class
           :define-binary-type
           :read-value
           :*in-progress-objects*
           :parent-of-type
           :current-binary-object
           :*null*))

;; 将一个符号转化成对应的关键字符号1
(defun as-keyword (sym) (intern (string sym) :keyword))

;; 接收一个define-binary-class槽描述符并返回一个 DEFCLASS 槽描述符
(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))


(defmacro define-binary-class (name slots)
  `(defclass ,name ()
     ,(mapcar #'slot->defclass-slot slots)))



(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))


(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun mklist (x) (if (listp  x) x (list x)))

(defmacro define-binary-class (name slots)
  (with-gensyms (typevar objectvar streamvar)
    `(progn
       (defclass ,name ()
         ,(mapcar #'slot->defclass-slot slots))
       (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
         (let ((,objectvar (make-instance ',name)))
           (with-slots ,(mapcar #'first slots) ,objectvar
             ,@ (mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
           ,objectvar)))))


(defgeneric wriite-value (type stream value &key)
  (:documentation "Write a value as the given type to the stream."))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(wriite-value ',type ,stream ,name ,@args)))

(defmacro define-binary-class (name slots)
  (with-gensyms (typevar objectvar streamvar)
    `(progn
       (defclass ,name ()
         ,(mapcar #'slot->defclass-slot slots))
       (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
         (let ((,objectvar (make-instance ',name)))
           (with-slots ,(mapcar #'first slots) ,objectvar
             ,@ (mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
           ,objectvar))
       (defmethod wriite-value ((,typevar (eql ',name)) ,streamvar ,objectvar &key)
         (with-slots ,(mapcar #'first slots) ,objectvar
           ,@ (mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))



(macroexpand-1 '(define-binary-class id3-tag
                 ((indentifier (is-8859-1-string :length 3))
                  (major-version u1)
                  (revision u1)
                  (flags u1)
                  (size id3-tag-size)
                  (frames (id3-frames :tag-size size)))))

;; 添加继承和标记的结构

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream"))


(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object to the stream."))

(defmacro define-binary-class (name superclasses slots)
  (with-gensyms (objectvar streamvar)
    `(progn
       (defclass ,name ,superclasses
         ,(mapcar #'slot->defclass-slot slots))

       (defmethod read-object progn ((,objectvar ,name) ,streamvar)
                  (with-slots ,(mapcar #'first slots) ,objectvar
                    ,@ (mapcar #'(lambda (x) (slot->read-value x streamvar)) slots)))

       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
                  (with-slots ,(mapcar #'first slots) ,objectvar
                    ,@ (mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

;; 返回由一个二进制类直接定义的槽
(defun direct-slots (name)
  (copy-list (get name 'slots)))

;; 返回从其他二进制中继承的槽
(defun inherited-slots (name)
  (loop for super in (get name 'superclasses)
     nconc (direct-slots super)
     nconc (inherited-slots super)))

;; 返回包含所有直接定义和继承得到的槽名称的类表
(defun all-slots (name)
  (nconc (direct-slots name) (inherited-slots name)))

;;计算所有新类的槽列表
(defun new-class-all-slots (slots superclasses)
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

(defmacro define-binary-class (name (&rest superclasses) slots)
  (with-gensyms (objectvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'slots) ',(mapcar #'first slots))
         (setf (get ',name 'superclasses) ',superclasses))

       (defclass ,name ,superclasses
         ,(mapcar #'slot->defclass-slot slots))

       (defmethod read-object progn ((,objectvar ,name) ,streamvar)
                  (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
                    ,@ (mapcar #'(lambda (x) (slot->read-value x streamvar)) slots)))

       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
                  (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
                    ,@ (mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

;; 带有标记的结构

(defmacro define-generic-binary-class (name (&rest superclasses) slots read-method)
  (with-gensyms (objectvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'slots) ',(mapcar #'first slots))
         (setf (get ',name 'superclasses) ',superclasses))

       (defclass ,name ,superclasses
         ,(mapcar #'slot->defclass-slot slots))

       ,read-method

       (defmethod with-object progn ((,objectvar ,name) ,streamvar)
                  (declare (ignorable ,streamvar))
                  (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
                    ,@ (mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

(defmacro define-binary-class (name (&rest superclasses) slots)
  (with-gensyms (objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
                                  (defmethod read-object progn ((,objectvar ,name) ,streamvar)
                                             (declare (ignorable ,streamvar))
                                             (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
                                               ,@ (mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))))))


(defmacro define-tagged-binary-class (name (&rest superclasses) slots &rest options)
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
                                  (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
                                    (let* ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
                                      (let ((,objectvar
                                             (make-instance
                                              ,@ (or (cdr (assoc :dispatch options))
                                                     (error "Must supply :dispatch form."))
                                                 ,@ (mapcan #'slot->keyword-arg slots))))
                                        (read-object ,objectvar ,streamvar)
                                        ,objectvar))))))



(defun slot->binding (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))

;; 生成用来读写代表已有类的实例的值
(defmacro define-binary-type (name (&rest args) &body spec)
  (with-gensyms (type)
    `(progn
       ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
          `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
             ,@body))

       ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
          `(defmethod write-value ((,type (eql ',name)) ,out ,value &key ,@args)
             ,@body)))))


(defmacro define-binary-type (name (&rest args) &body spec)
  (ecase (length spec)
    (1
     (with-gensyms (type stream value)
       (destructuring-bind (derived-from &rest derived-args) (mklist (first spec))
         `(progn
            (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
              (read-value ',derived-from ,stream ,@derived-args))
            (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
              (wriite-value ',derived-from ,stream ,value ,@derived-args))))))
    (2
     (with-gensyms (type)
       `(progn
          ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
             `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
                ,@body))
          ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
             `(defmethod wriite-value ((,type (eql ',name)) ,out ,value &key ,@args)
                ,@body)))))))



(define-binary-type iso-8859-1-string (length)
  (:reader (in)
           (let ((string (make-string length)))
             (dotimes (i length)
               (setf (char string i) (code-char (read-byte in))))
             string))
  (:writer (out string)
           (dotimes (i length)
             (write-byte (char-code (char string i)) out))))

;; 当前对象栈
(defvar *in-progress-objects* nil)

(defmethod read-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defmethod write-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))


(defun current-binary-object () (first *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))
