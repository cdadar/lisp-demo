(defun hello-world ()
  (format t "hello world!"))

(defun verbose-sum (x y)
  "Sum any two numbers after prining a message."
  (format t "Summing ~d and ~d. ~%" x y)
  (+ x y))


;; 可选形参
(defun foo (a b &optional c d)(list a b c d))

;; 默认值
(defun foo (a &optional (b 10)) (list a b))

;; 获取形参是否是使用默认值
(defun foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))

;;剩余形参
;;; (defun foo (steam string &rest values) ...)
;;; (defun foo (&rest numbers) ...)

;; 关键字形参
(defun foo (&key a b c) (list a b c))
(foo :a 1 :b 2 :c 2)

;; 关键字形参 默认值 及 是否使用默认值
(defun foo (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))


;; 指定形参的关键字
(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))


;; 混合不同的形参类型
(defun foo (x &optional y &key z) (list x y z))

(defun foo (&rest rest &key a b c) (list rest a b c))

(foo :a 1 :b 2 :c 3)


;; 默认最后一个表达式被作为整个函数的返回值
(defun foo (a b)
  (+ a b)
  (- a b))

;; return-from 返回值
(defun foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo (list i j))))))

;; 获取一个函数对象的方法
(function foo)
#'foo


;; 使用函数对象调用
(foo 10)
(funcall #'foo 10)

;; funcall 建设性的用法 接收函数对象作为实参
(defun plot(fn min max step)
  (loop for i from min to max by step do
       (loop repeat (funcall fn i) do (format t "*"))
       (format t "~%")))

(plot #'exp 0 4 1/2)

;; apply 第一个参数是一个函数对象 最后一个是列表
(defvar plot-data '(#'exp 0 4 1/2))

;; (apply #'plot plot-data)

(apply 'cons '((+ 2 3) 4))

;;匿名参数

(funcall #'(lambda (x y)(+ x y)) 2 5)

((lambda (x y)(+ x y)) 2 5)


(plot #'(lambda (x) (* 2 x)) 0 10 1)

;;变量
(let ((x 10) (y 20) (z 30))
  (+ x y z))

(defun foo (x)
  (format t "Parameter: ~a~%" x)
  (let ((x 2))
    (format t "Quter let: ~a~%" x)
    (let ((x 3))
      (format t "Inner let: ~a~%" x))
    (format t "Quter let: ~a~%" x))
  (format t "Parameter: ~a~%" x))

(dotimes (x 10) (format t "~d " x))

;; let* 每个变量的初始值形式,都可以引用到那些在变量列表中早点引入的变量

(let* ((x 10)
       (y (+ x 10)))
  (list x y))

;;词法变量盒闭包
(let ((count 0)) #'(lambda()(setf count (+ 1 count))))

(defparameter *fn* (let ((count 0)) #'(lambda()(setf count (+ 1 count)))))


(let ((count 0)) (list
                  #'(lambda()(incf count))
                  #'(lambda()(decf count))
                  #'(lambda() count)))
;;动态变量 约定 以*开始和结尾的名字表示全局变量
(defvar *count* 0
  "Count of widgets made so far.")

(defparameter *gap-tolerance* 0.001
  "Tolerance to be allowed in widget gaps.")

;; defparameter 总是将初始值赋给命名的变量
;; defvar 只有当变量未定义时才这样做

(defun increment-widget-count()(incf *count*))


;; (let ((*standard-output* *some-other-stream*))
;;   (stuff))

(defvar *x* 10)
(defun foo()(format t "X: ~d~%" *x*))

(defun bar()
  (foo)
  (let ((*x* 20)) (foo))
  (foo))

(defun foo()
  (format t "Before assignment~18tX: ~d~%" *x*)
  (setf *x* (+ 1 *x*))
  (format t "After assignment~18tX: ~d~%" *x*))

;;常量 约定 以+开始和结尾的名字表示常量
;; (defconstant name initial-value-form [ docoumentation-string ])

;;赋值
;; (setf place value)

(setf *x* 10)

(defun foo (x) (setf x 10))


(let ((y 20))
  (foo y)
  (print y))

;; (setf x 1)
;; (setf y 2)

;;(setf x 1 y 2)

;;(setf x (setf y (random 10)))

;;广义赋值

;; Simple variable : (setf x 10)
;; Array : (setf (ardf a 0) 10)
;; Hash table : (setf (gethash 'key hash) 10)
;; Slot named 'filed' : (setf (field 0) 10)

;;(incf x) === (setf x (+ x 1))
;;(decf x) === (setf x (- x 1))
;;(incf x 10) === (setf x (+ x 10))


;; (incf (aref *array* (random (length *array*))))
;; (setf (aref *array* (random (length *array*)))
;;       (+ 1 (aref *array* (random *array*))))

;; rotatef 交换值的修改宏
;; (rotatef a b)
;; (let ((tmp a)) (setf a b b tmp) nil)

;; shiftf 将值向左侧移动
;; (shiftf a b 10)
;; (let ((tmp a)) (setf a b b 10) tmp)


;;宏
