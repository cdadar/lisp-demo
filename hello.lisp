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


;; 宏

;; (if condition then-form [else-form])

(if (> 2 3) "Yup" "Nope")

(if (> 2 3) "Yup")

(if (> 3 2) "Yup" "Nope")


;; (when (spam-p current-message)
;;   (file-in-spam-folder current-message)
;;   (update-spam-database current-message))


;; (unless (spam-p current-message)
;;   (file-in-spam-folder current-message)
;;   (update-spam-database current-message))


;; (cond
;;   (a (do-x))
;;   (b (do-y))
;;   (c (do-z)))


;; (dolist (var list-form)
;;   body-form*)

(dolist (x '(1 2 3)) (print x))

(dolist (x '(1 2 3)) (print x) (if (evenp x ) (return)))

;; (dotimes (var count-form)
;;   body-form*)

(dotimes (i 4) (print i))

(dotimes (x 20)
  (dotimes (y 20)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))


(dotimes (x 10)
  (dotimes (y 10)
    (format t "~3d " (* (+ 1 x) (+ 1 y))))
  (format t "~%"))


;; (do (variable-definition*)
;;     (end-test-form result-form*)
;;   statement*)

(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))

(do ((i 0 (1+ i)))
    ((>= i 4))
  (print i))

(dotimes (i 4) (print i))

(defvar *some-future-date*)

(setf *some-future-date* (+ (get-universal-time) 1000))


(do ()
    ((> (get-universal-time) *some-future-date*))
  (format t "Wating~%")
  (sleep 60))


(loop
   (when (> (get-universal-time) *some-future-date*)
     (return))
   (format t "Wating~%")
   (sleep 60))

(do ((nums nil) (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums))

(loop for i from 1 to 10 collecting i)

(loop for x from 1 to 10 summing (expt x 2))

(loop for x across "the quick brown fox jumps over the lazy dog"
   counting (find x "aeiou"))


;; across and below collecting counting finally for from summing then to 循环关键字
(loop for i below 10
   and a = 0 then b
   and b = 1 then (+ b a)
     finally (return a))


;; 定义宏

;; 宏运行的时期被称为宏展开期(macro expansion time)
;; 宏展开期无法访问那些仅存在与运行期的数据

;; (defmacro when (condition &rest body)
;;   `(if ,condition (progn ,@body)))

;; 编写宏的步骤
;; 1 编写示例的宏调用以及它应当展开成的代码,反之亦然;
;; 2 编写从示例调用的参数中生成手写展开式的代码
;; 3 确保宏抽象不产生"泄漏"

(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-primep (number)
  (loop for n from number when (primep n) return n))


;; 素数
(do-primes (p 0 19)
  (format t "~d " p))

(do ((p (next-primep 0) (next-primep (1+ p))))
    ((> p 19))
  (format t "~d " p))


(defmacro do-primes (var-and-range &rest body)
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-primep ,start) (next-primep (1+ ,var))))
        ((> ,var ,end))
      ,@body)))

;; &body 与 &rest 在语义上是等价的
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-primep ,start) (next-primep (1+ ,var))))
       ((> ,var ,end))
     ,@body))

;; 没有 "@" 符号,逗号会导致子表达式的值被原样包含
;; 有了 "@" 符号,其值(必须是一个列表) 可被"拼接" 到其所在的列表中


`(a (+ 1 2) c)
(list 'a '(+ 1 2) 'c)


`(a ,(+ 1 2) c)
(list 'a (+ 1 2) 'c)


`(a (list 1 2) c)
(list 'a '(list 1 2) 'c)

`(a ,(list 1 2) c)
(list 'a (list 1 2) 'c)

`(a ,@(list 1 2) c)
(append (list 'a) (list 1 2) (list 'c))


(defmacro do-primes-a ((var start end) &body body)
  (append '(do)
          (list (list (list var
                            (list 'next-primep start)
                            (list 'next-primep (list '1+ var)))))
          (list (list (list '> var end)))
          body))


;; macroexpand-1 接收任何Lisp表达式作为参数并返回做宏展开一层的结果

(macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))

;; 修复随机end值时重复求值的问题
(defmacro do-primes ((var start end) &body body)
  `(do ((ending-value ,end)
        (,var (next-primep ,start) (next-primep (1+ ,var))))
       ((> ,var ending-value))
     ,@body))


;; 当宏展开被求值时,传递给end的表达式将在传递给start的表达式之前求值
;; 这与他们在宏调用中的顺序相反



(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-primep ,start) (next-primep (1+ ,var)))
        (ending-value ,end))
       ((> ,var ending-value))
     ,@body))

;;(do-primes (ending-value 0 10)) 将无法运行
(let ((ending-value 0))
  (do-primes (p 0 10)
    (incf ending-value p))
  ending-value)


;; 函数 gensym  在每次被调用时返回唯一的符号. 这是一个没有被Lisp读取器读过的符号并且永远不会被读到
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-primep ,start) (next-primep (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))

;; 除非有特殊理由,否则需要将展开式中的任何子形式放在一个位置上,使其求值顺序与宏调用的子形式相同
;; 除非有特殊理由,否则需要确保子形式仅被求值一次,方法是在展开式中创建变量来持有求值参数形式所得到的值,然后在展开式中所有需要用到该值的地方使用这个变量.
;; 在宏展开期使用 gensym 来创建展开式中用到的变量名

;; 用于编写宏的宏

(defmacro do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-primep ,start) (next-primep (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))


;; do-primes 展开
;; (let ((ending-value-name (gensym)))
;;   `(do ((,var (next-primep ,start) (next-primep (1+ var)))
;;         (,ending-value-name ,end))
;;        ((> ,var ,ending-value-name))
;;      ,@body))

(defmacro do-primes ((var start end) &body body)
  (once-only (start end)
    `(do ((,var (next-primep ,start) (next-primep (1+ ,var))))
         ((> ,var ,end))
       ,@body)))

;; 用来生成特定顺序仅求值特定宏参数一次的代码
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))



;; 数字
;; #b 二进制
;; #o 八进制
;; #x 十六进制
;; #nR n代表进制数

;; 指数标记由单个字母后跟一个可选符号和一个数字序列组成,其代表10的指数用来跟指数标记前的数字向相乘
;; 指数标记 s,f,d,l分别代表短型,单精度,双精度以及长型
;; 字母e代表默认表示方式(单浮点数)

;; 复数 #C或#c跟上一个由两个实数所组成的列表,分别代表复数的实部和虚部.

;; floor 向负无穷方向截断;返回小于或等于实参的最大整数
;; ceiling 向正无穷方向截断;返回大于或等于参数的最小整数
;; truncate 向零截断,对于正实参而言,它等价与floor,而对于负实参则等价与ceiling
;; round 四舍五入
;; mod rem 返回两个实数截断相除得到的模和余数

;; /= 全部实参都是不同值时才返回真
