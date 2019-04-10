(defun double (x) (* x 2))

#'double

(eq #'double (car (list #'double)))

(lambda (x) (* x 2))

(setq double 2)

(symbol-value 'double)

(symbol-function 'double)

(setq x #'append)

(eq (symbol-value 'x) (symbol-function 'append))

(setf (symbol-function 'double)
      #'(lambda (x) (* x 2)))

;; (apply function args &reset args)
(apply #'+ 1 '(2))

(apply #'double '(1))

(funcall #'+ 1 2)

(sort '(1 2 3 4 5 6 7 8) #'<)

(defun cdadar/remove-if (fn lst)
  (if (null lst)
      nil
      (if (funcall fn (car lst))
          (cdadar/remove-if fn (cdr lst))
          (cons (car lst) (cdadar/remove-if fn (cdr lst))))))


(setf (get 'dog 'behavior)
      #'(lambda ()
          (wag-tail)
          (bark)))

;; 词法作用域
(let ((y 7))
  (defun scope-test (x)
    (list x y)))


(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n))
          lst))


(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))


(defun make-adder (n) #'(lambda (x) (+ x n)))


(defun make-adderb (n)
  #'(lambda (x &optional change)
      (if change
          (setq n x)
          (+ x n))))


(defun make-dbms (db)
  (list
   #'(lambda (key)
       (cdr (assoc key db)))
   #'(lambda (key val)
       (push (cons key val) db)
       key)
   #'(lambda (key)
       (setf db (delete key db :key #'car))
       key)))


(setq cities (make-dbms '((boston . us) (paris .france))))


(funcall (car cities) 'boston)
(funcall (second cities) 'london 'england)
(funcall (car cities) 'london)

(defun lookup (key db)
  (funcall (car db) key))

(mapcar #'(lambda (x) (+ x 2))
        '(2 5 7 3))

(labels ((inc (x) (1+ x)))
  (inc 3))

(defun count-instances (obj lsts)
  (labels ((instances-in (lst)
             (if (consp lst)
                 (+ (if (eq (car lst) obj) 1 0)
                    (instances-in (cdr lst)))
                 0)))
    (mapcar #'instances-in lsts)))


;; 尾递归

(defun cdadar/find-if (fn lst)
  (if (funcall fn (car lst))
      (car lst)
      (cdadar/find-if fn (cdr lst))))

(defun triangle (n)
  (labels ((tri (c n)
             (declare (type fixnum n c))
             (if (zerop n)
                 c
                 (tri (the fixnum (+ n c))
                      (the fixnum (- n 1))))))
    (tri 0 n)))


(defun foo (x) (1+ x))

(compiled-function-p #'foo)

(progn (compile 'bar '(lambda (x) (* x 3)))
       (compiled-function-p #'bar))


(defun 50th (lst) (nth 49 lst))

(proclaim '(inline 50th))

(defun foo (lst)
  (+ (50th lst) 1))

(defun foo (lst)
  (+ (nth 49 lst) 1))

(setq lst '(a b c))


(defun bad-reverse (lst)
  (let* ((len (length lst))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i ilimit))
      (rotatef (nth i lst) (nth j lst)))))

(bad-reverse lst)


(defun good-reverse (lst)
  (labels ((rev (lst acc)
             (if (null lst)
                 acc
                 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))

(defun powers (x)
  (values x (sqrt x) (expt x 2)))

(multiple-value-bind (base root square) (powers 4)
  (list base root square))

(defun fun (x)
  (list 'a (expt (car x) 2)))

(defun imp (x)
  (let (y sqr)
    (setq y (car x))
    (setq sqr (expt y 2))
    (list 'a sqr)))

(defun qualify (expr)
  (nconc (copy-list expr)
         (list 'maybe)))

(let ((x 0))
  (defun total (y)
    (incf x y)))

(defun ok (x) (nconc (list 'a x) (list 'c)))

(defun not-ok (x)
  (nconc (list 'a) x (list 'c)))

(setq *anything* 1)

(defun anything (x)
  (+ x *anything*))

(defun exclaim (expression)
  (append expression '(oh my)))

(exclaim '(lions and tigers and bears))

(nconc * '(goodness))

(exclaim '(fixnum and bignums and floats))

(defun exclaim (expression)
  (append expression (list 'oh 'my)))

(defun all-nicknames (names)
  (if (null names)
      nil
      (nconc (nicknames (car names))
             (all-nicknames (cdr names)))))

(defun find-books (towns)
  (if (null towns)
      nil
      (let ((shops (bookshops (car towns))))
        (if shops
            (values (car towns) shops)
            (find-books (cdr towns))))))


(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(find2 #'bookshops towns)

;; 代码 4.1： 操作列表的一些小函数
(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;; 代码 4.2： 操作列表的一些较大函数
(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))


(filter #'(lambda (x) (if (numberp x) (1+ x)))
        '(a 1 2 b 3 c d 4))

(group '(a b c d e f g) 3 )


(defun flatten (x)
  (labels ((rec  (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))


(flatten '(a (b c) ((d e) f)))

(prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (9)))

(before 'b 'd '(a b c d))

(< (position 'b '(a b c d)) (position 'd '(a b c d)))
(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))


(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))

(most #'length '((a b) (a b c) (a) (e f g)))

(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))

(best #'> '(1 2 3 4 5))

(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

(mostn #'length '((a b) (a b c) (a) (e f g)))

(defun mapa-b (fn a b &optional (step 1))
  (map-> fn
         a
         #'(lambda (x) (> x b))
         #'(lambda (x) (+ x step))))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(mapcars #'sqrt list1 list2)




(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))


(rmapcar #'princ '(1 2 (3 4 (5) 6) 7 (8 9)))

(rmapcar #'+ '(1 (2 (3) 4)) '(10 (20 (30) 40)))

(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                        (apply #'read-line args)
                        ")"))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.'~%")
  (loop
     (let ((in (apply))))))
