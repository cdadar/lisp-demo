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


;; 
