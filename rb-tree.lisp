;;;=====================================================
;;; 2013年 08月 28日 星期三 14:10:43 CST
;;; author: 李小丹(Li Shao Dan) 字 殊恒(shuheng)
;;; K.I.S.S
;;; S.P.O.T
;;; red-black tree
;;; Copyright © 2013 李小丹


(defstruct rb-node
  (color 'RED)
  (data nil)
  (left nil)
  (right nil)
  (parent nil))


(defstruct rb-tree
  (root nil)
  (size 0)
  (less #'<)
  (equal #'eql))


(defun search-core (rbt root data)
  (when root
    (let ((el (rb-node-data root)))
      (cond
        ((and (rb-node-left root) (funcall (rb-tree-less rbt) data el))
         (search-core rbt (rb-node-left root) data))
        ((and (rb-node-right root) (funcall (rb-tree-less rbt) el data))
         (search-core rbt (rb-node-right root) data))
        ((funcall (rb-tree-equal rbt) data el) (values root t))
        (t (values root nil))))))


(defmacro def-rotate (dir set1 set2)
  (let
      ((fnn (intern (concatenate 'string (symbol-name dir) "-ROTATE")))
       (set11 (find-symbol (concatenate 'string "RB-NODE-" (symbol-name set1))))
       (set22 (find-symbol (concatenate 'string "RB-NODE-" (symbol-name set2))))
       (d (gensym)))
    `(defun ,fnn (rbt node)
       (let ((,d (,set11 node)) (p (rb-node-parent node)))
         (setf (,set11 node) p)
         (setf (,set22 p) ,d)
         (setf (rb-node-parent node) (rb-node-parent p))
         (setf (rb-node-parent p) node)
         (when ,d (setf (rb-node-parent ,d) p))
         (let ((pp (rb-node-parent node)))
           (if pp
               (if (funcall (rb-tree-less rbt) (rb-node-data pp) (rb-node-data p))
                   (setf (rb-node-right pp) node)
                   (setf (rb-node-left pp) node))
               (setf (rb-tree-root rbt) node)))))))


                                        ;(format t "~a~%" (macroexpand-1 '(def-rotate left left right)))
(def-rotate LEFT LEFT RIGHT)
(def-rotate RIGHT RIGHT LEFT)


(defun successor-core (rbt node)
  (let ((r (rb-node-right node)))
    (if r (rb-minimum r)
        (let ((p (rb-node-parent node)) (cur node))
          (loop while (and p (not (eql (l-or-r rbt p node) 'LEFT)))
             do (setf cur p p (rb-node-parent cur))) p))))


(defun rbt-successor (rbt data)
  (let ((root (rb-tree-root rbt)))
    (multiple-value-bind (d e) (search-core rbt root data)
      (if e (let ((s (successor-core rbt d)))
              (if s (values (rb-node-data s) t) (values nil nil)))
          (values nil nil)))))


(defun predecessor-core (rbt node)
  (let ((l (rb-node-left node)))
    (if l (rb-maximum l)
        (let ((p (rb-node-parent node)) (cur node))
          (loop while (and p (not (eql (l-or-r rbt p node) 'RIGHT)))
             do (setf cur p p (rb-node-parent cur))) p))))


(defun rbt-predecessor (rbt data)
  (let ((root (rb-tree-root rbt)))
    (multiple-value-bind (d e) (search-core rbt root data)
      (if e (let ((p (predecessor-core rbt d)))
              (if p (values (rb-node-data p) t) (values nil nil)))
          (values nil nil)))))


(defmacro rb-uncle (node d)
  (let ((gpar (gensym))
        (rbn (find-symbol (concatenate 'string "RB-NODE-" (symbol-name d)))))
    `(let ((,gpar (rb-node-parent (rb-node-parent ,node))))
       (if ,gpar (,rbn ,gpar) nil))))


(defun get-color (node)
  (if node (rb-node-color node) 'BLACK))


(defun l-or-r (rbt p node)
  (if (funcall (rb-tree-less rbt) (rb-node-data p) (rb-node-data node))
      'RIGHT 'LEFT))


(defun insert-core (rbt p data)
  (let ((node (make-rb-node :data data :parent p)))
    (if (eql (l-or-r rbt p node) 'LEFT) (setf (rb-node-left p) node)
        (setf (rb-node-right p) node)) node))


(defmacro ins-b-core (rbt p node d)
  (let ((cur (gensym))
        (par (gensym))
        (sr nil)
        (sd nil)
        (rotate (find-symbol (concatenate 'string (symbol-name d) "-ROTATE"))))
    (if (eql d 'LEFT)
        (progn (setf sd (find-symbol (concatenate 'string "RB-NODE-" (symbol-name 'RIGHT))))
               (setf sr (find-symbol (concatenate 'string (symbol-name 'RIGHT) "-ROTATE"))))
        (progn (setf sd (find-symbol (concatenate 'string "RB-NODE-" (symbol-name 'LEFT))))
               (setf sr (find-symbol (concatenate 'string (symbol-name 'LEFT) "-ROTATE")))))
    `(let ((,cur ,node) (,par ,p))
       (when (eql ',d (l-or-r ,rbt ,par ,cur))
         (,sr ,rbt ,cur)
         (setf ,cur (,sd ,cur))
         (setf ,par (rb-node-parent ,cur)))
       (setf (rb-node-color ,par) 'BLACK)
       (setf (rb-node-color (rb-node-parent ,par)) 'RED)
       (,rotate ,rbt ,par)
       (values ,cur ,par))))


(defun ins-change-color (par uncle)
  (let ((gpar (rb-node-parent par)))
    (setf (rb-node-color uncle) 'BLACK)
    (setf (rb-node-color par) 'BLACK)
    (setf (rb-node-color gpar) 'RED)))


(defmacro ins-balance-core (rbt c p d)
  (let ((uncle (gensym)) (cur (gensym)) (par (gensym)))
    `(let ((,uncle (rb-uncle ,c ,d)) (,cur ,c) (,par ,p))
       (if (eql (get-color ,uncle) 'RED)
           (progn (ins-change-color ,par ,uncle)
                  (setf ,cur (rb-node-parent ,par))
                  (if ,cur
                      (setf ,par (rb-node-parent ,cur))
                      (setf ,par nil))
                  (values ,cur ,par))
           (ins-b-core ,rbt ,par ,cur ,d)))))



(defun ins-balance (rbt node)
  (let* ((cur node) (par (rb-node-parent node)) (root (rb-tree-root rbt)))
    (loop while (and (not (eql cur root)) (eql (get-color par) 'RED)) do
         (if (eql (l-or-r rbt (rb-node-parent par) par) 'LEFT)
             (multiple-value-bind (c p) (ins-balance-core rbt cur par RIGHT)
               (setf cur c par p))
             (multiple-value-bind (c p) (ins-balance-core rbt cur par LEFT)
               (setf cur c par p)))
         (setf root (rb-tree-root rbt)))))


(defun rbt-insert (rbt data)
  (let ((root (rb-tree-root rbt)))
    (if root
        (multiple-value-bind (p e) (search-core rbt root data)
          (when (not e) (incf (rb-tree-size rbt))
                (ins-balance rbt (insert-core rbt p data))))
        (progn (incf (rb-tree-size rbt)) (setf (rb-tree-root rbt) (make-rb-node :data data)))))
  (setf (rb-node-color (rb-tree-root rbt)) 'BLACK) rbt)


(defmacro rm-balance-core (rbt node par md sd)
  (let ((mrotate (find-symbol (concatenate 'string (symbol-name md) "-ROTATE")))
        (srotate (find-symbol (concatenate 'string (symbol-name sd) "-ROTATE")))
        (mdc (find-symbol (concatenate 'string "RB-NODE-" (symbol-name md))))
        (sdc (find-symbol (concatenate 'string "RB-NODE-" (symbol-name sd))))
        (bro (gensym))
        (cur (gensym))
        (pnt (gensym)))
    `(let ((,cur ,node) (,pnt ,par) (,bro (,sdc ,par)))
       (when (eql (get-color ,bro) 'RED)
         (setf (rb-node-color ,pnt) 'RED)
         (setf (rb-node-color ,bro) 'BLACK)
         (,mrotate ,rbt ,bro)
         (setf ,bro (,sdc ,pnt)))
       (if (or (null ,bro) (and (eql (get-color (rb-node-left ,bro)) 'BLACK)
                                (eql (get-color (rb-node-right ,bro)) 'BLACK)))
           (progn
             (when ,bro (setf (rb-node-color ,bro) 'RED))
             (setf ,cur ,pnt))
           (progn
             (when (eql (get-color (,sdc ,bro)) 'BLACK)
               (setf (rb-node-color ,bro) 'RED)
               (setf (rb-node-color (,mdc ,bro)) 'BLACK)
               (,srotate ,rbt (,mdc ,bro))
               (setf ,bro (,sdc ,pnt)))
             (setf (rb-node-color ,bro) (rb-node-color ,pnt))
             (setf (rb-node-color ,pnt) 'BLACK)
             (setf (rb-node-color (,sdc ,bro)) 'BLACK)
             (,mrotate ,rbt ,bro)
             (setf ,cur (rb-tree-root ,rbt))))
       ,cur)))


(defun rb-rm-balance (rbt node par d)
  (let ((cur node) (root (rb-tree-root rbt)) (pnt par))
    (loop while
         (and (not (eql cur root)) (eql (get-color cur) 'BLACK)) do
         (if (eql d 'LEFT)
             (setf cur (rm-balance-core rbt cur pnt LEFT RIGHT))
             (setf cur (rm-balance-core rbt cur pnt RIGHT LEFT)))
         (setf pnt (rb-node-parent cur))
         (setf root (rb-tree-root rbt)))
    (when cur (setf (rb-node-color cur) 'BLACK))))


(defmacro np-node (fn bd id)
  (let ((bn (find-symbol (concatenate 'string "RB-NODE-" (symbol-name bd))))
        (in (find-symbol (concatenate 'string "RB-NODE-" (symbol-name id))))
        (name (intern (concatenate 'string (symbol-name fn)))))
    `(defun ,name (node)
       (let ((p (,bn node)))
         (when p
           (let ((n (,in p)))
             (loop while n do (setf p n) (setf n (,in n))))) p))))


                                        ;(format t "~a~%" (macroexpand-1 '(np-node next-node RIGHT LEFT)))
(np-node next-node RIGHT LEFT)
(np-node prev-node LEFT RIGHT)


(defun rb-rm-core (rbt node)
  (let ((par nil) (tmp nil) (rm nil) (d nil))
    (if (not (and (rb-node-left node) (rb-node-right node)))
        (setf rm node) (setf rm (next-node node)))
    (if (not (setf tmp (rb-node-left rm))) (setf tmp (rb-node-right rm)))
    (setf par (rb-node-parent rm))
    (if par
        (progn (setf d (l-or-r rbt par rm))
               (if (eql d 'LEFT) (setf (rb-node-left par) tmp)
                   (setf (rb-node-right par) tmp)))
        (setf (rb-tree-root rbt) tmp))
    (when tmp (setf (rb-node-parent tmp) par))
    (when (not (eql rm node)) (setf (rb-node-data node) (rb-node-data rm)))
    (when (eql (rb-node-color rm) 'BLACK) (rb-rm-balance rbt tmp par d)) node))


(defun rbt-remove (rbt data)
  (let ((root (rb-tree-root rbt)))
    (multiple-value-bind (n e) (search-core rbt root data)
      (when e (decf (rb-tree-size rbt)) (rb-rm-core rbt n)) (values rbt e))))


(defun mid-traverse (rbt cur fn)
  (when cur
    (mid-traverse rbt (rb-node-left cur) fn)
    (funcall fn (rb-node-data cur))
    (mid-traverse rbt (rb-node-right cur) fn)))


(defun front-traverse (rbt cur fn)
  (when cur
    (funcall fn (rb-node-data cur))
    (front-traverse rbt (rb-node-left cur) fn)
    (front-traverse rbt (rb-node-right cur) fn)))

(defun back-traverse (rbt cur fn)
  (when cur
    (back-traverse rbt (rb-node-left cur) fn)
    (back-traverse rbt (rb-node-right cur) fn)
    (funcall fn (rb-node-data cur))))


(defun rbt-traverse (rbt fn &optional (tr-type 'MID))
  (case tr-type
    (FRONT (front-traverse rbt (rb-tree-root rbt) fn))
    (MID (mid-traverse rbt (rb-tree-root rbt) fn))
    (BACK (back-traverse rbt (rb-tree-root rbt) fn))) nil)


(defun rbt-find (rbt data)
  (let ((root (rb-tree-root rbt)))
    (multiple-value-bind (d e) (search-core rbt root data)
      (if e (values (rb-node-data d) t) (values nil nil)))))


(defun rbt-clear (rbt)
  (setf (rb-tree-root rbt) nil) rbt)


(defun rb-maximum (node)
  (let ((cur node) (r (rb-node-right node)))
    (loop while r do (setf cur r r (rb-node-right cur))) cur))


(defun rb-minimum (node)
  (let ((cur node) (l (rb-node-left node)))
    (loop while l do (setf cur l l (rb-node-left cur))) cur))


(defun rbt-max (rbt)
  (let ((root (rb-tree-root rbt))) (rb-node-data (rb-maximum root))))


(defun rbt-min (rbt)
  (let ((root (rb-tree-root rbt))) (rb-node-data (rb-minimum root))))


(defun rbt-size (rbt)
  (rb-tree-size rbt))

;;; ======================== test ============================

(setf *print-circle* t)


(defun mytest ()
  (let ((rbt (make-rb-tree :equal #'=)) (nmax 38))
    (dotimes (i nmax) (rbt-insert rbt i))
    (format t "~a~%" (rbt-size rbt))
    (format t "~a~%" rbt)
    (rbt-traverse rbt #'(lambda (x) (format t "~a~%" x)))
    (format t "============================~%")
    (rbt-traverse rbt #'(lambda (x) (format t "~a~%" x)) 'FRONT)
    (format t "============================~%")
    (rbt-traverse rbt #'(lambda (x) (format t "~a~%" x)) 'BACK)
    (format t "============================~%")
    (dotimes (i nmax) (format t "~a: ~a~%" i (rbt-find rbt i)))
    (format t "max: ~a min: ~a~%" (rbt-max rbt) (rbt-min rbt))
    (dotimes (i nmax) (format t "~a: ~a~%" i (rbt-successor rbt i)))
    (dotimes (i nmax) (format t "~a: ~a~%" i (rbt-predecessor rbt i)))
    (dotimes (i nmax) (rbt-remove rbt i))
    (format t "~a~%" (rbt-size rbt))
    (rbt-traverse rbt #'(lambda (x) (format t "~a~%" x)) 'FRONT)
    (format t "----------------------------~%")
    (rbt-traverse rbt #'(lambda (x) (format t "~a~%" x)))
    (format t "----------------------------~%")))


(defun mytest-1 ()
  (let ((nmax 100) (rs (make-random-state t))
        (rbt (make-rb-tree :equal #'=)))
    (dotimes (i 38) (rbt-insert rbt (random nmax rs)))
    (format t "~a~%" (rbt-size rbt))
    (format t "~a~%" rbt)
    (rbt-traverse rbt #'(lambda (x) (format t "~a~%" x)))
    (format t "============================~%")
    (rbt-traverse rbt #'(lambda (x) (format t "~a~%" x)) 'FRONT)
    (format t "============================~%")
    (rbt-traverse rbt #'(lambda (x) (format t "~a~%" x)) 'BACK)
    (format t "============================~%")
    (format t "max: ~a min: ~a~%" (rbt-max rbt) (rbt-min rbt))
    (dotimes (i nmax) (format t "~a~%" (rbt-find rbt i)))
    (dotimes (i nmax) (format t "~a: ~a~%" i (rbt-successor rbt i)))
    (dotimes (i nmax) (format t "~a: ~a~%" i (rbt-predecessor rbt i)))
    (let ((m (random 100 rs))) (format t "rm max number: ~a~%" (1- m))
         (dotimes (i m) (rbt-remove rbt i)))
    (format t "~a~%" (rbt-size rbt))
    (format t "~a~%" rbt)
    (rbt-traverse rbt #'(lambda (x) (format t "~a~%" x)) 'FRONT)
    (format t "------------------------------~%")
    (rbt-traverse rbt #'(lambda (x) (format t "~a~%" x)))
    (format t "------------------------------~%")))


(mytest)
(mytest-1)
;;;============================================================
