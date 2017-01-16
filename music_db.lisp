;;添加Cd
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;;定义 db 全局变量
(defvar *db* nil)

;;添加数据到 db'
(defun add-record (cd) (push cd *db*))

;;打印数据库
(defun dump-db() (dolist (cd *db*)
                   (format t "~{~a: ~10t~a~%~}~%" cd)))

;;打印数据库
(defun dump-db()
  (format t "~{~{~a: ~10t~a~%~}~%~}" *db*))


(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;;添加单个数据到db
(defun prompt-for-cd()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating")  :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))


;;添加多个数据到 *db*
(defun add-cds()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: "))(return))))

;;保存数据到文件上
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;;加载数据库文件
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;;加载缓存保存下来的数据
(load-db "test.db")

;;搜索艺术家
(defun select-by-artlist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist ) artist))
   *db*))

;; select 方法
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;;艺术家搜索
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))


;;绑定变量
(defun foo (&key a b c)(list a b c))


(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))

;;where 条件
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda(cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))


;;跟新数据
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda(row)
             (when (funcall selector-fn row)
               (if title (setf (getf row :title) title))
               (if artist (setf (getf row :artist) artist))
               (if rating (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

;;删除数据
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))


(defmacro backwards (expr) (reverse expr))

(backwards ("hello,world" t format))

;;wrong
(defun make-comparison-expr (field value)
  (list equal (list getf cd field) value))


;;right
(defun make-comparison-expr(field value)
  (list 'equal (list 'getf 'cd field) value))


(defun make-comparison-expr(field value)
  `(equal (getf cd ,field) ,value))


(defun make-comparison-list(fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))


(defmacro where (&rest clauses)
  `#'(lambda(cd)(and ,@(make-comparison-list clauses))))

;;test
(where :title "Give Us a Break" :ripped t)

;;test
(macroexpand-1 '(where :title "Give Us a Break" :ripped t))

;;test
(select (where :title "Give Us a Break" :ripped t))
