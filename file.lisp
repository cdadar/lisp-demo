;; (defun foo()
;;   #+allegro (do-one-thing)
;;   #+sbcl (do-another-thing)
;;   #+clisp (something-else)
;;   #+cmu (yet-another-verison)
;;   #- (or allegro sbcl clisp cmu)(error "Not implemented"))

;; #+ 特性表达式为真时,读取器会正常下一个表达式;否则跳过下一个表达式,将它作为空白对待.
;; #- 特性表达式为假时,读取器会正常下一个表达式;否则跳过下一个表达式,将它作为空白对待.


(in-package :cl-user)

(defpackage :com.chens.pathnames
  (:use :common-lisp)
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p))




;; 测试一个路径名的给定组件是否"存在",也就是说该组件既不是NIL也不是特殊值:unspecific
(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

;; 测试一个路径名是否已经是目录形式

(defun directory-pathname-p (p)
  "Is the given pathname the name of a directory? This function can
usefully be used to test whether a name returned by LIST-DIRECTORIES
or passed to the function in WALK-DIRECTORY is the name of a directory
in the file system since they always return names in `directory normal
form'."
  (flet ((component-present-p (value)
           (and value (not (eql value :unspecific)))))
    (and
     (not (component-present-p (pathname-name p)))
     (not (component-present-p (pathname-type p)))
     p)))


(defun file-pathname-p (p)
  (unless (directory-pathname-p p) p))


;; 将任何路径名转换成目录形式的路径名

(defun pathname-as-directory (name)
  "Return a pathname reperesenting the given pathname in
`directory normal form', i.e. with all the name elements in the
directory component and NIL in the name and type components. Can
not be used on wild pathnames because there's not portable way to
convert wildcards in the name and type into a single directory
component. Returns its argument if name and type are both nil or
:unspecific."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name      nil
         :type      nil
         :defaults pathname)
        pathname)))

;; 接受一个目录形式或文件文件形式的路径名,并返回一个给定实现下的适当的通配符.它通过使用读取期条件化在除 clisp 之外的所有实现里生成一个带有 :wild 类型组件的路径名,而在 clisp 中该组件为nil
(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))



;; 实现标准函数 directory 外围的一个包装层
(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (directory (directory-wildcard dirname)))

;; 添加多个实现的兼容
(defun list-directory (dirname)
  "Return a list of the contents of the directory named by dirname.
Names of subdirectories will be returned in `directory normal
form'. Unlike CL:DIRECTORY, LIST-DIRECTORY does not accept
wildcard pathnames; `dirname' should simply be a pathname that
names a directory. It can be in either file or directory form."
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))

  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    ;; SBCL, CMUCL, and Lispworks return subdirectories in directory
    ;; form just the way we want.
    (directory wildcard)

    #+openmcl
    ;; OpenMCl by default doesn't return subdirectories at all. But
    ;; when prodded to do so with the special argument :directories,
    ;; it returns them in directory form.
    (directory wildcard :directories t)

    #+allegro
    ;; Allegro normally return directories in file form but we can
    ;; change that with the :directories-are-files argument.
    (directory wildcard :directories-are-files nil)

    #+clisp
    ;; CLISP has a particularly idiosyncratic view of things. But we
    ;; can bludgeon even it into doing what we want.
    (nconc
     ;; CLISP won't list files without an extension when :type is
     ;; wild so we make a special wildcard for it.
     (directory wildcard)
     ;; And CLISP doesn't consider subdirectories to match unless
     ;; there is a :wild in the directory component.
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))

;; 在clisp的directory只有传递给一个以:wild作为目录组件的最后一个元素且名字和类型组件为nil的通配路径名时,才可以返回子目录
#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))



;; 接受一个路径名(目录形式或文件形式),并在其代表的文件存在时返回一个等价的路径名,否则返回NIL,但如果该文件存在并且是一个目录,那么总是返回目录形式的路径名

(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)
  ;; 在allegro cmucl 中接受任何形式的目录名但不会返回目录形式的路径名,而只是简单地返回传给它的参数
  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))
  ;; 在clisp 中传递目录形式的名字时立即报错,无论该名字所代表的文件或目录是否存在;以文件形式传递一个名字且该名字实际上是一个目录的名字时报错.测试目录是否存在clisp提供一个probe-directory(在ext包中)
  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))

  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "list-directory not implemented."))

;; pathname-as-directory 的逆函数,它返回等价于其参数的文件形式的路径名

(defun pathname-as-file (name)
  "Return a pathname reperesenting the given pathname in `file form',
i.e. with the name elements in the name and type component. Can't
convert wild pathnames because of problems mapping wild directory
component into name and type components. Returns its argument if
it is already in file form."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
        (let* ((directory (pathname-directory pathname))
               (name-and-type (pathname (first (last directory)))))
          (make-pathname
           :directory (butlast directory)
           :name (pathname-name name-and-type)
           :type (pathname-type name-and-type)
           :defaults pathname))
        pathname)))

;; 遍历目录树
;; 接受一个目录的名字和一个函数,并在该目录下所有文件的路径名上递归地调用该函数.接受两个关键字:directories和:test.当:directories 为真时,他将所有目录的路径名和正规文件上调用该函数;如果有:test参数,它指定另一个函数,在调用主函数之前在每一个路径名上调用该函数,主函数只有当测试参数返回真时才会被调用.:test参数的默认值是一个总是返回真的函数,它是通过调用标准函数constantly而生成的

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  "Walk a directory invoking `fn' on each pathname found. If `test' is
supplied fn is invoked only on pathnames for which `test' returns
true. If `directories' is t invokes `test' and `fn' on directory
pathnames as well."
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))


(defun directory-p (name)
  "Is `name' the name of an existing directory."
  (let ((truename (file-exists-p name)))
    (and truename (directory-pathname-p name))))

(defun file-p (name)
  "Is `name' the name of an existing file, i.e. not a directory."
  (let ((truename (file-exists-p name)))
    (and truename (file-pathname-p name))))

