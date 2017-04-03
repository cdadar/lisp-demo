(require :aserve)

(defpackage :com.chens.allegro-service
  (:use :cl :net.aserve :com.chens.macro-utilities))

(in-package :com.chens.allegro-service)

(start :port 2001)

(publish-file :path "/hello.html" :file "./hello.html")

(publish-file :path "/hello.html" :remove t)

(publish-directory :prefix "/" :destination "./")

(defun random-number (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (format
       (request-reply-stream request)
       "<html>~@
                               <head><title>Random</title></head>~@
<body>~@
<p>Random number : ~d</p>~@
</body>~@

</html>" (random 10000)))))

(publish :path "/random-number" :function 'random-number)

(:html
 (:head (:title "Hello"))
 (:body (:p "Hello,world!")
        (:a :href "foo.html" "This is a link")))


(emit-html '(:html
             (:head (:title "Hello"))
             (:body (:p "Hello,world!")
              (:a :href "foo.html" "This is a link"))))

(emit-html `(:html
             (:head
              (:title "Random numbers"))
             (:body
              (:h1 "Random numbers")
              (:p ,@ (loop repeat 1000 collect (random 1000) collect " ")))))

(html (:p "foo"))

(let ((x "foo")) (html (:p x )))

(html (:ul (dolist (item (list 1 2 3)) (html (:li item)))))

(html (:p (+ 1 2)))


(let ((x (+ 1 2))) (html (:p x)))

(html
  (:html
   (:head
    (:title "Random numbers"))
   (:body
    (:h1 "Random numbers")
    (:p (loop repeat 10 do (html (:print (random 1000)) " "))))))

(defun random-number (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
        (html
          (:html
           (:head (:title "Random"))
           (:body
            (:p "Random number: " (:print (random 1000))))))))))

;; html 宏
(define-html-macro :standard-page ((&key title) &body body)
  `(:html
    (:head (:title ,title))
    (:body
     (:h1 ,title)
     ,@body)))

(html (:standard-page (:title "Hello") (:p "Hello,world.")))


;; 查询参数
(defun show-query-params (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
        (html
          (:standard-page
           (:title "Query Parameters")
           (if (request-query request)
               (html
                 (:table :border 1
                         (loop for (k . v) in (request-query request)
                            do (html (:tr (:td k) (:td v))))))
               (html
                 (:p "No query parameters.")))))))))

(publish :path "/show-query-params" :function 'show-query-params)


(defun simple-form (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (let ((*html-output* (request-reply-stream request)))
        (html
          (:html
           (:head (:title "Simple Form"))
           (:body
            (:form :method "POST" :action "/show-query-params"
                   (:table
                    (:tr (:td "Foo")
                         (:td (:input :name "foo" :size 20)))
                    (:tr (:td "Password")
                         (:td (:input :name "password" :type "password" :size 20))))
                   (:p (:input :name "submit" :type "submit" :value "Okay")
                       (:input ::type "reset" :value "Reset"))))))))))

(publish :path "/simple-form" :function 'simple-form)

(defun random-number (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (let* ((*html-output* (request-reply-stream request))
             (limit-string (or (request-query-value "limit" request) " "))
             (limit (or (parse-integer limit-string :junk-allowed t) 1000)))
        (html
          (:html
           (:head (:title "Random"))
           (:body
            (:p "Random number:" (:print (random limit))))))))))

(defun show-cookies (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
        (html
          (:standard-page
           (:title "Cookies")
           (if (null (get-cookie-value request))
               (html (:p "No cookies"))
               (html
                 (:table
                  (loop for (key . value) in (get-cookie-values request)
                     do (html (:tr (:td key) (:td value)))))))))))))

(publish :paht "/show-cookies" :function 'show-cookies)

(defun set-cookie (request entity)
  (with-http-response (request entity :content-type "text/html")
    (set-cookie-header request :name "MyCookie" :value "A cookie value")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
        (html
          (:standard-page
           (:title "Set Cookie")
           (:p "Cookie set.")
           (:p (:a :href "/show-cookies" "Look at cookie jar."))))))))

(publish :path "/set-cookie" :function 'set-cookie)


(define-url-function random-number (request (limit integer 1000))
  (:html
   (:head (:title "Random"))
   (:body
    (:p ("Random number:" (:print (random limit)))))))


(defmacro define-url-function (name (request &rest params) &body body)
  (with-gensyms (entity)
    (let ((params (mapcar #'normalize-param params)))
      `(progn
         (defun ,name (,request ,entity)
           (with-http-response (,request ,entity :content-type "text/html")
             (let* (,@ (param-bindings name request params))
               ,@ (set-cookies-code name request params)
                  (with-http-body (,request ,entity)
                    (with-html-output ((request-reply-stream ,request))
                      (html ,@body))))))
         (publish :path ,(format nil "/~(~a~)" name) :function ',name)))))


(defun param-bindings (function-name request params)
  (loop for param in params
     collect (param-binding function-name request param)))

(defun param-binding (function-name request param)
  (destructuring-bind (name type &optional default sticky) param
    (let ((query-name (symbol->query-name name))
          (cookie-name (symbol->cookie-name function-name name sticky)))
      `(,name (or
               (string->type ',type (request-query-value ,query-name ,request))
               ,@ (if cookie-name
                      (list `(string->type ',type
                                           (get-cookie-value ,request ,cookie-name))))
                  ,default)))))

(defgeneric string->type (type value))

(defmethod string->type ((type (eql 'string)) value)
  (and (plusp (length value)) value))

(defmethod string->type ((type (eql 'integer)) value)
  (parse-integer (or value "") :junk-allowed t))

(defun get-cookie-value (request name)
  (cdr (assoc name (get-cookie-values request) :test #'string=)))

(defun symbol->query-name (sym)
  (string-downcase sym))

(defun symbol->cookie-name (function-name sym sticky)
  (let ((package-name (package-name (symbol-package function-name))))
    (when sticky
      (ecase sticky
        (:global
         (string-downcase sym))
        (:package
         (format nil "~(~a:~a~)" package-name sym))
        (:local
         (format nil "~(~a:~a:~a~)" package-name function-name sym))))))

(defun set-cookies-code (function-name request params)
  (loop for param in params
     when (set-cookie-code function-name request param) collect it))

(defun set-cookie-code (function-name request param)
  (destructuring-bind (name type &optional default sticky) param
    (declare (ignore type default))
    (if sticky
        `(when ,name
           (set-cookie-header
            ,request
            :name ,(symbol->cookie-name function-name name sticky)
            :value (printc-to-string ,name))))))
