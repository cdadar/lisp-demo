(defpackage :com.chens.html-system (:use :asdf :cl))
(in-package :com.chens.html-system)

(defsystem html
  :name "html"
  :author "chens"
  :version "1.0"
  :maintainer "chens"
  :licence "BSD"
  :description "HTML and CSS generation from sexps."
  :long-description ""
  :components
  ((:file "html"))
  :depends-on (:macro-utilities))
