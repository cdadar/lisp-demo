(defpackage :com.chens.lisp-demo (:use :asdf :cl))
(in-package :com.chens.lisp-demo)

(require 'aserve)

(defsystem lisp-demo
  :name "lisp-demo"
  :author "chens"
  :version "1.0"
  :licence "BSD"
  :description "common lisp demo"
  :depends-on
  (:pathname
   :binary-data
   :macro-utilities
   :id3v2
   :shoutcast)
  :components
  ((:file "file")
   (:file "macro-utilities")
   (:file "binary-data" :depends-on ("macro-utilities"))
   (:file "id3v2" :depends-on ("binding-data"))
   (:file "shoutcast" :depends-on ("id3v2"))))
