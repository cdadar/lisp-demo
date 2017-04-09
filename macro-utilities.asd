(defpackage :com.chens.macro-utilities-system (:use :asdf :cl))
(in-package :com.chens.macro-utilities-system)

(defsystem macro-utilities
  :name "macro-utilities"
  :author "chens"
  :version "1.0"
  :maintainer "chens"
  :licence "BSD"
  :description "Utilities for writing macros"
  :long-description ""
  :components
  ((:file "macro-utilities"))
  :depends-on ())
