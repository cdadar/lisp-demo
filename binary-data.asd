(defpackage :com.chens.binary-data-system (:use :asdf :cl))
(in-package :com.chens.binary-data-system)

(defsystem binary-data
  :name "binary-data"
  :author "chens"
  :version "1.0"
  :maintainer "chens"
  :licence "BSD"
  :description "Parser for binary data files. "
  :long-description ""
  :components
  ((:file "binary-data"))
  :depends-on (:macro-utilities))
