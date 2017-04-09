(defpackage :com.chens.pathnames-system (:use :asdf :cl))
(in-package :com.chens.pathnames-system)

(defsystem pathnames
  :name "pathnames"
  :author "chens"
  :version "1.0"
  :maintainer "chens"
  :licence "BSD"
  :description "Portable pathname manipulation functions."
  :long-description ""
  :components
  ((:file "pathnames")))
