(defpackage :com.chens.id3v2-system (:use :asdf :cl))
(in-package :com.chens.id3v2-system)

(defsystem id3v2
  :name "id3"
  :author "chens"
  :version "1.0"
  :maintainer "chens"
  :licence "BSD"
  :description "ID3v2 parser. "
  :long-description ""
  :components
  ((:file "id3v2"))
  :depends-on (:binary-data :pathnames))
