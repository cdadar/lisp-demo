(defpackage :com.chens.mp3-database-system (:use :asdf :cl))
(in-package :com.chens.mp3-database-system)

(defsystem mp3-database
  :name "mp3-database"
  :author "chens"
  :version "1.0"
  :maintainer "chens"
  :licence "BSD"
  :description "In-memory MP3 Database."
  :long-description ""
  :components
  ((:file "mp3-database"))
  :depends-on (:pathnames :macro-utilities :id3v2))
