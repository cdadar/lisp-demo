(defpackage :com.chens.url-function-system (:use :asdf :cl))
(in-package :com.chens.url-function-system)

(require :aserve)

(defsystem url-function
  :name "url-function"
  :author "chens"
  :version "0.1"
  :maintainer "chens"
  :licence "BSD"
  :description "define-url-function macro for AllegroServe"
  :long-description ""
  :components
  ((:file "html-infrastructure"))
  :depends-on (:html :macro-utilities))
