(defpackage :com.chens.mp3-browser-system (:use :asdf :cl))
(in-package :com.chens.mp3-browser-system)

(require :aserve)

(defclass css-file (static-file) ())
(defmethod source-file-type ((c css-file) (s module)) "css")

(defsystem mp3-browser
  :name "mp3-browser"
  :author "chens"
  :version "1.0"
  :maintainer "chens"
  :licence "BSD"
  :description "AllegroServe-based user interface for Shoutcast server."
  :long-description ""
  :components
  ((:file "mp3-browser")
   (:css-file "mp3-browser"))
  :depends-on (:html :id3v2 :mp3-database :shoutcast :url-function ))
