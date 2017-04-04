(defpackage :com.chens.shoutcast
  (:use :common-lisp
        :net.aserve
        :com.chens.id3v2)
  (:export :song
           :file
           :title
           :id3-size
           :find-song-source
           :current-song
           :still-current-p
           :maybe-move-to-next-song
           :*song-source-type*))

(in-package :com.chens.shoutcast)

(defgeneric current-cong (source)
  (:documentation "Return the currently playing song or NIL."))

(defgeneric maybe-move-to-next-song (song source)
  (:documentation
   "If the given song is still the current one update the value returned by current-song"))

(defgeneric still-current-p (song source)
  (:documentation
   "Return true if the song given is the same as the current-song"))

(defgeneric find-song-source (type request)
  (:documentation "Find the song-source of the given type for the given request."))

(defclass song ()
  ((file :reader file :initarg :file)
   (title :reader title :initarg :title)
   (id3-size :reader id3-size :initarg :id3-size)))

(defclass simple-song-queue ()
  ((songs :accessor songs :initform (make-array 10 :adjustable t :fill-pointer 0))
   (index :accessor index :initform 0)))

(defparameter *songs* (make-instance 'simple-song-queue))

(defmethod find-song-source ((type (eql 'singleton)) request)
  (declare (ignore request))
  *songs*)

(defmethod current-song ((source simple-song-queue))
  (when (array-in-bounds-p (songs source) (index source))
    (aref (songs source) (index source))))

(defmethod still-current-p (song (source simple-song-queue))
  (eql song (current-song source)))

(defmethod maybe-move-to-next-song (song (source simple-song-queue))
  (when (still-current-p song source)
    (incf (index source))))

(defun add-file-to-songs (file)
  (vector-push-extend (file->song file) (songs *songs*)))

(defun file->song (file)
  (let ((id3 (read-id3 file)))
    (make-instance
     'song
     :file (namestring (truename file))
     :title (format nil "~a by ~a from ~a" (song id3) (artist id3) (album id3))
     :id3-size (size id3))))


(defun shoutcast (request entity)
  (with-http-response
      (request entity :content-type "audio/MP3" :timeout *timeout-seconds*)
    (prepare-icy-response request *metadata-interval*)
    (let ((wants-metadata-p (header-slot-value request :icy-metadata)))
      (with-http-body (request entity)
        (play-songs
         (request-socket request)
         (find-song-source *song-source-type* request)
         (if wants-metadata-p *metadata-interval*))))))

(publish :path "/stream.mp3" :function 'shoutcast)

(defparameter *timeout-seconds* (* 60 60 24 7 52 10))

(defun prepare-icy-response (request metadata-interval)
  (setf (request-reply-protocol-string request) "ICY")
  (loop for (k v) in (reverse
                      `((:|icy-metaint| ,(princ-to-string metadata-interval))
                        (:|icy-notice1| "<BR>This stream blah blah blah<BR>")
                        (:|icy-notice2| "More blah")
                        (:|icy-name| "MyLispShoutcaseServer")
                        (:|icy-genre| "Unknown")
                        (:|icy-url| ,(request-uri request))
                        (:|icy-pub| "1")))
     do (setf (reply-header-slot-value request k) v))
  ;;iTunes,despite claiming to speak HTTP/1.1,doesn't understand
  ;;chunked Transfer-encoding.Grrr. So we just turn it official
  (turn-off-chunked-transfer-encoding request))

(defun turn-off-chunked-transfer-encoding (request)
  (setf (request-reply-strategy request)
        (remove :chunked (request-reply-strategy request))))

(defparameter *metadata-interval* (expt 2 12))

(defparameter *song-source-type* 'singleton)
