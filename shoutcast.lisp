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

(defgeneric current-song (source)
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


(defun play-songs (stream song-source metadata-interval)
  (handler-case
      (loop
         for next-metadata = metadata-interval
         then (play-current
               stream
               song-source
               next-metadata
               metadata-interval)
         while next-metadata)
    (error (e) (format *trace-output* "Caught error in play-songs:~a" e))))

;; 从歌曲源中得到当前歌曲,并得到一个缓冲区,含有将要通过传递标题给make-icy-metadata来发送元数据
;; 接着它打开文件并使用两参数形式的file-position跳过id3标签.
;; 然后从文件中读取字节并将它们写到请求的流中
(defun play-current (out song-source next-metadata metadata-interval)
  (let ((song (current-song song-source)))
    (when song
      (let ((metadata (make-icy-metadata (title song))))
        (with-open-file (mp3 (file song) :element-type '(unsigned-byte 8))
          (unless (file-position mp3 (id3-size song))
            (error "Can't skip to position ~d in ~a" (id3-size song) (file song)))
          (loop for byte = (read-byte mp3 nil nil)
             while (and byte (still-current-p song song-source)) do
               (write-byte byte out)
               (decf next-metadata)
             when (and (zerop next-metadata) metadata-interval) do
               (write-sequence metadata out)
               (setf next-metadata metadata-interval))
          (maybe-move-to-next-song song song-source)))
      next-metadata)))




;; 接受当前歌曲的标题,并生成一个含有正确格式化的icy元数据片段的字节数据
(defun make-icy-metadata (title)
  (let* ((text (format nil "StreamTitle='~a';" (substitute #\Space #\' title)))
         (blocks (ceiling (length text) 16))
         (buffer (make-array (1+ (* blocks 16))
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (setf (aref buffer 0) enable-package-locks)
    (loop
       for char across  text
       for i from 1
       do (setf (aref buffer i) (char-code char)))
    buffer))

;; 高效版play-current
(defun play-current (out song-source next-metadata metadata-interval)
  (let ((song (current-song song-source)))
    (when song
      (let ((metadata (make-icy-metadata (title song)))
            (buffer (make-array size :element-type '(unsigned-byte 8))))
        (with-open-file (mp3 (file song))
          (labels ((write-buffer (start end)
                     (if metadata-interval
                         (write-buffer-with-metadata start end)
                         (write-sequence buffer out :start start :end end)))

                   (write-buffer-with-metadata (start end)
                     (cond
                       ((> next-metadata (- end start))
                        (write-sequence buffer out :start start :end end)
                        (decf next-metadata (- end start)))
                       (t
                        (let ((middle (+ start next-metadata)))
                          (write-sequence buffer out :start start :end middle)
                          (write-sequence metadata out)
                          (setf next-metadata metadata-interval)
                          (write-buffer-with-metadata middle end))))))

            (multiple-value-bind (skip-blocks skip-bytes)
                (floor (id3-size song) (length buffer))

              (unless (file-position mp3 (* skip-blocks (length buffer)))
                (error "Couldn't skip over ~d ~d byte blocks."
                       skip-blocks (length buffer)))

              (loop for end = (read-sequence buffer mp3)
                 for start = skip-bytes then 0
                 do (write-buffer start end)
                 while (and (= end (length buffer))
                            (still-current-p song song-source)))

              (maybe-move-to-next-song song song-source)))))
      next-metadata)))
