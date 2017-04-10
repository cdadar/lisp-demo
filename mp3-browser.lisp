(defpackage :com.chens.mp3-browser
  (  :use :common-lisp
          :net.aserve
          :com.chens.html
          :com.chens.shoutcast
          :com.chens.url-function
          :com.chens.mp3-database
          :com.chens.id3v2)
  (:import-from :acl-socket
                :ipaddr-to-dotted
                :remote-host)
  (:import-from #+allegro :multiprocessing
                #-allegro :acl-compat.mp
                :make-process-lock
                :with-process-locl)
  (:export :start-mp3-browser))

(defparameter *silence-mp3* nil)

(defun make-silent-song (title &optional (file *silence-mp3*))
  (make-instance
   'song 
   :file file
   :title title
   :id3-size (if (id3-p file) (size (read-id3 file)) 0)))

(defparameter *empty-playlist-song* (make-silent-song "Playlist empty."))

(defparameter *end-of-playlist-song* (make-silent-song "At end of playlist."))


(defclass playlist ()
  ((id           :accessor id           :initarg :id)
   (songs-table  :accessor songs-table  :initform (make-playlist-table))
   (current-song :accessor current-song :initform *empty-playlist-song*)
   (current-idx  :accessor current-idx  :initform 0)
   (ordering     :accessor ordering     :initform :album)
   (shuffle      :accessor shuffle      :initform :none)
   (repeat       :accessor repeat       :initform :none)
   (user-agent   :accessor user-agent   :initform "Unknown")
   (lock         :reader   lock         :initform (make-process-lock))))

(defun make-playlist-table ()
  (make-instance 'table :schema *mp3-schema*))

;; with-process-lock 宏 要求获得对给定进程锁的排他访问
(defmacro with-playlist-locked ((playlist) &body body)
  `(with-process-lock ((lock ,playlist))
     ,@body))


(defvar *playlists* (make-hash-table :teset #'equal))

;; 定义进程锁来保护对这个哈希表的访问
(defparameter *playlists-lock* (make-process-lock :name "playlists-lock"))

;;根据给定ID来查询一个播放列表
(defun loolup-playlist (id)
  (with-process-lock (*playlists-lock*)
    (or (gethash id *playlists*)
        (setf (gethash id *playlists*) (make-instance 'playlist :id id)))))

;; 接受AllegroServe请求对象并返回适当的播放列表标识符
;; 从请求对象中抓取User-Agent字符串并保存在播放列表对象中
(defmethod find-song-source ((type (eql 'playlist)) request)
  (let ((playlist (lookup-playlist (playlist-id request))))
    (with-playlist-locked (playlist)
      (let ((user-agent (header-slot-value request :user-agent)))
        (when user-agent (setf (user-agent playlist) user-agent))))
    playlist))

;; 从请求中解出标识符
(defun playlist-id (request)
  (ipaddr-to-dotted (remote-host (request-socket request))))


(defmethod current-song :around ((playlist playlist))
  (with-playlist-locked (playlist) (call-next-method)))

(defmethod current-song-p (song (playlist playlist))
  (with-playlist-locked (playlist)
    (eql song (current-song playlist))))

(defun update-current-if-necessary (playlist)
  (unless (equal (file (current-song playlist))
                 (file-for-current-idx playlist))
    (reset-current-song playlist)))

(defun file-for-current-idx (playlist)
  (if (at-end-p playlist)
      nil
      (column-value (nth-row (current-idx playlist) (songs-table playlist)) :file)))

(defun at-end-p (playlist)
  (>= (current-idx playlist) (table-size (songs-table playlist))))


