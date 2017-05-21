(in-package :cl-user)

(defpackage :com.chens.mp3-browser
  (:use :common-lisp
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
                :with-process-lock)
  (:export :start-mp3-browser))


;; Set this variable to the filename of an MP3 of silence.
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

(defmacro with-playlist-locked ((playlist) &body body)
  `(with-process-lock ((lock ,playlist))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; find-song-source

(defvar *playlists* (make-hash-table :test #'equal))
(defparameter *playlists-lock* (make-process-lock :name "playlists-lock"))

(defmethod find-song-source ((type (eql 'playlist)) request)
  (let ((playlist (lookup-playlist (playlist-id request))))
    (with-playlist-locked (playlist)
      (let ((user-agent (header-slot-value request :user-agent)))
        (when user-agent (setf (user-agent playlist) user-agent))))
    playlist))

(defun lookup-playlist (id)
  (with-process-lock (*playlists-lock*)
    (or (gethash id *playlists*)
        (setf (gethash id *playlists*) (make-instance 'playlist :id id)))))

(defun playlist-id (request)
  (ipaddr-to-dotted (remote-host (request-socket request))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; song-source implementation

(defmethod current-song :around ((playlist playlist))
  (with-playlist-locked (playlist) (call-next-method)))

(defmethod still-current-p (song (playlist playlist))
  (with-playlist-locked (playlist)
    (eql song (current-song playlist))))

(defmethod maybe-move-to-next-song (song (playlist playlist))
  (with-playlist-locked (playlist)
    (when (still-current-p song playlist)
      (unless (at-end-p playlist)
        (ecase (repeat playlist)
          (:song) ; nothing changes
          (:none (incf (current-idx playlist)))
          (:all  (setf (current-idx playlist)
                       (mod (1+ (current-idx playlist))
                            (table-size (songs-table playlist)))))))
      (update-current-if-necessary playlist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internals

;;; update-current-if-necessary

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

(defun reset-current-song (playlist)
  (setf
   (current-song playlist) 
   (cond
     ((empty-p playlist) *empty-playlist-song*)
     ((at-end-p playlist) *end-of-playlist-song*)
     (t (row->song (nth-row (current-idx playlist) (songs-table playlist)))))))

(defun row->song (song-db-entry)
  (with-column-values (file song artist album id3-size) song-db-entry
                      (make-instance
                       'song
                       :file file
                       :title (format nil "~a by ~a from ~a" song artist album)
                       :id3-size id3-size)))

(defun empty-p (playlist)
  (zerop (table-size (songs-table playlist))))


;;; Playlist manipulation functions called from mp3-browser.lisp

(defun add-songs (playlist column-name values)
  (let ((table (make-instance
                'table 
                :schema (extract-schema (list column-name) (schema *mp3s*)))))
    (dolist (v values) (insert-row (list column-name v) table))
    (do-rows (row (select :from *mp3s* :where (in column-name table)))
      (insert-row row (songs-table playlist))))
  (update-current-if-necessary playlist))

(defun delete-songs (playlist &rest names-and-values)
  (delete-rows
   :from (songs-table playlist)
   :where (apply #'matching (songs-table playlist) names-and-values))
  (setf (current-idx playlist) (or (position-of-current playlist) 0))
  (update-current-if-necessary playlist))

(defun clear-playlist (playlist)
  (delete-all-rows (songs-table playlist))
  (setf (current-idx playlist) 0)
  (update-current-if-necessary playlist))

(defun sort-playlist (playlist ordering)
  (setf (ordering playlist) ordering)
  (setf (shuffle playlist) :none)
  (order-playlist playlist)
  (setf (current-idx playlist) (position-of-current playlist)))

(defun shuffle-playlist (playlist shuffle)
  (setf (shuffle playlist) shuffle)
  (case shuffle
    (:none (order-playlist playlist))
    (:song (shuffle-by-song playlist))
    (:album (shuffle-by-album playlist)))
  (setf (current-idx playlist) (position-of-current playlist)))

(defmethod (setf repeat) :after (value (playlist playlist))
  (if (and (at-end-p playlist) (not (empty-p playlist)))
      (ecase value
        (:song (setf (current-idx playlist) (1- (table-size (songs-table playlist)))))
        (:none)
        (:all  (setf (current-idx playlist) 0)))
      (update-current-if-necessary playlist)))

;;; Shuffling helpers

(defun position-of-current (playlist)
  (let* ((table (songs-table playlist))
         (matcher (matching table :file (file (current-song playlist))))
         (pos 0))
    (do-rows (row table)
      (when (funcall matcher row)
        (return-from position-of-current pos))
      (incf pos))))

(defun order-playlist (playlist)
  (apply #'sort-rows (songs-table playlist)
         (case (ordering playlist)
           (:genre  '(:genre :album :track))
           (:artist '(:artist :album :track))
           (:album  '(:album :track))
           (:song   '(:song)))))

(defun shuffle-by-song (playlist)
  (shuffle-table (songs-table playlist)))

(defun shuffle-by-album (playlist)
  (let ((new-table (make-playlist-table)))
    (do-rows (album-row (shuffled-album-names playlist))
      (do-rows (song (songs-for-album playlist (column-value album-row :album)))
        (insert-row song new-table)))
    (setf (songs-table playlist) new-table)))

(defun shuffled-album-names (playlist)
  (shuffle-table 
   (select
    :columns :album
    :from (songs-table playlist)
    :distinct t)))

(defun songs-for-album (playlist album)
  (select
   :from (songs-table playlist) 
   :where (matching (songs-table playlist) :album album)
   :order-by :track))


(defvar *major-version* 1)
(defvar *minor-version* 0)

(defparameter *mp3-dir* nil)

(defparameter *mp3-css* 
  (when *load-pathname* (make-pathname :name "mp3-browser" :type "css" :defaults *load-pathname*)))

(defun configure-mp3-browser (&optional force)
  (unless (or *mp3-dir* force)
    (format t "Enter root directory of MP3 collection: ")
    (force-output *standard-output*)
    (setf *mp3-dir* (read)))
  (unless (or *mp3-css* force)
    (format t "Enter full filename of mp3-browser.css: ")
    (force-output *standard-output*)
    (setf *mp3-css* (read))))

(defun start-mp3-browser ()
  (unless (and *mp3-dir* *mp3-css*)
    (configure-mp3-browser))
  (load-database *mp3-dir* *mp3s*)
  (publish-file :path "/mp3-browser.css"  :file *mp3-css* :content-type "text/css")
  (setf *song-source-type* 'playlist)
  (net.aserve::debug-on :notrap)
  (net.aserve:start :port 2001))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parameter types for url functions

(defmethod string->type ((type (eql 'integer)) value)
  (parse-integer (or value "") :junk-allowed t))

(defmethod string->type ((type (eql 'keyword)) value)
  (and (plusp (length value)) (intern (string-upcase value) :keyword)))

(defun safe-read-from-string (string)
  (let ((*read-eval* nil)) (ignore-errors (read-from-string string))))

(defmethod string->type ((type (eql 'base-64-list)) value)
  (let ((obj (base64->obj value)))
    (if (listp obj) obj nil)))

(defmacro with-safe-io-syntax (&body body)
  `(with-standard-io-syntax
     (let ((*read-eval* nil))
       ,@body)))

(defun obj->base64 (obj)
  (base64-encode (with-safe-io-syntax (write-to-string obj))))

(defun base64->obj (string)
  (ignore-errors
    (with-safe-io-syntax (read-from-string (base64-decode string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standard page layout

(define-html-macro :mp3-browser-page ((&key title (header title)) &body body)
  `(:html
    (:head
     (:title ,title)
     (:link :rel "stylesheet" :type "text/css" :href "mp3-browser.css"))
    (:body
     (standard-header)
     (when ,header (html (:h1 :class "title" ,header)))
     ,@body
     (standard-footer))))

(defun link (target &rest attributes)
  (html 
    (:attribute
     (:format "~a~@[?~{~(~a~)=~a~^&~}~]" target (mapcar #'urlencode attributes)))))

(defun urlencode (string)
  (net.aserve::encode-form-urlencoded string))

(defparameter *random-amount* 25)

(defun standard-header ()
  (html 
    ((:p :class "toolbar")
     "[" (:a :href (link "/browse" :what "genre") "All genres") "] "
     "[" (:a :href (link "/browse" :what "genre" :random *random-amount*) "Random genres") "] "
     "[" (:a :href (link "/browse" :what "artist") "All artists") "] "
     "[" (:a :href (link "/browse":what "artist" :random *random-amount*) "Random artists") "] "
     "[" (:a :href (link "/browse":what "album") "All albums") "] "
     "[" (:a :href (link "/browse":what "album" :random *random-amount*) "Random albums") "] "
     "[" (:a :href (link "/browse" :what "song" :random *random-amount*) "Random songs") "] "
     "[" (:a :href (link "/playlist") "Playlist") "] "
     "[" (:a :href (link "/all-playlists") "All playlists") "]")))

(defun standard-footer ()
  (html (:hr) ((:p :class "footer") "MP3 Browser v" *major-version* "." *minor-version*)))

(define-html-macro :table-row (&attributes attrs &rest values)
  `(:tr ,@attrs ,@(loop for v in values collect `(:td ,v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MP3 Browser

(define-url-function browse
    (request (what keyword :genre) genre artist album (random integer))

  (let* ((values (values-for-page what genre artist album random))
         (title (browse-page-title what random genre artist album))
         (single-column (if (eql what :song) :file what))
         (values-string (values->base-64 single-column values)))
    (html
      (:mp3-browser-page
       (:title title)
       ((:form :method "POST" :action "playlist")
        (:input :name "values" :type "hidden" :value values-string)
        (:input :name "what" :type "hidden" :value single-column)
        (:input :name "action" :type "hidden" :value :add-songs)
        (:input :name "submit" :type "submit" :value "Add all"))
       (:ul (do-rows (row values) (list-item-for-page what row)))))))

(define-url-function playlist 
    (request
     (playlist-id string (playlist-id request) :package)
     (action keyword)      ; Playlist manipulation action
     (what keyword :file)  ; for :add-songs action
     (values base-64-list) ;             "
     file                  ; for :add-songs and :delete-songs actions
     genre                 ; for :delete-songs action
     artist                ;             "
     album                 ;             "
     (order-by keyword)    ; for :sort action
     (shuffle keyword)     ; for :shuffle action
     (repeat keyword))     ; for :set-repeat action

  (let ((playlist (lookup-playlist playlist-id)))
    (with-playlist-locked (playlist)

      (case action
        (:add-songs      (add-songs playlist what (or values (list file))))
        (:delete-songs   (delete-songs 
                          playlist 
                          :file file :genre genre
                          :artist artist :album album))
        (:clear          (clear-playlist playlist))
        (:sort           (sort-playlist playlist order-by))
        (:shuffle        (shuffle-playlist playlist shuffle))
        (:set-repeat     (setf (repeat playlist) repeat)))

      (html
        (:mp3-browser-page
         (:title (:format "Playlist - ~a" (id playlist)) :header nil)
         (playlist-toolbar playlist)
         (if (empty-p playlist)
             (html (:p (:i "Empty.")))
             (html 
               ((:table :class "playlist")
                (:table-row "#" "Song" "Album" "Artist" "Genre")
                (let ((idx 0)
                      (current-idx (current-idx playlist)))
                  (do-rows (row (songs-table playlist))
                    (with-column-values (track file song album artist genre) row
                                        (let ((row-style (if (= idx current-idx) "now-playing" "normal")))
                                          (html
                                            ((:table-row :class row-style)
                                             track
                                             (:progn song   (delete-songs-link :file file))
                                             (:progn album  (delete-songs-link :album album))
                                             (:progn artist (delete-songs-link :artist artist))
                                             (:progn genre  (delete-songs-link :genre genre)))))
                                        (incf idx))))))))))))

(define-url-function all-playlists (request)
  (:mp3-browser-page
   (:title "All Playlists")
   ((:table :class "all-playlists")
    (:table-row "Playlist" "# Songs" "Most recent user agent")
    (with-process-lock (*playlists-lock*)
      (loop for playlist being the hash-values of *playlists* do
           (html
             (:table-row
              (:a :href (link "playlist" :playlist-id (id playlist)) (:print (id playlist)))
              (:print (table-size (songs-table playlist)))
              (:print (user-agent playlist)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions

(defun values-for-page (what genre artist album random)
  (let ((values
         (select 
          :from *mp3s*
          :columns (if (eql what :song) t what)
          :where (matching *mp3s* :genre genre :artist artist :album album)
          :distinct (not (eql what :song))
          :order-by (if (eql what :song) '(:album :track) what))))
    (if random (random-selection values random) values)))

(defun browse-page-title (what random genre artist album)
  (with-output-to-string (s)
    (when random (format s "~:(~r~) Random " random))
    (format s "~:(~a~p~)" what random)
    (when (or genre artist album)
      (when (not (eql what :song)) (princ " with songs" s))
      (when genre  (format s " in genre ~a" genre))
      (when artist (format s " by artist ~a" artist))
      (when album  (format s " on album ~a" album)))))

(defun list-item-for-page (what row)
  (if (eql what :song)
      (with-column-values (song file album artist genre) row
                          (html
                            (:li
                             (:a :href (link "playlist" :file file :action "add-songs") (:b song)) " from "
                             (:a :href (link "browse"  :what :song :album  album) album) " by "
                             (:a :href (link "browse" :what :song :artist artist) artist) " in genre "
                             (:a :href (link "browse"  :what :song :genre  genre) genre))))
      (let ((value (column-value row what)))
        (html
          (:li value " - "
               (browse-link :genre  what value)
               (browse-link :artist what value)
               (browse-link :album  what value)
               (browse-link :song   what value))))))

(defun browse-link (new-what what value)
  (unless (eql new-what what)
    (html
      "[" 
      (:a :href (link "browse" :what new-what what value) (:format "~(~as~)" new-what))
      "] ")))

(defun playlist-toolbar (playlist)
  (let ((current-repeat (repeat playlist))
        (current-sort (ordering playlist))
        (current-shuffle (shuffle playlist)))
    (html
      (:p :class "playlist-toolbar"
          (:i "Sort by:")
          " [ "
          (sort-playlist-button "genre" current-sort) " | " 
          (sort-playlist-button "artist" current-sort) " | " 
          (sort-playlist-button "album" current-sort) " | " 
          (sort-playlist-button "song" current-sort) " ] "
          (:i "Shuffle by:")
          " [ "
          (playlist-shuffle-button "none" current-shuffle) " | "
          (playlist-shuffle-button "song" current-shuffle) " | "
          (playlist-shuffle-button "album" current-shuffle) " ] "
          (:i "Repeat:")
          " [ "
          (playlist-repeat-button "none" current-repeat) " | "
          (playlist-repeat-button "song" current-repeat) " | "
          (playlist-repeat-button "all" current-repeat) " ] "
          "[ " (:a :href (link "playlist" :action "clear") "Clear") " ] "))))

(defun playlist-button (action argument new-value current-value)
  (let ((label (string-capitalize new-value)))
    (if (string-equal new-value current-value)
        (html (:b label))
        (html (:a :href (link "playlist" :action action argument new-value) label)))))

(defun sort-playlist-button (order-by current-sort)
  (playlist-button :sort :order-by order-by current-sort))

(defun playlist-shuffle-button (shuffle current-shuffle)
  (playlist-button :shuffle :shuffle shuffle current-shuffle))

(defun playlist-repeat-button (repeat current-repeat)
  (playlist-button :set-repeat :repeat repeat current-repeat))

(defun delete-songs-link (what value)
  (html " [" (:a :href (link "playlist" :action :delete-songs what value) "x") "]"))

(defun values->base-64 (column values-table)
  (flet ((value (r) (column-value r column)))
    (obj->base64 (map-rows #'value values-table))))


