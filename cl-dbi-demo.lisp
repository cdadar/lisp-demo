(ql:quickload :cl-dbi)


(defvar *connection*
  (dbi:connect :mysql :database-name "bszb" :username "bszb" :password "bszbBase2016" :host "120.26.120.234" :charset "utf-8" :encoding "utf-8"))


(let* ((query (dbi:prepare *connection*
                           "SELECT * FROM core_account  WHERE CRACT_STATUS = ? and  CRACT_CDATE > ?"))
       (result (dbi:execute query 1 "2011-11-01")))
  (loop for row = (dbi:fetch result)
     while row
     ;; process "row".
     do (format t "~A~%" row)))

;; (dbi:with-connection (conn :mysql :database-name "bszb" :username "bszb" :password "bszbBase2016" :host "120.26.120.234")
;;   (let* ((query (dbi:prepare conn "SELECT * FROM core_account"))
;;          (result (dbi:execute query)))
;;     (loop for row = (dbi:fetch result)
;;        while row
;;        do (format t "~A~%" row))))
