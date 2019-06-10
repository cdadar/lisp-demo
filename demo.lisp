(defun sum (n) (let ((s 0))
                 (dotimes (i n s)
                   (incf s i))))
