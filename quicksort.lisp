(defun filter-< (lst pivot)
  (remove-if-not (lambda (x)
                   (< x pivot)) lst))

(defun quick-sort (lst)
  (if (null (cdr lst)) lst
      (let ((pivot (car lst))
            (else (cdr lst)))
        (append
         (quick-sort (filter-< else pivot))
         (list pivot)
         (quick-sort (set-difference
                      else
                      (filter-< else pivot)))))))


(quick-sort '(1  4  8  2  55  3  4  8  6  4  0  11  34  90  23  54  77  9  2  9  4  10))
