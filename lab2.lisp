(defun merge-lists-spinning-pairs (list1 list2)
  (when (or list1 list2)
      (let ((x (cond
                 ((null list1) (list (car list2)))
                 ((null list2) (list (car list1)))
                 (t (list (car list1) (car list2))))))
        (cons x (merge-lists-spinning-pairs (cdr list2) (cdr list1))))))

(defun is-in-list (elem lst)
  (cond
    ((null lst) nil)
    ((eql (car lst) elem) t)
    (t (is-in-list elem (cdr lst)))))



(defun list-set-intersect-p (list1 list2)
  (when list1
    (if (is-in-list (car list1) list2) t (list-set-intersect-p (cdr list1) list2))
    ))


(defun check-function (name func args expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (apply func args) expected)
          name))

(defun test-merge-lists-spinning-pairs ()
  (check-function "Test1" 'merge-lists-spinning-pairs '(() ()) nil)
  (check-function "Test2" 'merge-lists-spinning-pairs '((1 2 3 4 5) (a b c d)) '((1 A) (B 2) (3 C) (D 4) (5)))
  (check-function "Test3" 'merge-lists-spinning-pairs '((1 2 3 4) (4 5 6)) '((1 4) (5 2) (3 6) (4))))


(defun test-list-set-intersect-p ()
  (check-function "Test1" 'list-set-intersect-p '((1 2 3) (4 5 6)) nil)
  (check-function "Test2" 'list-set-intersect-p '((1 2 3 4 5) (nil)) nil)
  (check-function "Test3" 'list-set-intersect-p '((1 2 3 4) (4 5 6)) t))
