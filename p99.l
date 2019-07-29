;; 99 Common Lisp Problems, solved by a Clojurian

(defun take (n l)
  (if (< n 1)
      ()
      (cons (car l) (take (- n 1) (cdr l)))))

;; P01 (*) Find the last box of a list.
(defun my-last (l)
  (take 1 (reverse l)))

(my-last '(a b c d))
;;=>
'(D)

;; P02 (*) Find the last but one box of a list.
(defun my-but-last (l)
  (reverse (take 2 (reverse l))))

(my-but-last '(a b c d))
;;=>
'(C D)

;; P03 (*) Find the K'th element of a list.
(defun element-at (l n)
  (if (= n 1)
      (car l)
      (element-at (cdr l) (- n 1))))
(element-at '(a b c d e) 3)

;; P04 (*) Find the number of elements of a list.
(defun my-count (l)
  (if l
      (+ 1 (my-count (cdr l)))
      0))

(my-count '(1 2 3 4))
;;=>
'4

;; P05 (*) Reverse a list.
(defun my-reverse (l)
  (if l
      (concatenate 'list (my-reverse (cdr l)) (list (car l)))
      nil))

(my-reverse '(1 2 3))

;; P06 (*) Find out whether a list is a palindrome.
(defun palindrome? (l)
  (equal l (reverse l)))

(palindrome? '(1 2 3))
;;=>
NIL
(palindrome? '(3 2 3))
;;=>
T

;; P07 (**) Flatten a nested list structure.
(defun my-flatten (l)
  (cond
    ((equal l ()) ())
    ((atom (car l)) (cons (car l) (my-flatten (cdr l))))
    ((consp (car l)) (concatenate 'list (my-flatten (car l)) (my-flatten (cdr l))))
    (t l)))

(my-flatten ())
(my-flatten '(a))
(my-flatten '(a b))
(my-flatten '(a (b (c d) e)))
;;=>
'(A B C D E)

;; P08 (**) Eliminate consecutive duplicates of list elements.
;; Ideally I'd do this in Clojure with the core sequence functions, e.g.
;; (->> x (partition-by identity) (map first))
(identity 'x)
;;=>
'X
(defun take-while (f l)
  (cond
    ((not l) nil)
    ((not (funcall f (car l))) nil)
    ((cons (car l) (take-while f (cdr l))))))
(defun drop-while (f l)
  (cond
    ((not l) nil)
    ((not (funcall f (car l))) l)
    (t (drop-while f (cdr l)))))

(take-while #'evenp '(2 2 2 2 2 3 2 2 2))
(take-while #'evenp '(1 1 1 2))
(drop-while #'evenp nil)
(drop-while #'evenp '(2 4))
(drop-while #'evenp '(2 4 6 3 1 2))
;;=>
'(3 1 2)

(defun partition-by (f l)
  (if (not l)
      nil
      (let ((fcl (funcall f (car l))))
        (cons (take-while #'(lambda (x) (eql (funcall f x) fcl)) l)
              (partition-by f (drop-while #'(lambda (x) (eql (funcall f x) fcl)) l))))))

(eql (+ 1 1) 2)
(eql 'a 'a)
(partition-by #'evenp '(1 2 3 4 5))
(partition-by #'identity '(1 1 1 2 3))
(partition-by #'evenp '(2 2 2 3 3 3 3 4 4 4 4))
;;=>
'((2 2 2) (3 3 3 3) (4 4 4 4))

(defun compress (l)
  (mapcar #'car (partition-by #'identity l)))

(compress '(1 1 2 2 2 3 4 4 5 6 7 7 8))
;;=>
'(1 2 3 4 5 6 7 8)
(compress '(a a a a b c c a a d e e e e))
;;=>
'(A B C A D E)

;; P09 (**) Pack consecutive duplicates of list elements into sublists.
(defun pack (l)
  (partition-by #'identity l))
(pack '(a a a a b c c a a d e e e e))
;;=>
'((A A A A) (B) (C C) (A A) (D) (E E E E))

;; P10 (*) Run-length encoding of a list.
(length '(1 2 3))
;;=>
'3

(defun encode (l)
  (labels ((f (ll)
             (if (not ll)
                 nil
                 (let ((cl (car ll)))
                   (cons (list (length cl) (car cl))
                         (f (cdr ll)))))))
    (f (partition-by #'identity l))))

(encode '(a a a a b c c a a d e e e e))
;;=>
'((4 A) (1 B) (2 C) (2 A) (1 D) (4 E))

;; P11 (*) Modified run-length encoding.
(defun encode-modified (l)
  (labels ((f (ll)
             (if (not ll)
                 nil
                 (let* ((cl (car ll))
                        (ccl (car cl))
                        (ncl (length cl)))
                   (cons (if (= 1 ncl)
                             ccl
                             (list (length cl) ccl))
                         (f (cdr ll)))))))
    (f (partition-by #'identity l))))
(encode-modified '(a a a a b c c a a d e e e e))
;;=>
'((4 A) B (2 C) (2 A) D (4 E))

;; P12 (**) Decode a run-length encoded list.
(defun repeat (n el) (make-list n :initial-element el))
(defun decode-rle (l)
  ;; FIXME: get (or write) thread-last macro...
  (apply #'concatenate (cons 'list
                             (mapcar #'(lambda (x)
                                         (if (consp x)
                                             (repeat (car x) (cadr x))
                                             (list x)))
                                     l))))

(decode-rle '((4 A) B (2 C) (2 A) D (4 E)))

;;=>
'(A A A A B C C A A D E E E E)
