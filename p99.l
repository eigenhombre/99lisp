;; 99 Common Lisp Problems, solved by a Clojurian
;; See https://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html

(defun test= (a b)
  (assert (equal a b)))

(defun take (n l)
  (cond
    ((not l) nil)
    ((< n 1) ())
    (t (cons (car l) (take (- n 1) (cdr l))))))

(test= (take 2 '(a b c)) '(a b))
(test= (take 4 '(a b c)) '(a b c))

(defun drop (n l)
  (if (< n 1)
      l
      (drop (- n 1) (cdr l))))

(test= (drop 2 '(a b c)) '(c))


;; P01 (*) Find the last box of a list.
(defun my-last (l)
  (take 1 (reverse l)))

(test= (my-last '(a b c d))
       '(D))


;; P02 (*) Find the last but one box of a list.
(defun my-but-last (l)
  (reverse (take 2 (reverse l))))

(test= (my-but-last '(a b c d))
       '(C D))


;; P03 (*) Find the K'th element of a list.
(defun element-at (l n)
  (if (= n 1)
      (car l)
      (element-at (cdr l) (- n 1))))

(test= (element-at '(a b c d e) 3)
       'c)


;; P04 (*) Find the number of elements of a list.
(defun my-count (l)
  (if l
      (+ 1 (my-count (cdr l)))
      0))

(test= (my-count '(1 2 3 4))
       4)


;; P05 (*) Reverse a list.
(defun my-reverse (l)
  (if l
      (concatenate 'list (my-reverse (cdr l)) (list (car l)))
      nil))

(test= (my-reverse '(1 2 3))
       '(3 2 1))


;; P06 (*) Find out whether a list is a palindrome.
(defun palindrome? (l)
  (equal l (reverse l)))

(test= (palindrome? '(1 2 3))
       NIL)
(test= (palindrome? '(3 2 3))
       T)

;; P07 (**) Flatten a nested list structure.
(defun my-flatten (l)
  (cond
    ((equal l ()) ())
    ((atom (car l)) (cons (car l) (my-flatten (cdr l))))
    ((consp (car l)) (concatenate 'list (my-flatten (car l)) (my-flatten (cdr l))))
    (t l)))

(test= (my-flatten ()) nil)
(test= (my-flatten '(a)) '(a))
(test= (my-flatten '(a b)) '(a b))
(test= (my-flatten '(a (b (c d) e))) '(A B C D E))


;; P08 (**) Eliminate consecutive duplicates of list elements.
;; Ideally I'd do this in Clojure with the core sequence functions, e.g.
;; (->> x (partition-by identity) (map first))
(identity 'x) ;;=> 'X

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

(test= (take-while #'evenp '(2 2 2 2 2 3 2 2 2)) '(2 2 2 2 2))
(test= (take-while #'evenp '(1 1 1 2)) nil)
(test= (drop-while #'evenp nil) nil)
(test= (drop-while #'evenp '(2 4)) nil)
(test= (drop-while #'evenp '(2 4 6 3 1 2)) '(3 1 2))

(defun partition-by (f l)
  (if (not l)
      nil
      (let ((fcl (funcall f (car l))))
        (cons (take-while #'(lambda (x) (eql (funcall f x) fcl)) l)
              (partition-by f
                            (drop-while #'(lambda (x)
                                            (eql (funcall f x) fcl)) l))))))

(test= (partition-by #'evenp '(1 2 3 4 5))
       '((1) (2) (3) (4) (5)))
(test= (partition-by #'identity '(1 1 1 2 3))
       '((1 1 1) (2) (3)))
(test= (partition-by #'evenp '(2 2 2 3 3 3 3 4 4 4 4))
       '((2 2 2) (3 3 3 3) (4 4 4 4)))

(defun compress (l)
  (mapcar #'car (partition-by #'identity l)))

(test= (compress '(1 1 2 2 2 3 4 4 5 6 7 7 8))
       '(1 2 3 4 5 6 7 8))
(test= (compress '(a a a a b c c a a d e e e e))
       '(A B C A D E))


;; P09 (**) Pack consecutive duplicates of list elements into sublists.
(defun pack (l)
  (partition-by #'identity l))

(test= (pack '(a a a a b c c a a d e e e e))
       '((A A A A) (B) (C C) (A A) (D) (E E E E)))

;; P10 (*) Run-length encoding of a list.
(defun encode (l)
  (labels ((f (ll)
             (if (not ll)
                 nil
                 (let ((cl (car ll)))
                   (cons (list (length cl) (car cl))
                         (f (cdr ll)))))))
    (f (partition-by #'identity l))))

(test= (encode '(a a a a b c c a a d e e e e))
       '((4 A) (1 B) (2 C) (2 A) (1 D) (4 E)))


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

(test= (encode-modified '(a a a a b c c a a d e e e e))
       '((4 A) B (2 C) (2 A) D (4 E)))


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

(test= (decode-rle '((4 A) B (2 C) (2 A) D (4 E)))
       '(A A A A B C C A A D E E E E))

;; P13 (**) Run-length encoding of a list (direct solution).
(defun addlast (el l)
  ;; FIXME: Slow?
  (append l (list el)))

(defun encode-direct (l)
  (labels ((f (pair ll ret)
             (let* ((cl (car ll))
                    (cl1 (list 1 cl)))
               (cond
                 ((not ll) ret)
                 ;; first time:
                 ((not pair) (f cl1
                                (cdr ll)
                                (addlast cl1 ret)))
                 ;; repeat of previous item:
                 ((equal (car ll) (cadr pair))
                  (let ((newpair (list (+ 1 (car pair))
                                       (car ll))))
                    (f newpair
                       (cdr ll)
                       (addlast newpair (butlast ret)))))
                 ;; new item, make new pair:
                 (t (f cl1 (cdr ll) (addlast cl1 ret))))))
           (compress (l)
             (mapcar #'(lambda (x) (if (= 1 (car x)) (cadr x) x))
                     l)))
    (compress (f nil l nil))))

(test= (encode-direct '(a a a a b c c a a d e e e e))
       '((4 A) B (2 C) (2 A) D (4 E)))


;; P14 (*) Duplicate the elements of a list.
(defun juxt (&rest fs)
  (lambda (x)
    (mapcar #'(lambda (f) (funcall f x)) fs)))

(test= (apply #'append
              (mapcar (juxt #'identity #'identity)
                      '(a b c c d)))
       '(A A B B C C C C D D))


;; P15 (**) Replicate the elements of a list a given number of times.
(defun repli (l n)
  (apply #'append (mapcar #'(lambda (x)
                              (repeat n x))
                          l)))

(test= (repli '(a b c) 3)
       '(A A A B B B C C C))


;; P16 (**) Drop every N'th element from a list.
(defun partition-all (n l)
  ;; FIXME: do 3-ary version: (partition n l ofs)
  (labels ((f (ll ret)
             (if (not ll)
                 ret
                 (f (drop n ll)
                    (append ret (list (take n ll)))))))
    (f l nil)))

(test= (partition-all 3 '(a b c d e f g h i k))
       '((A B C) (D E F) (G H I) (K)))

(defun drop-nth (l n)
  (apply #'append
         (mapcar #'(lambda (x)
                     (if (= (length x) n)
                         (butlast x)
                         x))
                 (partition-all n l))))

(test= (drop-nth '(a b c d e f g h i k) 3)
       '(A B D E G H K))
