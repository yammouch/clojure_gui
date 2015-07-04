(defpackage "RANDOM-TREE"
            (:use "COMMON-LISP")
            (:nicknames "RTR")
            (:export gen-tree
             *terminal* *non-terminal*))
(in-package "RANDOM-TREE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro abbriv (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(abbriv mvbind multiple-value-bind)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; random tree generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *terminal* '(:move :left :right))
(setq *non-terminal*
  '((:if-food-ahead 2)
    (:prog2 2)
    (:prog3 3)))

(defun terminal ()
  (nth (random (length *terminal*))
       *terminal*))

(defun non-terminal ()
  (let ((choice (nth (random (length *non-terminal*))
                     *non-terminal*)))
    (values (car choice) (cadr choice))))

(defun distribute (i n) ; distributes i into n-element array
  (let ((retval (make-array n :initial-element 0)))
    (dotimes (_ i (coerce retval 'list))
      (incf (aref retval (random n))))))

(defun gen-tree (n-nt) ; n-nt: number of non-terminal
  (if (<= n-nt 0)
    (terminal)
    (mvbind (nt-symbol n-children) (non-terminal)
      (cons nt-symbol
            (mapcar #'gen-tree
                    (distribute (1- n-nt) n-children))))))
