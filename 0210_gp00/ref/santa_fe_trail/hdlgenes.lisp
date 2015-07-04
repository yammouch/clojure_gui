(defpackage hdlgenes
  (:use common-lisp)
  (:nicknames hg)
  (:export crossover count-node replace-node))
(in-package hdlgenes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rcurry (fn &rest args)
  (lambda (&rest args2)
    (apply fn (append args2 args))))

(defmacro abbriv (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(abbriv mvbind multiple-value-bind)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; operators for genes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; g1      _ _      _ _                    _ _
;        |_|_|--->|_|_|----------------->|_|/|
;         |        |                      |
;         v       _v_     _ _     _ _    _v_     _ _     _ _     _ _
; :if-food-ahead |_|_|-->|_|_|-->|_|_|  |_|_|-->|_|_|-->|_|_|-->|_|/|
;                 |       |       |      |       |       |       |
;                 v       v       v      v       v       v       v
;              :prog2   :move   :left :prog3   :left   :move  :right
(setq g1
  '(:if-food-ahead (:prog2 :move :left)
                   (:prog3 :left :move :right)))

(setq g2
  '(:prog2 (:if-food-ahead :move :right)
           (:if-food-ahead :move :left)))

(defun terminal? (x)
  (member x '(:move :left :right)
          :test #'eq))

(defun non-terminal? (x)
  (member x '(:if-food-ahead :prog2 :prog3)
          :test #'eq))

(defun grec-num (term-fn non-term-fn
                 &optional
                 (cons-fn (lambda (x l r i) (cons l r)))
                 (null-fn (lambda (i) nil)))
  (labels ((self (tr i)
             (cond ((null tr) (values (funcall null-fn i) i))
                   ((terminal? tr)
                    (values (funcall term-fn tr i)
                            (1+ i)))
                   ((non-terminal? (car tr))
                    (mvbind (children next-i) (self (cdr tr) (1+ i))
                      (values (funcall non-term-fn tr children i)
                              next-i)))
                   (t (mvbind (child1 next-i1) (self (car tr) i)
                        (mvbind (children next-i2) (self (cdr tr) next-i1)
                          (values (funcall cons-fn tr child1 children i)
                                  next-i2)))))))
    (rcurry #'self 0)))

(defmacro gwith-num (x term-exp non-term-exp
                     &optional
                     (cons-exp '(cons left right))
                     (null-exp 'nil))
  `(funcall (grec-num (lambda (it num) ,term-exp)
                      (lambda (it rec num) ,non-term-exp)
                      (lambda (it left right num) ,cons-exp)
                      (lambda (num) ,null-exp))
            ,x))

(defun number-node (tree)
  (gwith-num tree
    (cons num it)
    (cons num
          (cons (car it) rec))))

(defun count-node (tree)
  (gwith-num tree (1+ num) rec right num))

(defun pickup-node (tree n)
  (gwith-num tree
    (if (= num n) it nil)
    (if (= num n) it rec)
    (if left left right)))

(defun replace-node (tree new-node n)
  (gwith-num tree
    (if (= num n) new-node it)
    (if (= num n)
       new-node
       (cons (car it) rec))))

(defun crossover (t1 t2
                  &optional
                  (pos1 (random (count-node t1)))
                  (pos2 (random (count-node t2))))
  (values (replace-node t1
                        (pickup-node t2 pos2)
                        pos1)
          (replace-node t2
                        (pickup-node t1 pos1)
                        pos2)
          pos1 pos2))
