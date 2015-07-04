(defpackage "ARTIFICIAL-ANT"
  (:use common-lisp)
  (:nicknames ant)
  (:export make-ant loop-gene))
(in-package ant)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro abbriv (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(abbriv mvbind multiple-value-bind)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the ant class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass ant ()
 ((energy :initform 100 :initarg energy :accessor energy)
  (field  :initarg  :field :accessor field)
  (food :initarg :food :accessor food)
  (x :initform 0 :accessor x)
  (y :initform 0 :accessor y)
  (orientation :initform 'right :accessor orientation)))

(defun make-ant ()
  (let ((field (make-field)))
    (make-instance 'ant
      :field field
      :food (count-food field))))

(defmethod print-object ((a ant) s)
  (format s "#<ant energy:~A food:~A x:~A y:~A orientation:~A>"
    (energy a) (food a) (x a) (y a) (orientation a)))

(defconstant *foods-mat-size* 16)
(defun make-field ()
  (make-array (list *foods-mat-size* *foods-mat-size*)
   :initial-contents
   '((- f f f - - - - - - - - - - - -)
     (- - - - - - - - - - - - - - - -)
     (- - - f - - - - - - - - - - - -)
     (- - - f - - - - - - - - - - - -)
     (- - - - - - - - f f f - - - - -)
     (- - - - - - - f - - - f - - - -)
     (- - - f - - - - - - - - - - - -)
     (- - - f - - - f - - - f - - - -)
     (- - - - - - - f - - - f - - - -)
     (- - - f - - - - - - - - - - - -)
     (- - - f - - - f - - - f - - - -)
     (- - - f - - - f - - - - - - - -)
     (- - - - f f f - - - - f - - - -)
     (- - - - - - - - - - - - f f f -)
     (- - - - - - - - - - - - - - - f)
     (- - - - - - - - - - - - - - - f))))

(defun count-food (field)
  (let ((cnt 0))
    (dotimes (x *foods-mat-size* cnt)
      (dotimes (y *foods-mat-size*)
        (when (eql (aref field y x) 'f)
          (incf cnt))))))

(defmethod up-edge? ((a ant))
  (<= (y a) 0))
(defmethod down-edge? ((a ant))
  (<= (1- *foods-mat-size*) (y a)))
(defmethod left-edge? ((a ant))
  (<= (x a) 0))
(defmethod right-edge? ((a ant))
  (<= (1- *foods-mat-size*) (x a)))

(defmethod ahead ((a ant))
  (ecase (orientation a)
    (up (if (up-edge? a)
          'wall
          (values (x a) (1- (y a)))))
    (down (if (down-edge? a)
            'wall
            (values (x a) (1+ (y a)))))
    (left (if (left-edge? a)
            'wall
            (values (1- (x a)) (y a))))
    (right (if (right-edge? a)
             'wall
             (values (1+ (x a)) (y a))))))

(defmethod move ((a ant))
  (when (> (energy a) 0)
    (decf (energy a))
    (let ((prev-x (x a))
          (prev-y (y a)))
      (mvbind (x y) (ahead a)
        (unless (eql x 'wall)
          (setf (x a) x
                (y a) y)))
      (ecase (aref (field a) prev-y prev-x)
        (- (setf (aref (field a) prev-y prev-x)
                 'w))
        (f (decf (food a))
           (setf (aref (field a) prev-y prev-x)
                 'e))
        (w t) (e t)))
    t))

(defmethod left ((a ant))
  (when (> (energy a) 0)
    (decf (energy a))
    (setf (orientation a)
      (ecase (orientation a)
        (up    'left)
        (left  'down)
        (down  'right)
        (right 'up)))
    t))

(defmethod right ((a ant))
  (when (> (energy a) 0)
    (decf (energy a))
    (setf (orientation a)
      (ecase (orientation a)
        (up    'right)
        (right 'down)
        (down  'left)
        (left  'up)))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gene evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eval-if (then else a)
  (let ((pred (mvbind (x y) (ahead a)
                (if (eql x 'wall)
                  nil
                  (eql (aref (field a) y x)
                       'f)))))
    (if pred
      (eval-gene then a)
      (eval-gene else a))))

(defun eval-prog2 (expr1 expr2 a)
  (and (eval-gene expr1 a)
       (eval-gene expr2 a)))

(defun eval-prog3 (expr1 expr2 expr3 a)
  (and (eval-gene expr1 a)
       (eval-gene expr2 a)
       (eval-gene expr3 a)))

(defun eval-gene (expr a)
  (if (atom expr)
    (ecase expr
      (:move  (move a))
      (:left  (left a))
      (:right (right a)))
    (ecase (car expr)
      (:if-food-ahead (eval-if (cadr expr)
                               (caddr expr)
                               a))
      (:prog2 (eval-prog2 (cadr expr)
                          (caddr expr)
                          a))
      (:prog3 (eval-prog3 (cadr expr)
                          (caddr expr)
                          (cadddr expr)
                          a)))))

(defun loop-gene (expr a)
  (do ()
      ((or (<= (food a) 0)
           (<= (energy a) 0))
       (if (<= (energy a) 0)
         (- (food a))
         (energy a)))
    (eval-gene expr a)))

(setq g1
  '(:if-food-ahead (:prog2 :move
                           :left)
                   (:prog3 :right
                           :move
                           :left)))
(setq g2
  '(:if-food-ahead :move
                   (:prog2 :right :move)))
(setq g3
  '(:prog3 :move :move
           (:prog3 :right :move :left)))
