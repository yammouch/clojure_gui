(defpackage "ROULETTE"
  (:use common-lisp)
  (:nicknames "RLT")
  (:export make-roulette-wheel-selector))
(in-package roulette)

(defmacro abbriv (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(abbriv mvbind multiple-value-bind)

(setq *hoge* '((6 . a) (3 . b) (1 . c)))
;  (sum-up *hoge*)
;->((6 . a) (9 . b) (10 . c))
(defun sum-up (l)
  (labels ((rec (rest sum acc)
             (if (null rest)
               (values (nreverse acc) sum)
               (let ((new-sum (+ sum (caar rest))))
                 (rec (cdr rest)
                      new-sum
                      (cons (cons new-sum (cdar rest))
                            acc))))))
    (mvbind (l sum) (rec l 0 nil)
      (values l sum))))

(defun make-roulette-wheel-selector (l)
  (mvbind (sum-list sum) (sum-up l)
    (lambda ()
      (let ((a (random sum)))
        (cdr (find-if (lambda (x) (> (car x) a))
                      sum-list))))))
