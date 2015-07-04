(load "go-ant.lisp")
(load "roulette.lisp")
(load "hdlgenes.lisp")
(load "random-tree.lisp")

(defconstant *pool-size* 2000)
(defconstant *gene-size-limit* 100)
(defconstant *mutation-prob* 1/100) ; must be rational

(defun init-pool ()
  (let ((retval nil))
    (dotimes (_ *pool-size* retval)
      (push (rtr:gen-tree 6)
            retval))))

(defun score-genes (pool)
  (mapcar (lambda (gene)
            (cons (+ 31 ; initial food number
                     (ant:loop-gene gene (ant:make-ant)))
                  gene))
          pool))

(defun mutate (gene)
  (let ((nn (hg:count-node gene))
        (mutant-src (rtr:gen-tree 6)))
    (hg:replace-node gene mutant-src (random nn))))

(defun next-pool (scored-pool)
  (do ((roulette (rlt:make-roulette-wheel-selector scored-pool))
       (num 0)
       retval)
      ((>= num *pool-size*) retval)
    (let* ((mother (funcall roulette))
           (father (funcall roulette))
           (child (hg:crossover mother father)))
      (when (< (random (denominator *mutation-prob*))
               (numerator *mutation-prob*))
        (setq child (mutate child)))
      (when (<= (hg:count-node child) *gene-size-limit*)
        (push child retval)
        (incf num)))))

(defun main-loop (num-iter)
  (let (scored)
    (dotimes (i num-iter)
      (setq scored (score-genes
                     (if (= i 0)
                       (init-pool)
                       (next-pool scored))))
      (format t "max score at generation ~A: ~A~%"
        i (apply #'max (mapcar #'car scored))))
    scored))
