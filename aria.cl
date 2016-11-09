(proclaim '(optimize (speed 0) (safety 3) (debug 3)))

(ql:quickload '(alexandria iterate anaphora) :silent t)

(defpackage aria
  (:use cl iterate anaphora alexandria))

(in-package aria)

;; TODO
;; Extend to arbitrary, ordered, matching porcedures

;; gonna be printing out a lot of code...
(setf *print-pretty* nil)

(defun symbol-eq (x y)
   "Compares the names of symbols, to prevent package issues"
   (and (symbolp x) (symbolp y) (string= (string x) (string y))))

;; func is the arity determining function
(defun aria (exp func)
   (let ((r (aria* exp func)))
      (if (second r)
          (error "leftover arguments during aria expansion: ~a" (second r))
          (first r))))

(defun aria* (exp func)
   (let ((n (funcall func (car exp))))
      (if n
          (iter (repeat n)
                (with x = (copy-list (cdr exp)))
                (if x 
                    (collect
                       (let ((e (aria* x func)))
                             (setf x (second e))
                             (first e)) into y)
                    (error "too few atoms to satisfy ~a" (car exp)))
                (finally (return (list (append (list (car exp)) y) x))))
          (list (car exp) (cdr exp)))))

(defparameter arity-list
 '((if 3)
   (when 2)
   (+ 2)
   (- 2)
   (* 2)
   (map 2)
   (take 2)
   (range 1)
   (lambda 2)))

(defun from-arity-list (s)
   (aif (assoc s arity-list :test #'symbol-eq) (second it)))

(aria '(if pred a b) #'from-arity-list)
(aria '(if pred + a b - a c) #'from-arity-list)
(aria '(take 10 map (lambda (x) * x 2) range 100) #'from-arity-list)
