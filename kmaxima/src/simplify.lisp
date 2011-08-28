;;; ----------------------------------------------------------------------------
;;; simplify.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This file contains modified code from:
;;;
;;; Copyright (C) 1984, 1987 William Schelter, University of Texas
;;; Copyright (C) 1981, 1982 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;; ----------------------------------------------------------------------------

(in-package :kmaxima)

(defmvar $simp t)
(defmvar $float nil)
(defmvar $numer nil)
(defmvar $%enumer nil)
(defmvar $negdistrib t)

(defmvar $expop 0)
(defmvar $expon 0)

(defvar *dosimp* nil)

;;; ----------------------------------------------------------------------------

(setf (get '$%e '$numer)
      2.7182818284590452353602874713526624977572470936999595749669676277)

(setf (get '$%pi '$numer)
      3.1415926535897932384626433832795028841971693993751058209749445923)

(setf (get '$%phi '$numer)
      1.6180339887498948482045868343656381177203091798057628621354486227)

(setf (get '$%gamma '$numer)
      0.5772156649015328606065120900824024310421593359399235988057672348)

;;; ----------------------------------------------------------------------------

(defun oneargcheck (l)
  (when (or (null (cdr l))
            (cddr l))
    (wna-err (caar l))))

(defun twoargcheck (l)
  (when (or (null (cddr l))
            (cdddr l))
    (wna-err (caar l))))

(defun wna-err (op)
  (merror "Wrong number of arguments to ~A" op))

;;; ----------------------------------------------------------------------------

(defun add (&rest terms)
  (if (and (cdr terms) (null (cddr terms)))
      (apply #'add2 terms)
      (apply #'addn `(,terms t))))

(define-compiler-macro add (&rest terms)
  (if (and (cdr terms) (null (cddr terms)))
      `(add2 ,@terms))
      `(addn (list ,@terms) t))

(defun add2 (x y)
  (cond ((and (numberp x) (numberp y)) (+ x y))
        ((eql 0 x) y)
        ((eql 0 y) x)
        (t (simplifya `((mplus) ,x ,y) t))))

(defun addn (terms simp-flag)
  (cond ((null terms) 0)
        ((null (cdr terms)) (simplifya (car terms) simp-flag))
        (t (simplifya `((mplus) . ,terms) simp-flag))))

;;; ----------------------------------------------------------------------------

(defun mul (&rest factors)
  (if (= (length factors) 2)
      (apply #'mul2 factors)
      (apply #'muln `(,factors t))))

(define-compiler-macro mul (&rest factors)
  (if (= (length factors) 2)
      `(mul2 ,@factors))
      `(muln (list ,@factors) t))

(defun mul2 (x y)
  (cond ((and (numberp x) (numberp y)) (* x y))
        ((eql 1 x) y)
        ((eql 1 y) x)
        (t (simplifya `((mtimes) ,x ,y) t))))

(defun muln (factors simp-flag)
  (cond ((null factors) 1)
        ((null (cdr factors)) (simplifya (car factors) simp-flag))
        (t (simplifya `((mtimes) . ,factors) simp-flag))))

;;; ----------------------------------------------------------------------------

(defun inv (x)
  (power x -1))

(defun neg (x)
  (declare (special $negdistrib))
  (cond ((numberp x) (- x))
        (t (let (($negdistrib t))
             (simplifya `((mtimes) -1 ,x) t)))))

(defun sub (x y)
  (cond ((and (numberp x) (numberp y)) (- x y))
        ((eql 0 y) x)
        ((eql 0 x) (neg y))
        (t (add x (neg y)))))

(defun div (x y)
  (if (eql 1 x)
      (inv y)
      (mul x (inv y))))

(defun power (bas pow)
  (cond ((eql 1 pow) bas)
        (t (simplifya `((mexpt) ,bas ,pow) t))))

;;; ----------------------------------------------------------------------------

(defun make-rat (n d)
  (cond ((zerop n) 0)
        ((eql d 1) n)
        (t
         (let ((u (gcd n d)))
           (setq n (truncate n u)
                 d (truncate d u))
           (when (minusp d) (setq n (- n) d (- d)))
           (cond ((eql d 1) n)
                 ($float (float (/ n d)))
                 (t (list '(rat simp) n d)))))))

(defun rat-to-float (x)
  (float (/ (cadr x) (caddr x))))

(defun rat-num (x)
  (if (numberp x) x (cadr x)))

(defun rat-den (x)
  (if (numberp x) 1 (caddr x)))

;;; ----------------------------------------------------------------------------

(defun addk (xx yy)
  (cond ((eql xx 0) yy)
        ((eql yy 0) xx)
        ((and (numberp xx) (numberp yy)) (+ xx yy))
        (t
         (prog (g a b (x xx) (y yy))
           (cond ((numberp x)
                  (cond ((floatp x) (return (+ x (rat-to-float y))))
                        (t (setq x (list '(rat) x 1)))))
                 ((numberp y)
                  (cond ((floatp y) (return (+ y (rat-to-float x))))
                        (t (setq y (list '(rat) y 1))))))
           (setq g (gcd (caddr x) (caddr y)))
           (setq a (truncate (caddr x) g)
                 b (truncate (caddr y) g))
           (setq g (timeskl (list '(rat) 1 g)
                            (list '(rat)
                                  (+ (* (cadr x) b)
                                     (* (cadr y) a))
                                  (* a b))))
           (return (cond ((numberp g) g)
                         ((eql (caddr g) 1) (cadr g))
                         ($float (rat-to-float g))
                         (t g)))))))

;;; ----------------------------------------------------------------------------

(defun timesk (x y)
  (cond ((eql x 1) y)
        ((eql y 1) x)
        ((and (numberp x) (numberp y)) (* x y))
        ((floatp x) (* x (rat-to-float y)))
        ((floatp y) (* y (rat-to-float x)))
        (t (timeskl x y))))

(defun timeskl (x y)
  (prog (u v g)
     (setq u (make-rat (rat-num x) (rat-den y)))
     (setq v (make-rat (rat-num y) (rat-den x)))
     (setq g (cond ((or (eql u 0) (eql v 0)) 0)
                   ((eql v 1) u)
                   ((and (numberp u) (numberp v)) (* u v))
                   (t
                    (list '(rat simp)
                          (* (rat-num u) (rat-num v))
                          (* (rat-den u) (rat-den v))))))
     (return (cond ((numberp g) g)
                   ((eql (caddr g) 1) (cadr g))
                   ($float (rat-to-float g))
                   (t g)))))

;;; ----------------------------------------------------------------------------

(defun simpcheck (e flag)
  (cond (flag e)
        (t
         (let (($%enumer $numer))
           (simplifya e nil)))))

;;; ----------------------------------------------------------------------------

(defun testtneg (x)
  (if (and $negdistrib
           (eql (cadr x) -1)
           (null (cdddr x))
           (mplusp (caddr x)))
      (addn (mapcar #'(lambda (z) (mul -1 z)) (cdaddr x)) t)
      x))

;;; ----------------------------------------------------------------------------

(defun simplifya (x y)
  (cond ((not $simp) x)
        ((atom x)
         (cond ((and $%enumer $numer (eq x '$%e))
                (setq x (get '$%e '$numer)))
               (t x)))
        ((atom (car x))
         (cond ((and (cdr x) (atom (cdr x)))
                (merror "simplifya: Found a cons with an atomic cdr."))
               (t
                (cons (car x)
                      (mapcar #'(lambda (x) (simplifya x y)) (cdr x))))))
        ((eq (caar x) 'rat) (simp-rat x 1 nil))
        ((and (not *dosimp*) (member 'simp (cdar x) :test #'eq)) x)
        (t
         (let ((w (get (caar x) 'operators)))
           (cond ((and w
                       (not (member 'array (cdar x) :test #'eq)))
                  (funcall w x 1 y))
                 (t (simpargs x y)))))))

(defun simpargs (x y)
  (if (and (member 'array (cdar x) :test #'eq)
           (null (margs x)))
      (merror "simplifya: Subscripted variable found with no subscripts."))
  (eqtest (if y
              x
              (let ((flag (member (caar x) '(mlist mequal) :test #'eq)))
                (cons (ncons (caar x))
                      (mapcar #'(lambda (u)
                                  (if flag
                                      (simplifya u nil)
                                      (simpcheck u nil)))
                              (cdr x)))))
          x))

(defun eqtest (x check)
  (cond ((or (atom x)
             (eq (caar x) 'rat)
             (member 'simp (cdar x) :test #'eq))
         x)
        ((and (eq (caar x) (caar check))
              (equal (cdr x) (cdr check)))
         (cond ((member 'simp (cdar check) :test #'eq)
                check)
               (t
                (cons (cons (caar check)
                            (if (cdar check)
                                (cons 'simp (cdar check))
                                '(simp)))
                      (cdr check)))))
        ((or (member 'array (cdar x) :test #'eq)
             (and (eq (caar x) (caar check))
                  (member 'array (cdar check) :test #'eq)))
         (rplaca x (cons (caar x) '(simp array))))
        (t
         (rplaca x (cons (caar x) '(simp))))))

;;; ----------------------------------------------------------------------------

(defun simp-rat (x y z)
  (declare (ignore y z))
  (cond ((member 'simp (cdar x) :test #'eq)
         (if $float
             (rat-to-float x)
             x))
        (t (make-rat (cadr x) (caddr x)))))

;;; ----------------------------------------------------------------------------

(setf (get 'mquotient 'operators) 'simp-mquotient)

(defun simp-mquotient (x y z)
  (twoargcheck x)
  (cond ((and (integerp (cadr x))
              (integerp (caddr x))
              (not (zerop (caddr x))))
         (make-rat (cadr x) (caddr x)))
        ((and (numberp (cadr x))
              (numberp (caddr x))
              (not (zerop (caddr x))))
         (/ (cadr x) (caddr x)))
        (t
         (setq y (simplifya (cadr x) z))
         (setq x (simplifya (list '(mexpt) (caddr x) -1) z))
         (if (eql y 1)
             x
             (simplifya (list '(mtimes) y x) t)))))

;;; ----------------------------------------------------------------------------

(setf (get 'mminus 'operators) 'simp-mminus)

(defun simp-mminus (x y z)
  (cond ((null (cdr x)) 0)
        ((null (cddr x))
         (mul -1 (simplifya (cadr x) z)))
        (t
         (sub (simplifya (cadr x) z) (addn (cddr x) z)))))

;;; ----------------------------------------------------------------------------

(setf (get 'mplus 'operators) 'simp-mplus)

(defun simp-mplus (x w z)
  (prog (res check eqnflag)
     (if (null (cdr x)) (return 0))
     (setq check x)
  start
     (setq x (cdr x))
     (if (null x) (go end))
     (setq w (if z (car x) (simplifya (car x) nil)))
  st1
     (cond ((atom w) nil)
           ((eq (caar w) 'mequal)
            (setq eqnflag
                  (if (not eqnflag)
                      w
                      (list (car eqnflag)
                            (add (cadr eqnflag) (cadr w))
                            (add (caddr eqnflag) (caddr w)))))
            (go start)))
     (setq res (pls w res))
     (go start)
  end
     (setq res (eqtest (testp res) check))
     (return (if eqnflag
                 (list (car eqnflag)
                       (add (cadr eqnflag) res)
                       (add (caddr eqnflag) res))
                 res))))

;;; ----------------------------------------------------------------------------

(defvar *plusflag* nil)

(defun testp (x)
  (cond ((atom x) 0)
        ((null (cddr x)) (cadr x))
        ((zerop1 (cadr x))
         (if (null (cdddr x))
             (caddr x)
             (rplacd x (cddr x))))
        (t x)))

(defun pls (x out)
  (prog (fm *plusflag*)
     (if (mtimesp x) (setq x (testtneg x)))
     (when (and $numer (atom x) (eq x '$%e))
       (setq x (get '$%e '$numer)))
     (cond ((null out)
            (return
              (cons '(mplus)
                    (cond ((mnumberp x) (ncons x))
                          ((not (mplusp x))
                           (list 0 (cond ((atom x) x) (t (copy-list x)))))
                          ((mnumberp (cadr x)) (copy-list (cdr x)))
                          (t (cons 0 (copy-list (cdr x) )))))))
           ((mnumberp x)
            (return (cons '(mplus)
                          (if (mnumberp (cadr out))
                              (cons (addk (cadr out) x) (cddr out))
                              (cons x (cdr out))))))
           ((not (mplusp x)) (plusin x (cdr out)) (go end)))
     (rplaca (cdr out)
             (addk (if (mnumberp (cadr out)) (cadr out) 0)
                   (cond ((mnumberp (cadr x)) (setq x (cdr x)) (car x))
                         (t 0))))
     (setq fm (cdr out))
  start
     (if (null (setq x (cdr x))) (go end))
     (setq fm (plusin (car x) fm))
     (go start)
  end
     (if (not *plusflag*) (return out))
     (setq *plusflag* nil)
  a  
     (setq fm (cdr out))
  loop
     (when (mplusp (cadr fm))
       (setq x (cadr fm))
       (rplacd fm (cddr fm))
       (pls x out)
       (go a))
     (setq fm (cdr fm))
     (if (null (cdr fm)) (return out))
     (go loop)))

;;; ----------------------------------------------------------------------------

(defun plusin (x fm)
  (prog (x1 x2 flag check v w xnew a n m c)
     (setq w 1)
     (setq v 1)
     (cond ((mtimesp x)
            (setq check x)
            (if (mnumberp (cadr x)) (setq w (cadr x) x (cddr x))
                (setq x (cdr x))))
           (t (setq x (ncons x))))
     (setq x1 (if (null (cdr x)) (car x) (cons '(mtimes) x))
           xnew (list* '(mtimes) w x))
  start
     (cond ((null (cdr fm)))
           ((and (alike1 x1 (cadr fm)) (null (cdr x)))
            (go equ))
           ;; Implement the simplification of
           ;;   v*a^(c+n)+w*a^(c+m) -> (v*a^n+w*a^m)*a^c
           ;; where a, v, w, and (n-m) are integers.
           ((and (or (and (mexptp (setq x2 (cadr fm)))
                          (setq v 1))
                     (and (mtimesp x2)
                          (not (alike1 x1 x2))
                          (null (cadddr x2))
                          (integerp (setq v (cadr x2)))
                          (mexptp (setq x2 (caddr x2)))))
                 (integerp (setq a (cadr x2)))
                 (mexptp x1)
                 (equal a (cadr x1))
                 (integerp (sub (caddr x2) (caddr x1))))
            (setq n (if (and (mplusp (caddr x2))
                             (mnumberp (cadr (caddr x2))))
                        (cadr (caddr x2))
                        (if (mnumberp (caddr x2))
                            (caddr x2)
                            0)))
            (setq m (if (and (mplusp (caddr x1))
                             (mnumberp (cadr (caddr x1))))
                        (cadr (caddr x1))
                        (if (mnumberp (caddr x1))
                            (caddr x1)
                            0)))
            (setq c (sub (caddr x2) n))
            (cond ((integerp n)
                   ;; The simple case:
                   ;; n and m are integers and the result is (v*a^n+w*a^m)*a^c.
                   (setq x1 (mul (addk (timesk v (exptb a n))
                                       (timesk w (exptb a m)))
                                 (power a c)))
                   (go equt2))
                  (t
                   ;; n and m are rational numbers: The difference n-m is an
                   ;; integer. The rational numbers might be improper fractions.
                   ;; The mixed numbers are: n = n1 + d1/r and m = n2 + d2/r,
                   ;; where r is the common denominator. We have two cases:
                   ;; I)  d1 = d2: e.g. 2^(1/3+c)+2^(4/3+c)
                   ;;     The result is (v*a^n1+w*a^n2)*a^(c+d1/r)
                   ;; II) d1 # d2: e.g. 2^(1/2+c)+2^(-1/2+c)
                   ;;     In this case one of the exponents d1 or d2 must
                   ;;     be negative. The negative exponent is factored out.
                   ;;     This guarantees that the factor (v*a^n1+w*a^n2)
                   ;;     is an integer. But the positive exponent has to be
                   ;;     adjusted accordingly. E.g. when we factor out
                   ;;     a^(d2/r) because d2 is negative, then we have to
                   ;;     adjust the positive exponent to n1 -> n1+(d1-d2)/r.
                   ;; Remark:
                   ;; Part of the simplification is done in simptimes. E.g.
                   ;; this algorithm simplifies the sum sqrt(2)+3*sqrt(2)
                   ;; to 4*sqrt(2). In simptimes this is further simplified
                   ;; to 2^(5/2).
                   (multiple-value-bind (n1 d1)
                       (truncate (num1 n) (denom1 n))
                     (multiple-value-bind (n2 d2)
                         (truncate (num1 m) (denom1 m))
                       (cond ((equal d1 d2)
                              ;; Case I: -> (v*a^n1+w*a^n2)*a^(c+d1/r)
                              (setq x1
                                    (mul (addk (timesk v (exptb a n1))
                                               (timesk w (exptb a n2)))
                                         (power a
                                                (add c
                                                     (div d1 (denom1 n))))))
                              (go equt2))
                             ((minusp d2)
                              ;; Case II:: d2 is negative, adjust n1.
                              (setq n1 (add n1 (div (sub d1 d2) (denom1 n))))
                              (setq x1
                                    (mul (addk (timesk v (exptb a n1))
                                               (timesk w (exptb a n2)))
                                         (power a
                                                (add c
                                                     (div d2 (denom1 n))))))
                              (go equt2))
                             ((minusp d1)
                              ;; Case II: d1 is negative, adjust n2.
                              (setq n2 (add n2 (div (sub d2 d1) (denom1 n))))
                              (setq x1
                                    (mul (addk (timesk v (exptb a n1))
                                               (timesk w (exptb a n2)))
                                         (power a 
                                                (add c
                                                     (div d1 (denom1 n))))))
                              (go equt2))
                             ;; This clause should never be reached.
                             (t (merror "Internal error in simplus."))))))))
           ((mtimesp (cadr fm))
            (cond ((alike1 x1 (cadr fm))
                   (go equt))
                  ((and (mnumberp (cadadr fm)) (alike x (cddadr fm)))
                   (setq flag t) ; found common factor
                   (go equt))
                  ((great xnew (cadr fm)) (go gr))))
           ((great x1 (cadr fm)) (go gr)))
     (setq xnew (eqtest (testt xnew) (or check '((foo)))))
     (return (cdr (rplacd fm (cons xnew (cdr fm)))))
  gr 
     (setq fm (cdr fm))
     (go start)
  equ
     (rplaca (cdr fm)
             (if (equal w -1)
                 (list* '(mtimes simp) 0 x)
                 ;; Call muln to get a simplified product.
                 (if (mtimesp (setq x1 (muln (cons (addk 1 w) x) t)))
                     (testtneg x1)
                     x1)))
  del
     (cond ((not (mtimesp (cadr fm)))
            (go check))
           ((onep (cadadr fm))
            ;; Do this simplification for an integer 1, not for 1.0 and 1.0b0
            (rplacd (cadr fm) (cddadr fm))
            (return (cdr fm)))
           ((not (zerop1 (cadadr fm)))
            (return (cdr fm)))
           ;; Handle the multiplication with a zero.
           ((and (or (not $listarith) (not $doallmxops))
                 (mxorlistp (caddr (cadr fm))))
            (return (rplacd fm 
                            (cons (constmx 0 (caddr (cadr fm))) (cddr fm))))))
     ;; (cadadr fm) is zero. If the first term of fm is a number,
     ;;  add it to preserve the type.
     (when (mnumberp (car fm))
       (rplaca fm (addk (car fm) (cadadr fm))))
     (return (rplacd fm (cddr fm)))
  equt
     ;; Call muln to get a simplified product.
     (setq x1 (muln (cons (addk w (if flag (cadadr fm) 1)) x) t))
  equt2
     (rplaca (cdr fm)
             (if (zerop1 x1)
                 (list* '(mtimes) x1 x)
                 (if (mtimesp x1) (testtneg x1) x1)))
     (if (not (mtimesp (cadr fm))) (go check))
     (when (and (onep (cadadr fm)) flag (null (cdddr (cadr fm))))
       ;; Do this simplification for an integer 1, not for 1.0 and 1.0b0
       (rplaca (cdr fm) (caddr (cadr fm))) (go check))
     (go del)
  check
     (if (mplusp (cadr fm)) (setq *plusflag* t)) ; A nested mplus expression
     (return (cdr fm))))

;;; ----------------------------------------------------------------------------

(setf (get 'mtimes 'operators) 'simp-mtimes)

(defun simp-mtimes (x w z)
  (prog (res check eqnflag)
     (if (null (cdr x)) (return 1))
     (setq check x)
  start
     (setq x (cdr x))
     (if (null x) (go end))
;     (cond ((zerop1 res) (return res))
;           ((null x) (go end)))
     (setq w (if z (car x) (simplifya (car x) nil)))
  st1
     (cond ((atom w) nil)
           ((eq (caar w) 'mequal)
            (setq eqnflag
                  (if (not eqnflag)
                      w
                      (list (car eqnflag)
                            (mul (cadr eqnflag) (cadr w))
                            (mul (caddr eqnflag) (caddr w)))))
            (go start)))
     (setq res (tms w 1 res))
     (go start)
  end
     (cond ((mtimesp res) (setq res (testt res))))
     (cond ((or (atom res)
                (not (member (caar res) '(mexpt mtimes) :test #'eq))
                (and (zerop $expop) (zerop $expon))
                expandflag))
           ((eq (caar res) 'mtimes) (setq res (expandtimes res)))
           ((and (mplusp (cadr res))
                 (fixnump (caddr res))
                 (not (or (> (caddr res) $expop)
                          (> (- (caddr res)) $expon))))
            (setq res (expandexpt (cadr res) (caddr res)))))
     (if res (setq res (eqtest res check)))
     (return (cond (eqnflag
                    (if (null res) (setq res 1))
                    (list (car eqnflag)
                          (mul (cadr eqnflag) res)
                          (mul (caddr eqnflag) res)))
                   (t res)))))

(defun testt (x)
  (cond ((mnumberp x) x)
        ((null (cddr x)) (cadr x))
        ((eql 1 (cadr x))
         (cond ((null (cdddr x))
                (caddr x))
               (t (rplacd x (cddr x)))))
        (t (testtneg x))))

(defun tms (factor power product &aux tem)
  (let ((rulesw nil)
        (z nil))
    (when (mplusp product) (setq product (list '(mtimes simp) product)))
    (cond ((zerop1 factor)
           (cond ((mnegativep power)
                  (if errorsw
                      (throw 'errorsw t)
                      (merror "Division by 0")))
                 ((mnumberp product)
                  (list '(mtimes) (timesk factor product)))
                 ((mnumberp (cadr product))
                  (list '(mtimes) (timesk factor (cadr product))))
                 (t (list '(mtimes) factor))))
          ((and (null product)
                (or (and (mtimesp factor) (equal power 1))
                    (and (setq product (list '(mtimes) 1)) nil)))
           (setq tem (append '((mtimes)) (if (mnumberp (cadr factor)) nil '(1))
                             (cdr factor) nil))
           (if (= (length tem) 1)
               (setq tem (copy-list tem))
               tem))
          ((mtimesp factor)
           (do ((factor-list (cdr factor) (cdr factor-list)))
               ((or (null factor-list) (zerop1 product))  product)
             (setq z (timesin (car factor-list) (cdr product) power))
             (when rulesw
               (setq rulesw nil)
               (setq product (tms-format-product z)))))
          (t
           (setq z (timesin factor (cdr product) power))
           (if rulesw
               (tms-format-product z)
               product)))))

(defun tms-format-product (x)
  (cond ((zerop1 x) x)
        ((mnumberp x) (list '(mtimes) x))
        ((not (mtimesp x)) (list '(mtimes) 1 x))
        ((not (mnumberp (cadr x))) (cons '(mtimes) (cons 1 (cdr x))))
        (t x)))

(defun timesin (x y w)
  (prog (fm temp z check u expo)
     (if (mexptp x) (setq check x))
  top
     (cond ((equal w 1)
            (setq temp x))
           (t
            (setq temp (cons '(mexpt) (if check 
                                          (list (cadr x) (mult (caddr x) w))
                                          (list x w))))
            (if (and (not timesinp) (not (eq x '$%i)))
                (let ((timesinp t))
                  (setq temp (simplifya temp t))))))
     (setq x (if (mexptp temp)
                 (cdr temp)
                 (list temp 1)))
     (setq w (cadr x)
           fm y)
  start
     (cond ((null (cdr fm))
            (go less))
           ((or (and (mnumberp temp)
                     (not (or (integerp temp)
                              (ratnump temp))))
                (and (integerp temp)
                     (equal temp -1)))
            (go less))
           ((mexptp (cadr fm))
            (cond ((alike1 (car x) (cadadr fm))
                   (cond ((zerop1 (setq w (plsk (caddr (cadr fm)) w)))
                          (go del))
                         ((and (mnumberp w)
                               (or (mnumberp (car x))
                                   (eq (car x) '$%i)))
                          (rplacd fm (cddr fm))
                          (cond ((mnumberp (setq x (if (mnumberp (car x))
                                                    (exptrl (car x) w)
                                                    (power (car x) w))))
                                 (return (rplaca y (timesk (car y) x))))
                                ((mtimesp x)
                                 (go times))
                                (t
                                 (setq temp x
                                       x (if (mexptp x) (cdr x) (list x 1)))
                                 (setq w (cadr x)
                                       fm y)
                                 (go start))))
                         ((maxima-constantp (car x))
                          (go const))
                         ((onep1 w)
                          (cond ((mtimesp (car x))
                                 ;; A base which is a mtimes expression.
                                 ;; Remove the factor from the lists of products.
                                 (rplacd fm (cddr fm))
                                 ;; Multiply the factors of the base with 
                                 ;; the list of all remaining products.
                                 (setq rulesw t)
                                 (return (muln (nconc y (cdar x)) t)))
                                (t (return (rplaca (cdr fm) (car x))))))
                         (t
                          (go spcheck))))
                  ;; At this place we have to add code for a rational number
                  ;; as a factor to the list of products.
                  ((and (onep1 w)
                        (or (ratnump (car x))
                            (and (integerp (car x))
                                 (not (onep (car x))))))
                   ;; Multiplying a^k * rational.
                   (let* ((numerator (if (integerp (car x)) 
                                         (car x)
                                         (second (car x))))
                          (denom (if (integerp (car x)) 
                                     1
                                     (third (car x))))
                          (sgn (signum numerator)))
                     (setf expo (exponent-of (abs numerator) (second (cadr fm))))
                     (when expo
                       ;; We have a^m*a^k.
                       (setq temp (power (second (cadr fm)) 
                                         (add (third (cadr fm)) expo)))
                       ;; Set fm to have 1/denom term.
                       (setq x (mul sgn
                                    (car y)
                                    (div (div (mul sgn numerator)
                                              (power (second (cadr fm))
                                                     expo))
                                         denom)))
                       (setf y (rplaca y 1))
                       ;; Add in the a^(m+k) term.
                       (rplacd fm (cddr fm))
                       (rplacd fm (cons temp (cdr fm)))
                       (setq temp x
                             x (list x 1)
                             w 1
                             fm y)
                       (go start))
                     (setf expo (exponent-of (inv denom) (second (cadr fm))))
                     (when expo
                       ;; We have a^(-m)*a^k.
                       (setq temp (power (second (cadr fm)) 
                                         (add (third (cadr fm)) expo)))
                       ;; Set fm to have the numerator term.
                       (setq x (mul (car y)
                                    (div numerator
                                         (div denom
                                              (power (second (cadr fm)) 
                                                     (- expo))))))
                       (setf y (rplaca y 1))
                       ;; Add in the a^(k-m) term.
                       (rplacd fm (cddr fm))
                       (rplacd fm (cons temp (cdr fm)))
                       (setq temp x
                             x (list x 1)
                             w 1
                             fm y)
                       (go start))
                     ;; Next term in list of products.
                     (setq fm (cdr fm))
                     (go start)))
                  
                  ((and (not (atom (car x)))
                        (eq (caar (car x)) 'mabs)
                        (equal (cadr x) 1)
                        (integerp (caddr (cadr fm)))
                        (< (caddr (cadr fm)) -1)
                        (alike1 (cadr (car x)) (cadr (cadr fm)))
                        (not (member ($csign (cadr (car x)))
                                     '($complex imaginary))))
                   ;; 1/x^n*abs(x) -> 1/(x^(n-2)*abs(x)), where n an integer
                   ;; Replace 1/x^n -> 1/x^(n-2)
                   (setq temp (power (cadr (cadr fm))
                                     (add (caddr (cadr fm)) 2)))
                   (rplacd fm (cddr fm))
                   (if (not (equal temp 1))
                       (rplacd fm (cons temp (cdr fm))))
                   ;; Multiply factor 1/abs(x) into list of products.
                   (setq x (list (car x) -1))
                   (setq temp (power (car x) (cadr x)))
                   (setq w (cadr x))
                   (go start))
                  
                  ((and (not (atom (car x)))
                        (eq (caar (car x)) 'mabs)
                        (equal (cadr x) -1)
                        (integerp (caddr (cadr fm)))
                        (> (caddr (cadr fm)) 1)
                        (alike1 (cadr (car x)) (cadr (cadr fm)))
                        (not (member ($csign (cadr (car x)))
                                     '($complex imaginary))))
                   ;; x^n/abs(x) -> x^(n-2)*abs(x), where n an integer.
                   ;; Replace x^n -> x^(n-2)
                   (setq temp (power (cadr (cadr fm)) 
                                     (add (caddr (cadr fm)) -2)))
                   (rplacd fm (cddr fm))
                   (if (not (equal temp 1))
                       (rplacd fm (cons temp (cdr fm))))
                   ;; Multiply factor abs(x) into list of products.
                   (setq x (list (car x) 1))
                   (setq temp (power (car x) (cadr x)))
                   (setq w (cadr x))
                   (go start))
                  
                  ((and (not (atom (cadr fm)))
                        (not (atom (cadr (cadr fm))))
                        (eq (caaadr (cadr fm)) 'mabs)
                        (equal (caddr (cadr fm)) -1)
                        (integerp (cadr x))
                        (> (cadr x) 1)
                        (alike1 (cadadr (cadr fm)) (car x))                        
                        (not (member ($csign (cadadr (cadr fm)))
                                     '($complex imaginary))))
                   ;; 1/abs(x)*x^n -> x^(n-2)*abs(x), where n an integer.
                   ;; Replace 1/abs(x) -> abs(x)
                   (setq temp (cadr (cadr fm)))
                   (rplacd fm (cddr fm))
                   (rplacd fm (cons temp (cdr fm)))
                   ;; Multiply factor x^(n-2) into list of products.
                   (setq x (list (car x) (add (cadr x) -2)))
                   (setq temp (power (car x) (cadr x)))
                   (setq w (cadr x))
                   (go start))
                  
                  ((or (maxima-constantp (car x))
                       (maxima-constantp (cadadr fm)))
                   (if (great temp (cadr fm))
                       (go gr)))
                  ((great (car x) (cadadr fm))
                   (go gr)))
            (go less))
           ((alike1 (car x) (cadr fm))
            (go equ))
          ((mnumberp temp)
           (setq fm (cdr fm))
           (go start))
           
           ((and (not (atom (cadr fm)))
                 (eq (caar (cadr fm)) 'mabs)
                 (integerp (cadr x))
                 (< (cadr x) -1)
                 (alike1 (cadr (cadr fm)) (car x))
                 (not (member ($csign (cadr (cadr fm)))
                                     '($complex imaginary))))
            (setq temp (power (cadr fm) -1))
            (rplacd fm (cddr fm))
            (rplacd fm (cons temp (cdr fm)))
            (setq x (list (car x) (add (cadr x) 2)))
            (setq temp (power (car x) (cadr x)))
            (setq w (cadr x))
            (go start))
           ((maxima-constantp (car x))
            (when (great temp (cadr fm))
              (go gr)))
           ((great (car x) (cadr fm))
            (go gr)))
  less
     (cond ((mnumberp temp)
           (return (rplaca y (timesk (car y) temp))))
           ((and (eq (car x) '$%i)
                 (fixnump w))
            (go %i))
           ((and (eq (car x) '$%e)
                 $numer
                 (integerp w))
            (return (rplaca y (timesk (car y) (exp (float w))))))
           ((and (onep1 w)
                 (not (constant (car x))))
            (go less1))
           ((and (mexptp temp)
                 (not (onep1 (car y)))
                 (or (integerp (car y))
                     (ratnump (car y))))
            (let* ((numerator (if (integerp (car y)) 
                                 (car y)
                                 (second (car y))))
                   (denom (if (integerp (car y)) 
                              1
                              (third (car y)))))
              (setf expo (exponent-of (abs numerator) (car x)))
              (when expo
                (setq temp (power (car x)
                                  (add (cadr x) expo)))
                (setq x (div (div numerator
                                  (power (car x) expo))
                             denom))
                (setf y (rplaca y 1))
                (rplacd fm (cons temp (cdr fm)))
                (setq temp x
                      x (list x 1)
                      w 1
                      fm y)
                (go start))
              (setf expo (exponent-of (inv denom) (car x)))
              (when expo
                ;; We have a^(-m)*a^k.
                (setq temp (power (car x) 
                                  (add (cadr x) expo)))
                ;; Set fm to have the numerator term.
                (setq x (div numerator
                             (div denom
                                  (power (car x) 
                                         (- expo)))))
                (setf y (rplaca y 1))
                ;; Add in the a^(k-m) term.
                (rplacd fm (cons temp (cdr fm)))
                (setq temp x
                      x (list x 1)
                      w 1
                      fm y)
                (go start))
              ;; The rational doesn't contain any (simple) powers of
              ;; the exponential term.  We're done.
              (return (cdr (rplacd fm (cons temp (cdr fm)))))))
           ((and (maxima-constantp (car x))
                 (do ((l (cdr fm) (cdr l)))
                     ((null (cdr l)))
                   (when (and (mexptp (cadr l))
                              (alike1 (car x) (cadadr l)))
                     (setq fm l)
                     (return t))))
            (go start))
           ((or (and (mnumberp (car x))
                     (mnumberp w))
                (and (eq (car x) '$%e)
                     $%emode
                     (among '$%i w)
                     (among '$%pi w)
                     (setq u (%especial w))))
            (setq x (cond (u)
                          ((alike (cdr check) x)
                           check)
                          (t
                           (exptrl (car x) w))))
            (cond ((mnumberp x)
                   (return (rplaca y (timesk (car y) x))))
                  ((mtimesp x)
                   (go times))
                  ((mexptp x)
                   (return (cdr (rplacd fm (cons x (cdr fm))))))
                  (t
                   (setq temp x
                         x (list x 1)
                         w 1
                         fm y)
                   (go start))))
           ((onep1 w)
            (go less1))
           (t
            (setq temp (list '(mexpt) (car x) w))
            (setq temp (eqtest temp (or check '((foo)))))
            (return (cdr (rplacd fm (cons temp (cdr fm)))))))
  less1
     (return (cdr (rplacd fm (cons (car x) (cdr fm)))))
  gr
     (setq fm (cdr fm))
     (go start)
  equ
     (cond ((and (eq (car x) '$%i) (equal w 1))
            (rplacd fm (cddr fm))
            (return (rplaca y (timesk -1 (car y)))))
           ((zerop1 (setq w (plsk 1 w)))
            (go del))
           ((and (mnumberp (car x)) (mnumberp w))
            (return (rplaca (cdr fm) (exptrl (car x) w))))
           ((maxima-constantp (car x))
            (go const)))
  spcheck
     (setq z (list '(mexpt) (car x) w))
     (cond ((alike1 (setq x (simplifya z t)) z)
            (return (rplaca (cdr fm) x)))
           (t
            (rplacd fm (cddr fm))
            (setq rulesw t)
            (return (muln (cons x y) t))))
  const
     (rplacd fm (cddr fm))
     (setq x (car x) check nil)
     (go top)
  times
     (setq z (tms x 1 (setq temp (cons '(mtimes) y))))
     (return (cond ((eq z temp)
                    (cdr z))
                   (t
                    (setq rulesw t) z)))
  del
     (return (rplacd fm (cddr fm)))
  %i
     (if (minusp (setq w (rem w 4)))
         (incf w 4))
     (return (cond ((zerop w)
                    fm)
                   ((= w 2)
                    (rplaca y (timesk -1 (car y))))
                   ((= w 3)
                    (rplaca y (timesk -1 (car y)))
                    (rplacd fm (cons '$%i (cdr fm))))
                   (t
                    (rplacd fm (cons '$%i (cdr fm))))))))

(defun exponent-of (m base)
  (declare (ignore m base))
  nil)

;;; ----------------------------------------------------------------------------

(setf (get 'mexpt 'operators) 'simp-mexpt)

(defun simp-mexpt (x y z)
  (prog (gr pot check res rulesw w mlpgr mlppot)
     (setq check x)
     (when z
       (setq gr (cadr x) pot (caddr x))
       (go cont))
     (twoargcheck x)
     (setq gr (simplifya (cadr x) nil))
     (setq pot (let (($%enumer $numer)) (simplifya (caddr x) nil)))
  cont
     (cond ((onep1 pot) (go atgr))
           ((or (zerop1 pot) (onep1 gr)) (go retno))
           ((zerop1 gr)
            (cond ((mnumberp pot)
                   (if (mnegativep pot)
                       (merror "expt: Undefined: 0 to a negative exponent.")
                       (return (cond ((or (floatp gr) (floatp pot)) 0.0)
                                     (t 0)))))
                  ((or (member (setq z ($csign pot)) '($neg $nz))
                       (and *zexptsimp? (eq ($asksign pot) '$neg)))
                   ;; A negative exponent. Maxima error.
                   (cond ((not errorsw) (merror "expt: undefined: 0 to a negative exponent."))
                         (t (throw 'errorsw t))))
                  ((and (member z '($complex $imaginary))
                        ;; A complex exponent. Look at the sign of the realpart.
                        (member (setq z ($sign ($realpart pot))) 
                                '($neg $nz $zero)))
                   (cond ((not errorsw)
                          (merror "expt: undefined: 0 to a complex exponent."))
                         (t (throw 'errorsw t))))
                  ((and *zexptsimp? (eq ($asksign pot) '$zero))
                   (cond ((not errorsw)
                          (merror "expt: undefined: 0^0"))
                         (t (throw 'errorsw t))))
                  ((not (member z '($pos $pz)))
                   ;; The sign of realpart(pot) is not known. We can not return
                   ;; an unsimplified 0^a expression, because timesin can not
                   ;; handle it. We return ZERO. That is the old behavior.
                   ;; Look for the imaginary symbol to be consistent with 
                   ;; old code.
                   (cond ((not (free pot '$%i))
                          (cond ((not errorsw)
                                 (merror "expt: undefined: 0 to a complex exponent."))
                                (t (throw 'errorsw t))))
                         (t
                          ;; Return ZERO and not an unsimplified expression.
                          (return (zerores gr pot)))))
                  (t (return (zerores gr pot)))))
           ((and (mnumberp gr)
                 (mnumberp pot)
                 (or (not (ratnump gr)) (not (ratnump pot))))
            (return (eqtest (exptrl gr pot) check)))
           ;; Check for numerical evaluation of the sqrt.
           ((and (alike1 pot '((rat) 1 2))
                 (or (setq res (flonum-eval '%sqrt gr))
                     (and (not (member 'simp (car x) :test #'eq))
                          (setq res (big-float-eval '%sqrt gr)))))
            (return res))
           ((eq gr '$%i)
            (return (%itopot pot)))
           ((and (realp gr) (minusp gr) (mevenp pot))
            (setq gr (- gr))
            (go cont))
           ((and (realp gr) (minusp gr) (moddp pot))
            (return (mul2 -1 (power (- gr) pot))))
           ((and (equal gr -1) (maxima-integerp pot) (mminusp pot))
            (setq pot (neg pot))
            (go cont))
           ((and (equal gr -1)
                 (maxima-integerp pot)
                 (mtimesp pot)
                 (= (length pot) 3)
                 (fixnump (cadr pot))
                 (oddp (cadr pot))
                 (maxima-integerp (caddr pot)))
            (setq pot (caddr pot))
            (go cont))
           ((atom gr) (go atgr))
           ((and (eq (caar gr) 'mabs)
                 (evnump pot)
                 (or (and (eq $domain '$real) (not (decl-complexp (cadr gr))))
                     (and (eq $domain '$complex) (decl-realp (cadr gr)))))
            (return (power (cadr gr) pot)))
           ((and (eq (caar gr) 'mabs)
                 (integerp pot)
                 (oddp pot)
                 (not (equal pot -1))
                 (or (and (eq $domain '$real) (not (decl-complexp (cadr gr))))
                     (and (eq $domain '$complex) (decl-realp (cadr gr)))))
            ;; abs(x)^(2*n+1) -> abs(x)*x^(2*n), n an integer number
            (if (plusp pot)
                (return (mul (power (cadr gr) (add pot -1))
                             gr))
                (return (mul (power (cadr gr) (add pot 1))
                             (inv gr)))))
           ((eq (caar gr) 'mequal)
            (return (eqtest (list (ncons (caar gr))
                                  (power (cadr gr) pot)
                                  (power (caddr gr) pot))
                            gr)))
           ((symbolp pot) (go opp))
           ((eq (caar gr) 'mexpt) (go e1))
           ((and (eq (caar gr) '%sum)
                 $sumexpand
                 (integerp pot)
                 (signp g pot)
                 (< pot $maxposex))
            (return (do ((i (1- pot) (1- i))
                         (an gr (simptimes (list '(mtimes) an gr) 1 t)))
                        ((signp e i) an))))
           ((equal pot -1) 
            (return (eqtest (testt (tms gr pot nil)) check)))
           ((fixnump pot)
            (return (eqtest (cond ((and (mplusp gr)
                                        (not (or (> pot $expop)
                                                 (> (- pot) $expon))))
                                   (expandexpt gr pot))
                                  (t (simplifya (tms gr pot nil) t)))
                            check))))
     
  opp
     (cond ((eq (caar gr) 'mexpt) (go e1))
           ((eq (caar gr) 'rat)
            (return (mul2 (power (cadr gr) pot)
                          (power (caddr gr) (mul2 -1 pot)))))
           ((not (eq (caar gr) 'mtimes)) (go up))
           ((or (eq $radexpand '$all) (and $radexpand (simplexpon pot)))
            (setq res (list 1))
            (go start))
           ((and (or (not (numberp (cadr gr)))
                     (equal (cadr gr) -1))
                 (equal -1 ($num gr)) ; only for -1
                 ;; Do not simplify for a complex base.
                 (not (member ($csign gr) '($complex $imaginary)))
                 (and (eq $domain '$real) $radexpand))
            ;; (-1/x)^a -> 1/(-x)^a for x negative
            ;; For all other cases (-1)^a/x^a
            (if (eq ($csign (setq w ($denom gr))) '$neg)
                (return (inv (power (neg w) pot)))
                (return (div (power -1 pot)
                             (power w pot)))))
           ((or (eq $domain '$complex) (not $radexpand)) (go up)))
     (return (do ((l (cdr gr) (cdr l)) (res (ncons 1)) (rad))
                 ((null l)
                  (cond ((equal res '(1))
                         (eqtest (list '(mexpt) gr pot) check))
                        ((null rad) 
                         (testt (cons '(mtimes simp) res)))
                        (t
                         (setq rad (power* ; RADEXPAND=()?
                                     (cons '(mtimes) (nreverse rad)) pot))
                         (cond ((not (onep1 rad))
                                (setq rad
                                      (testt (tms rad 1 (cons '(mtimes) res))))
                                (cond (rulesw
                                       (setq rulesw nil res (cdr rad))))))
                         (eqtest (testt (cons '(mtimes) res)) check))))
               ;; Check with $csign to be more complete. This prevents wrong 
               ;; simplifications like sqrt(-z^2)->%i*sqrt(z^2) for z complex.
               (setq z ($csign (car l)))
               (if (member z '($complex $imaginary))
                   (setq z '$pnz)) ; if appears complex, unknown sign
               (setq w (cond ((member z '($neg $nz) :test #'eq)
                              (setq rad (cons -1 rad))
                              (mult -1 (car l)))
                             (t (car l))))
               (cond ((onep1 w))
                     ((alike1 w gr) (return (list '(mexpt simp) gr pot)))
                     ((member z '($pn $pnz) :test #'eq)
                      (setq rad (cons w rad)))
                     (t
                      (setq w (testt (tms (simplifya (list '(mexpt) w pot) t)
                                          1 (cons '(mtimes) res))))))
               (cond (rulesw (setq rulesw nil res (cdr w))))))
     
  start
     (cond ((and (cdr res) (onep1 (car res)) (ratnump (cadr res)))
            (setq res (cdr res))))
     (cond ((null (setq gr (cdr gr)))
            (return (eqtest (testt (cons '(mtimes) res)) check)))
           ((mexptp (car gr))
            (setq y (list (caar gr) (cadar gr) (mult (caddar gr) pot))))
           ((eq (car gr) '$%i)
            (setq y (%itopot pot)))
           ((mnumberp (car gr))
            (setq y (list '(mexpt) (car gr) pot)))
           (t (setq y (list '(mexpt simp) (car gr) pot))))
     (setq w (testt (tms (simplifya y t) 1 (cons '(mtimes) res))))
     (cond (rulesw (setq rulesw nil res (cdr w))))
     (go start)
     
  retno
     (return (exptrl gr pot))
     
  atgr
     (cond ((zerop1 pot) (go retno))
           ((onep1 pot)
            (let ((y (getprop gr '$numer)))
              (if (and y (floatp y) (or $numer (not (equal pot 1))))
                  ;; A numeric constant like %e, %pi, ... and 
                  ;; exponent is a float or bigfloat value.
                  (return (if (and (member gr *builtin-numeric-constants*)
                                   (equal pot bigfloatone))
                              ;; Return a bigfloat value.
                              ($bfloat gr)
                              ;; Return a float value.
                              y))
                  ;; In all other cases exptrl simplifies accordingly.
                  (return (exptrl gr pot)))))
           ((eq gr '$%e)
            ;; Numerically evaluate if the power is a flonum.
            (when $%emode
              (let ((val (flonum-eval '%exp pot)))
                (when val
                  (return val)))
              ;; Numerically evaluate if the power is a (complex)
              ;; big-float.  (This is basically the guts of
              ;; big-float-eval, but we can't use big-float-eval.)
              (when (and (not (member 'simp (car x) :test #'eq))
                         (complex-number-p pot 'bigfloat-or-number-p))
                (let ((x ($realpart pot))
                      (y ($imagpart pot)))
                  (cond ((and ($bfloatp x) (like 0 y))
                         (return ($bfloat `((mexpt simp) $%e ,pot))))
                        ((or ($bfloatp x) ($bfloatp y))
                         (let ((z (add ($bfloat x) (mul '$%i ($bfloat y)))))
                           (setq z ($rectform `((mexpt simp) $%e ,z)))
                           (return ($bfloat z))))))))
            (cond ((and $logsimp (among '%log pot)) (return (%etolog pot)))
                  ((and $demoivre (setq z (demoivre pot))) (return z))
                  ((and $%emode
                        (among '$%i pot)
                        (among '$%pi pot)
                        ;; Exponent contains %i and %pi and %emode is TRUE:
                        ;; Check simplification of exp(%i*%pi*p/q*x)
                        (setq z (%especial pot)))
                   (return z))
                  (($taylorp (third x))
                   ;; taylorize %e^taylor(...)
                   (return ($taylor x)))))
           (t
            (let ((y (mget gr '$numer)))
              ;; Check for a numeric constant.
              (and y
                   (floatp y)
                   (or (floatp pot)
                       ;; The exponent is a bigfloat. Convert base to bigfloat.
                       (and ($bfloatp pot)
                            (member gr *builtin-numeric-constants*)
                            (setq y ($bfloat gr)))
                       (and $numer (integerp pot)))
                   (return (exptrl y pot))))))

  up 
     (return (eqtest (list '(mexpt) gr pot) check))

  matrix
     (cond ((zerop1 pot)
            (cond ((mxorlistp1 gr) (return (constmx (addk 1 pot) gr)))
                  (t (go retno))))
           ((onep1 pot) (return gr))
           ((or $doallmxops $doscmxops $domxexpt)
            (cond ((or (and mlpgr
                            (or (not ($listp gr)) $listarith)
                            (scalar-or-constant-p pot $assumescalar))
                       (and $domxexpt
                            mlppot
                            (or (not ($listp pot)) $listarith)
                            (scalar-or-constant-p gr $assumescalar)))
                   (return (simplifya (outermap1 'mexpt gr pot) t)))
                  (t (go up))))
           ((and $domxmxops (member pot '(-1 -1.0) :test #'equal))
            (return (simplifya (outermap1 'mexpt gr pot) t)))
           (t (go up)))
  e1 
     ;; At this point we have an expression: (z^a)^b with gr = z^a and pot = b
     (cond ((or (eq $radexpand '$all)
                ;; b is an integer or an odd rational
                (simplexpon pot)
                (and (eq $domain '$complex)
                     (not (member ($csign (caddr gr)) '($complex $imaginary)))
                         ;; z >= 0 and a not a complex
                     (or (member ($csign (cadr gr)) '($pos $pz $zero))
                         ;; -1 < a <= 1
                         (and (mnumberp (caddr gr))
                              (eq ($sign (sub 1 (take '(mabs) (caddr gr))))
                                  '$pos))))
                (and (eq $domain '$real)
                     (member ($csign (cadr gr)) '($pos $pz $zero)))
                ;; (1/z)^a -> 1/z^a when z a constant complex
                (and (eql (caddr gr) -1)
                     (or (and $radexpand
                              (eq $domain '$real))
                         (and (eq ($csign (cadr gr)) '$complex)
                              ($constantp (cadr gr)))))
                ;; This does (1/z)^a -> 1/z^a. This is in general wrong.
                ;; We switch this type of simplification on, when
                ;; $ratsimpexpons is T. E.g. radcan sets this flag to T.
                ;; radcan hangs for expressions like sqrt(1/(1+x)) without
                ;; this simplification.
                (and $ratsimpexpons
                     (equal (caddr gr) -1))
                (and $radexpand
                     (eq $domain '$real)
                     (odnump (caddr gr))))
            ;; Simplify (z^a)^b -> z^(a*b)
            (setq pot (mul pot (caddr gr))
                  gr (cadr gr)))
           ((and (eq $domain '$real)
                 (free gr '$%i)
                 $radexpand
                 (not (decl-complexp (cadr gr)))
                 (evnump (caddr gr)))
            ;; Simplify (x^a)^b -> abs(x)^(a*b)
            (setq pot (mul pot (caddr gr))
                  gr (radmabs (cadr gr))))
           ((and $radexpand
                 (eq $domain '$real)
                 (mminusp (caddr gr)))
            ;; Simplify (1/z^a)^b -> 1/(z^a)^b
            (setq pot (neg pot)
                  gr (power (cadr gr) (neg (caddr gr)))))
           (t (go up)))
     (go cont)))

;;; ----------------------------------------------------------------------------

(defun exptrl (r1 r2)
  (cond ((equal r2 1) r1)
        ((equal r2 1.0)
         (cond ((mnumberp r1) (addk 0.0 r1))
               (t (list '(mexpt simp) r1 1.0))))
	((zerop1 r1)
	 (cond ((or (zerop1 r2) (mnegp r2))
		(if (not errorsw)
		    (merror "expt: undefined: ~M" (list '(mexpt) r1 r2))
		    (throw 'errorsw t)))
	       (t (zerores r1 r2))))
	((or (zerop1 r2) (onep1 r1))
	 (cond ((or ($bfloatp r1) ($bfloatp r2)) bigfloatone)
	       ((or (floatp r1) (floatp r2)) 1.0)
	       (t 1)))
	((or ($bfloatp r1) ($bfloatp r2)) ($bfloat (list '(mexpt) r1 r2)))
	((and (numberp r1) (integerp r2)) (exptb r1 r2))
	((and (numberp r1) (floatp r2) (equal r2 (float (floor r2))))
	 (exptb (float r1) (floor r2)))
	((or $numer (and (floatp r2) (or (plusp (num1 r1)) $numer_pbranch)))
	 (let (y  #+kcl(r1 r1) #+kcl(r2 r2))
	   (cond ((minusp (setq r1 (addk 0.0 r1)))
		  (cond ((or $numer_pbranch (eq $domain '$complex))
		         ;; for R1<0:
		         ;; R1^R2 = (-R1)^R2*cos(pi*R2) + i*(-R1)^R2*sin(pi*R2)
			 (setq r2 (addk 0.0 r2))
			 (setq y (exptrl (- r1) r2) r2 (* %pi-val r2))
			 (add2 (* y (cos r2))
			       (list '(mtimes simp) (* y (sin r2)) '$%i)))
			(t (setq y (let ($numer $float $keepfloat $ratprint)
				     (power -1 r2)))
			   (mul2 y (exptrl (- r1) r2)))))
	         ((equal (setq r2 (addk 0.0 r2)) (float (floor r2)))
	          (exptb r1 (floor r2)))
	         ((and (equal (setq y (* 2.0 r2)) (float (floor y)))
	               (not (equal r1 %e-val)))
		  (exptb (sqrt r1) (floor y)))
		 (t (exp (* r2 (log r1)))))))
	((floatp r2) (list '(mexpt simp) r1 r2))
	((integerp r2)
	 (cond ((minusp r2)
	        (exptrl (cond ((equal (abs (cadr r1)) 1)
	                       (* (cadr r1) (caddr r1)))
	                       ;; We set the simp flag at this place. This
	                       ;; changes nothing for an exponent r2 # -1.
	                       ;; exptrl is called again and does not look at
	                       ;; the simp flag. For the case r2 = -1 exptrl
	                       ;; is called with an exponent 1. For this case
	                       ;; the base is immediately returned. Now the
	                       ;; base has the correct simp flag. (DK 02/2010)
			      ((minusp (cadr r1))
			       (list '(rat simp) (- (caddr r1)) (- (cadr r1))))
			      (t (list '(rat simp) (caddr r1) (cadr r1))))
			(- r2)))
	       (t (list '(rat simp) (exptb (cadr r1) r2) (exptb (caddr r1) r2)))))
	((and (floatp r1) (alike1 r2 '((rat) 1 2)))
	 (if (minusp r1)
	     (list '(mtimes simp) (sqrt (- r1)) '$%i)
	     (sqrt r1)))
	((and (floatp r1) (alike1 r2 '((rat) -1 2)))
	 (if (minusp r1)
	     (list '(mtimes simp) (/ -1.0 (sqrt (- r1))) '$%i)
	     (/ (sqrt r1))))
	((floatp r1)
	 (if (plusp r1)
	     (exptrl r1 (fpcofrat r2))
	     (mul2 (exptrl -1 r2) ;; (-4.5)^(1/4) -> (4.5)^(1/4) * (-1)^(1/4)
		   (exptrl (- r1) r2))))
	(exptrlsw (list '(mexpt simp) r1 r2))
	(t
	 (let ((exptrlsw t))
	   (simptimes (list '(mtimes)
			    (exptrl r1 (truncate (cadr r2) (caddr r2)))
			    (let ((y (let ($keepfloat $ratprint)
				       (simpnrt r1 (caddr r2))))
				  (z (rem (cadr r2) (caddr r2))))
			      (if (mexptp y)
				  (list (car y) (cadr y) (mul2 (caddr y) z))
				  (power y z))))
		      1 t)))))

;;; ----------------------------------------------------------------------------

(defun exptb (a b)
  (cond ((eql a (get '$%e '$numer))
         (exp (float b)))
        ((or (floatp a) (not (minusp b)))
         (expt a b))
        (t
         (setq b (expt a (- b)))
         (make-rat 1 b))))

;;; ----------------------------------------------------------------------------

