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
(defmvar $numer nil)
(defmvar $float nil)

(defvar *dosimp* nil)

;;; ----------------------------------------------------------------------------

(defun oneargcheck (l)
  (if (or (null (cdr l)) (cddr l))
      (wna-err (caar l))))

(defun twoargcheck (l)
  (if (or (null (cddr l)) (cdddr l))
      (wna-err (caar l))))

(defun wna-err (op)
  (merror "Wrong number of arguments to ~:@M" op))

;;; ----------------------------------------------------------------------------

(defun specrepp (e)
  (and (not (atom e))
       (member (caar e) '(mrat mpois) :test #'eq)))

(defun specdisrep (e)
  (cond ((eq (caar e) 'mrat) (ratdisrep e))
        (t ($outofpois e))))

;;; ----------------------------------------------------------------------------

(defun simpcheck (e flag)
  (cond ((specrepp e) (specdisrep e))
        (flag e)
        (t (simplifya e nil))))

;;; ----------------------------------------------------------------------------

(defun simplifya (x y &aux w)
  (cond ((not $simp) x)
        ((atom x) x)
        ((atom (car x))
         (cond ((and (cdr x) (atom (cdr x)))
                (merror "simplifya: Found a cons with an atomic cdr."))
               (t
                (cons (car x)
                      (mapcar #'(lambda (x) (simplifya x y)) (cdr x))))))
        ((eq (caar x) 'rat)
         (cond ((member 'simp (cdar x) :test #'eq)
                (if $float (/ (cadr x) (caddr x)) x))
               (t (simp-rat (cadr x) (caddr x)))))
        ((and (not *dosimp*) (member 'simp (cdar x) :test #'eq)) x)
        ((setq w (getprop (caar x) 'operators))
         (funcall w x 1 y))
        (t
         (simpargs x y))))

;;; ----------------------------------------------------------------------------

(defun simpargs (x y)
  (if (or (eq (get (caar x) 'dimension) 'dimension-infix)
          (get (caar x) 'binary))
      (twoargcheck x))
  (if (and (member 'array (cdar x) :test #'eq) (null (margs x)))
      (merror "Subscripted variable found with no subscripts."))
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

;;; ----------------------------------------------------------------------------

(defun eqtest (x check)
  (let ((y nil))
    (cond ((or (atom x)
               (eq (caar x) 'rat)
               (eq (caar x) 'mrat)
               (member 'simp (cdar x) :test #'eq))
           x)
          ((and (eq (caar x) (caar check))
                (equal (cdr x) (cdr check)))
           (cond ((and (null (cdar check))
                       (setq y (get (caar check) 'msimpind)))
                  (cons y (cdr check)))
                 ((member 'simp (cdar check) :test #'eq)
                  check)
                 (t
                  (cons (cons (caar check)
                              (if (cdar check)
                                  (cons 'simp (cdar check))
                                  '(simp)))
                        (cdr check)))))
           ((setq y (get (caar x) 'msimpind))
            (rplaca x y))
           ((or (member 'array (cdar x) :test #'eq)
                (and (eq (caar x) (caar check))
                     (member 'array (cdar check) :test #'eq)))
            (rplaca x (cons (caar x) '(simp array))))
          (t
           (rplaca x (cons (caar x) '(simp)))))))

;;; ----------------------------------------------------------------------------

(defun simp-rat (n d)
  (cond ((zerop n) 0)
        ((equal d 1) n)
        (t
         (let ((u (gcd n d)))
           (setq n (truncate n u)
                 d (truncate d u))
           (if (minusp d) (setq n (- n) d (- d)))
           (cond ((equal d 1) n)
                 ($float (float (/ n d)))
                 (t (list '(rat simp) n d)))))))

;;; ----------------------------------------------------------------------------

;;; Simplification of the "/" operator.

(defprop mquotient simp-quotient operators)

(defun simp-quotient (x y z)
  (twoargcheck x)
  (cond ((and (integerp (cadr x))
              (integerp (caddr x))
              (not (zerop (caddr x))))
         (simp-rat (cadr x) (caddr x)))
        ((and (numberp (cadr x))
              (numberp (caddr x))
              (not (zerop (caddr x))))
         (/ (cadr x) (caddr x)))
        (t
         (setq y (simplifya (cadr x) z))
         (setq x (simplifya (list '(mexpt) (caddr x) -1) z))
         (if (equal y 1)
             x
             (simplifya (list '(mtimes) y x) t)))))

;;; ----------------------------------------------------------------------------

(defun testp (x)
  (cond ((atom x) 0)
        ((null (cddr x)) (cadr x))
        ((zerop1 (cadr x))
         (cond ((null (cdddr x)) (caddr x)) (t (rplacd x (cddr x)))))
        (t x)))

;;; ----------------------------------------------------------------------------

(defprop mplus simplus operators)

(defun simplus (x w z)
  (prog (check res)
    (if (null (cdr x)) (return 0))
    (setq check x)
    (do ((x (cdr x) (cdr x)))
        ((null x))
      (setq w (if z (car x) (simplifya (car x) nil)))
      (setq res (pls w res)))
    (return (eqtest (testp res) check))))

(defun pls (x out)
  (prog (fm *plusflag*)
     (if (mtimesp x) (setq x (testtneg x)))
     (when (and $numer (atom x) (eq x '$%e))
       ;; Replace $%e with its numerical value, when $numer ist TRUE
       (setq x %e-val))
     (cond ((null out)
            ;; Initialize a form like ((mplus) <number> expr)
            (return
              (cons '(mplus)
                    (cond ((mnumberp x) (ncons x))
                          ((not (mplusp x))
                           (list 0 (cond ((atom x) x) (t (copy-list x)))))
                          ((mnumberp (cadr x)) (copy-list (cdr x) ))
                          (t (cons 0 (copy-list (cdr x) )))))))
           ((mnumberp x)
            ;; Add a number into the first term of the list out.
            (return (cons '(mplus)
                          (if (mnumberp (cadr out))
                              (cons (addk (cadr out) x) (cddr out))
                              (cons x (cdr out))))))
           ((not (mplusp x)) (plusin x (cdr out)))
           (t
            ;; At this point we have a mplus expression as argument x. The following
            ;; code assumes that the argument x is already simplified and the terms
            ;; are in a canonical order.
            ;; First we add the number to the first term of the list out.
            (rplaca (cdr out)
                    (addk (if (mnumberp (cadr out)) (cadr out) 0)
                          (cond ((mnumberp (cadr x))
                                 (setq x (cdr x))
                                 (car x))
                                (t 0))))
            ;; Initialize fm with the list of terms and start the loop to add the
            ;; terms of an mplus expression into the list out.
            (do ((x (cdr x) (cdr x))
                 (fm (cdr out)))
                ((null x))
              ;; The return value of PLUSIN is a list, where the first element is the
              ;; added argument and the rest are the terms which follow the added
              ;; argument.
              (setq fm (plusin (car x) fm)))))
     (if (not *plusflag*) (return out))
     (setq *plusflag* nil)   ; *PLUSFLAG* T handles e.g. a+b+3*(a+b)-2*(a+b)
  a  
     ;; *PLUSFLAG* is set by PLUSIN to indicate that a mplus expression is
     ;; part of the result. For this case go again through the terms of the
     ;; result and add any term of the mplus expression into the list out.
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

(defun addk (xx yy)
  (cond ((equal xx 0) yy)
	((equal yy 0) xx)
	((and (numberp xx) (numberp yy)) (+ xx yy))
	(t (prog (g a b (x xx)(y yy))
	      (cond ((numberp x)
		     (cond ((floatp x)
		            (return (+ x (float (/ (cadr y) (caddr y))))))
			   (t (setq x (list '(rat) x 1)))))
		    ((numberp y)
		     (cond ((floatp y)
		            (return (+ y (float (/ (cadr x) (caddr x))))))
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
			    ((equal (caddr g) 1) (cadr g))
			    ($float (float (/ (cadr g) (caddr g))))
	                    (t g)))))))

;;; ----------------------------------------------------------------------------

(defun testt (x)
  (cond ((mnumberp x) x)
        ((null (cddr x)) (cadr x))
        ((eql 1 (cadr x))
         (cond ((null (cdddr x)) (caddr x))
               (t (rplacd x (cddr x)))))
        (t (testtneg x))))

(defun testtneg (x)
  (if (and $negidstrib
           (equal (cadr x) -1)
           (null (cdddr x))
           (mplusp (caddr x)))
      (addn (mapcar #'(lambda (z) (mul -1 z)) (cdaddr x)) t)
      x))

;;; ----------------------------------------------------------------------------

(defprop mtimes simptimes operators)

(defun simptimes (x w z)
  (prog (check res)
    (if (null (cdr x)) (return 1))
    (setq check x)
    (do ((x (cdr x) (cdr x)))
        ((null x))
      (setq w (if z (car x) (simplifya (car x) nil)))
      (setq res (tms w 1 res)))
    (if (mtimesp res) (setq res (testt res)))
    (return (eqtest res check))))

;;; ----------------------------------------------------------------------------

(defun tms (factor power product &aux tem)
  (let ((rulesw nil)
        (z nil))
    (when (mplusp product) (setq product (list '(mtimes simp) product)))
    (cond ((zerop1 factor)
           (cond ((mnegativep power)
                  (if errorsw
                      (throw 'errorsw t)
                      (merror "Division by 0")))
                 (t factor)))
          ((and (null product)
                (or (and (mtimesp factor)
                         (equal power 1))
                    (and (setq product (list '(mtimes) 1)) nil)))
           (setq tem (append '((mtimes)) (if (mnumberp (cadr factor)) nil '(1))
                             (cdr factor) nil))
           (if (= (length tem) 1)
               (setq tem (copy-list tem))
               tem))
          ((mtimesp factor)
           (do ((factor-list (cdr factor) (cdr factor-list)))
               ((or (null factor-list) (zerop1 product)) product)
             (setq z (timesin (car factor-list) (cdr product) power))
             (when rulesw
               (setq rulesw nil)
               (setq product (tms-format-product z)))))
          (t
           (setq z (timesin factor (cdr product) power))
           (if rulesw
               (tms-format-product z)
               product)))))

(defun timesin (x y w)
  (prog (fm temp z check u expo)
     (if (mexptp x) (setq check x))
  top
     ;; Prepare the factor x^w and initialize the work of timesin
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
     ;; Go through the list of terms in fm and look what is to do.
     (cond ((null (cdr fm))
            ;; The list of terms is empty. The loop is finshed.
            (go less))
           ((or (and (mnumberp temp)
                     (not (or (integerp temp)
                              (ratnump temp))))
                (and (integerp temp)
                     (equal temp -1)))
            ;; Stop the loop for a float or bigfloat number, or number -1.
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
           ;; When a number goto start and look in the next term.
           (setq fm (cdr fm))
           (go start))
           
           ((and (not (atom (cadr fm)))
                 (eq (caar (cadr fm)) 'mabs)
                 (integerp (cadr x))
                 (< (cadr x) -1)
                 (alike1 (cadr (cadr fm)) (car x))
                 (not (member ($csign (cadr (cadr fm)))
                                     '($complex imaginary))))
            ;; abs(x)/x^n -> 1/(x^(n-2)*abs(x)), where n an integer.
            ;; Replace abs(x) -> 1/abs(x).
            (setq temp (power (cadr fm) -1))
            (rplacd fm (cddr fm))
            (rplacd fm (cons temp (cdr fm)))
            ;; Multiply factor x^(-n+2) into list of products.
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
           ;; Multiply a number into the list of products.
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
           ;; At this point we will insert a mexpt expression,
           ;; but first we look at the car of the list of products and
           ;; modify the expression if we found a rational number.
           ((and (mexptp temp)
                 (not (onep1 (car y)))
                 (or (integerp (car y))
                     (ratnump (car y))))
            ;; Multiplying x^w * rational or integer.
            (let* ((numerator (if (integerp (car y)) 
                                 (car y)
                                 (second (car y))))
                   (denom (if (integerp (car y)) 
                              1
                              (third (car y)))))
              (setf expo (exponent-of (abs numerator) (car x)))
              (when expo
                ;; We have a^m*a^k.
                (setq temp (power (car x)
                                  (add (cadr x) expo)))
                ;; Set fm to have 1/denom term.
                (setq x (div (div numerator
                                  (power (car x) expo))
                             denom))
                (setf y (rplaca y 1))
                ;; Add in the a^(m+k) term.
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

;;; ----------------------------------------------------------------------------

(defun num1 (a)
  (if (numberp a) a (cadr a)))

(defun denom1 (a)
  (if (numberp a) 1 (caddr a)))

(defun timesk (x y)
  (cond ((equal x 1) y)
        ((equal y 1) x)
        ((and (numberp x) (numberp y)) (* x y))
        ((floatp x) (* x (float (/ (cadr y) (caddr y)))))
        ((floatp y) (* y (float (/ (cadr x) (caddr x)))))
        (t (timeskl x y))))

(defun timeskl (x y)
  (prog (u v g)
     (setq u (simp-rat (num1 x) (denom1 y)))
     (setq v (simp-rat (num1 y) (denom1 x)))
     (setq g (cond ((or (equal u 0) (equal v 0)) 0)
                   ((equal v 1) u)
                   ((and (numberp u) (numberp v)) (* u v))
                   (t
                    (list '(rat simp)
                          (* (num1 u) (num1 v))
                          (* (denom1 u) (denom1 v))))))
     (return (cond ((numberp g) g)
                   ((equal (caddr g) 1) (cadr g))
                   ($float (float (/ (cadr g) (caddr g))))
                   (t g)))))

;;; ----------------------------------------------------------------------------
