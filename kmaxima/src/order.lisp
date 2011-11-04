;;; ----------------------------------------------------------------------------
;;; order.lisp
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

;;; ----------------------------------------------------------------------------

(defun alphalessp (x y)
  (cond ((numberp x)
	 (if (numberp y) (< x y) t))
	((stringp x)
	 (cond ((numberp y) nil)
	       ((stringp y)
		(string< x y))
	       (t t)))
	((symbolp x)
	 (cond ((or (numberp y) (stringp y)) nil)
	       ((symbolp y)
		(let ((nx (symbol-name x))
		      (ny (symbol-name y)))
		  (declare (string nx ny))
		  (cond ((string< nx ny)
			 t)
			((string= nx ny)
			 (cond ((eq nx ny) nil)
			       ((null (symbol-package x)) nil)
			       ((null (symbol-package y)) nil)
			       (t (string<
				   (package-name (symbol-package x))
				   (package-name (symbol-package y))))))
			(t nil))))
	       ((consp y) t)))
	((listp x)
	 (cond ((or (numberp y) (stringp y)(symbolp y )) nil)
	       ((listp y)
		(or (alphalessp (car x) (car y))
		    (and (equal (car x) (car y))
			 (alphalessp (cdr x) (cdr y)))))
	       (t nil)))
	((or (numberp y) (stringp y) (symbolp y)(consp y))
	 nil)
	(t
	 (alphalessp (format nil "~s" x) (format nil "~s" y)))))

;;; ----------------------------------------------------------------------------

(defun great (x y)
  (cond ((atom x)
         (cond ((atom y)
                (cond ((numberp x)
                       (cond ((numberp y)
                              (setq y (- x y))
                              (cond ((zerop y) (floatp x))
                                    (t (plusp y))))))
                      ((decl-constant x)
                       (cond ((decl-constant y) (alphalessp y x))
                             (t (numberp y))))
                      ((get x '$scalar)
                       (cond ((get y '$scalar) (alphalessp y x))
                             (t (mconstantp y))))
                      ((get x '$mainvar)
                       (cond ((get y '$mainvar) (alphalessp y x))
                             (t t)))
                      (t
                       (or (mconstantp y)
                           (get y '$scalar)
                           (and (not (get y '$mainvar))
                                (alphalessp y x))))))
               (t (not (ordfna y x)))))
        ((atom y) (ordfna x y))
        ((eq (caar x) 'rat)
         (cond ((eq (caar y) 'rat)
                (> (* (caddr y) (cadr x)) (* (caddr x) (cadr y))))))
        ((eq (caar y) 'rat))
        ((or (member (caar x) '(mtimes mplus mexpt) :test #'eq)
             (member (caar y) '(mtimes mplus mexpt) :test #'eq))
         (ordfn x y))
        (t
         (do ((x1 (margs x) (cdr x1))
              (y1 (margs y) (cdr y1)))
             (())
           (cond ((null x1)
                  (return (cond (y1 nil)
                                ((not (alike1 (mop x) (mop y)))
                                 (great (mop x) (mop y)))
                                ((member 'array (cdar x) :test #'eq) t))))
                 ((null y1) (return t))
                 ((not (alike1 (car x1) (car y1)))
                  (return (great (car x1) (car y1)))))))))

(defun ordhack (x)
  (if (and (cddr x) (null (cdddr x)))
      (great (if (eq (caar x) 'mplus) 0 1) (cadr x))))

(defun ordfna (e a)
  (cond ((numberp a)
         (or (not (eq (caar e) 'rat))
             (> (cadr e) (* (caddr e) a))))
        ((and (decl-constant a)
              (not (member (caar e) '(mplus mtimes mexpt) :test #'eq)))
         (not (member (caar e) '(rat bigfloat) :test #'eq)))
        ((null (margs e)) nil)
        ((eq (caar e) 'mexpt)
         (cond ((and (mconstantp (cadr e))
                     (or (not (decl-constant a))
                         (not (mconstantp (caddr e)))))
                (or (not (free (caddr e) a)) (great (caddr e) a)))
               ((eq (cadr e) a) (great (caddr e) 1))
               (t (great (cadr e) a))))
        ((member (caar e) '(mplus mtimes) :test #'eq)
         (let ((u (car (last e))))
           (cond ((eq u a) (not (ordhack e))) (t (great u a)))))
        ((prog2
           (setq e (car (margs e)))
           (and (not (atom e)) (member (caar e) '(mplus mtimes) :test #'eq)))
         (let ((u (car (last e)))) (or (eq u a) (great u a))))
        ((eq e a))
        (t (great e a))))

(defun ordlist (a b cx cy)
  (prog (l1 l2 c d)
    (setq l1 (length a)
          l2 (length b))
  loop
    (cond ((eql l1 0)
           (return (cond ((eql l2 0) (eq cx 'mplus))
                         ((and (eq cx cy) (eql l2 1))
                          (great (if (eq cx 'mplus) 0 1) (car b))))))
          ((eql l2 0)
           (return (not (ordlist b a cy cx)))))
    (setq c (nth (1- l1) a)
          d (nth (1- l2) b))
    (if (not (alike1 c d)) (return (great c d)))
    (setq l1 (1- l1)
          l2 (1- l2))
    (go loop)))

(defun ordfn (x y)
  (labels ((term-list (x)
             (if (mplusp x) (cdr x) (list x)))
           (factor-list (x)
             (if (mtimesp x) (cdr x) (list x))))
    (let ((cx (caar x))
          (cy (caar y)))
      (cond ((or (eq cx 'mtimes) (eq cy 'mtimes))
             (ordlist (factor-list x) (factor-list y) 'mtimes 'mtimes))
            ((or (eq cx 'mplus) (eq cy 'mplus))
             (ordlist (term-list x) (term-list y) 'mplus 'mplus))
            ((eq cx 'mexpt)
             (ordmexpt x y))
            ((eq cy 'mexpt)
             (not (ordmexpt y x)))))))

(defun ordmexpt (x y)
  (cond ((eq (caar y) 'mexpt)
         (cond ((alike1 (cadr x) (cadr y))
                (great (caddr x) (caddr y)))
               ((mconstantp (cadr x))
                (if (mconstantp (cadr y))
                    (if (or (alike1 (caddr x) (caddr y))
                            (and (mnumberp (caddr x))
                                 (mnumberp (caddr y))))
                        (great (cadr x) (cadr y))
                        (great (caddr x) (caddr y)))
                    (great x (cadr y))))
               ((mconstantp (cadr y))
                (great (cadr x) y))
               ((mnumberp (caddr x))
                (great (cadr x) (if (mnumberp (caddr y)) (cadr y) y)))
               ((mnumberp (caddr y)) (great x (cadr y)))
               (t
                (let ((x1 (mul (caddr x) (take '(%log) (cadr x))))
                      (y1 (mul (caddr y) (take '(%log) (cadr y)))))
                  (if (alike1 x1 y1)
                      (great (cadr x) (cadr y))
                      (great x1 y1))))))
        ((alike1 (cadr x) y) (great (caddr x) 1))
        ((mnumberp (caddr x)) (great (cadr x) y))
        (t (great (mul (caddr x) (take '(%log) (cadr x)))
                  (take '(%log) y)))))
