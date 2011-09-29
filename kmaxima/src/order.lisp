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
	(t				;neither is of known type:
	 (alphalessp (format nil "~s" x)(format nil "~s" y)))))

;;; ----------------------------------------------------------------------------

(defun great (x y)
  (cond ((atom x)
         (cond ((atom y)
                (cond ((numberp x)
                       (cond ((numberp y)
                              (setq y (- x y))
                              (cond ((zerop y) (floatp x))
                                    (t (plusp y))))))
                      ((constant x)
                       (cond ((constant y) (alphalessp y x))
                             (t (numberp y))))
                      ((get x '$scalar)
                       (cond ((get y '$scalar) (alphalessp y x))
                             (t (maxima-constantp y))))
                      ((get x '$mainvar)
                       (cond ((get y '$mainvar) (alphalessp y x))
                             (t t)))
                      (t
                       (or (maxima-constantp y)
                           (get y '$scalar)
                           (and (not (get y '$mainvar))
                                (alphalessp y x))))))
               (t (not (ordfna y x)))))
        ((atom y) (ordfna x y))
        ((eq (caar x) 'rat)
         (cond ((eq (caar y) 'rat)
                (> (* (caddr y) (cadr x)) (* (caddr x) (cadr y))))))
        ((eq (caar y) 'rat))
        ((member (caar x) '(mbox mlabox) :test #'eq) (great (cadr x) y))
        ((member (caar y) '(mbox mlabox) :test #'eq) (great x (cadr y)))
        ((or (member (caar x) '(mtimes mplus mexpt %del) :test #'eq)
             (member (caar y) '(mtimes mplus mexpt %del) :test #'eq))
         (ordfn x y))
        ((and (eq (caar x) 'bigfloat) (eq (caar y) 'bigfloat)) (mgrp x y))
        (t
         (do ((x1 (margs x) (cdr x1)) (y1 (margs y) (cdr y1)))
             (())
           (cond ((null x1)
                  (return (cond (y1 nil)
                                ((not (alike1 (mop x) (mop y)))
                                 (great (mop x) (mop y)))
                                ((member 'array (cdar x) :test #'eq) t))))
                 ((null y1) (return t))
                 ((not (alike1 (car x1) (car y1)))
                  (return (great (car x1) (car y1)))))))))

(defun ordfna (e a)
  (cond ((numberp a)
         (or (not (eq (caar e) 'rat))
             (> (cadr e) (* (caddr e) a))))
        ((and (constant a)
              (not (member (caar e) '(mplus mtimes mexpt) :test #'eq)))
         (not (member (caar e) '(rat bigfloat) :test #'eq)))
        ((null (margs e)) nil)
        ((eq (caar e) 'mexpt)
         (cond ((and (maxima-constantp (cadr e))
                     (or (not (constant a)) (not (maxima-constantp (caddr e)))))
                (or (not (free (caddr e) a)) (great (caddr e) a)))
               ((eq (cadr e) a) (great (caddr e) 1))
               (t (great (cadr e) a))))
        ((member (caar e) '(mplus mtimes) :test #'eq)
         (let ((u (car (last e))))
           (cond ((eq u a) (not (ordhack e))) (t (great u a)))))
        ((eq (caar e) '%del))
        ((prog2
           (setq e (car (margs e)))
           (and (not (atom e)) (member (caar e) '(mplus mtimes) :test #'eq)))
         (let ((u (car (last e)))) (or (eq u a) (great u a))))
        ((eq e a))
        (t (great e a))))

(defun ordfn (x y)
  (let ((cx (caar x)) (cy (caar y)))
    (cond ((eq cx '%del) (if (eq cy '%del) (great (cadr x) (cadr y)) t))
          ((eq cy '%del) nil)
          ((or (eq cx 'mtimes) (eq cy 'mtimes))
           (ordlist (factor-list x) (factor-list y) 'mtimes 'mtimes))
          ((or (eq cx 'mplus) (eq cy 'mplus))
           (ordlist (term-list x) (term-list y) 'mplus 'mplus))
          ((eq cx 'mexpt) (ordmexpt x y))
          ((eq cy 'mexpt) (not (ordmexpt y x))))))

(defun ordmexpt (x y)
  (cond ((eq (caar y) 'mexpt)
         (cond ((alike1 (cadr x) (cadr y)) (great (caddr x) (caddr y)))
               ((maxima-constantp (cadr x))
                (if (maxima-constantp (cadr y))
                    (if (or (alike1 (caddr x) (caddr y))
                            (and (mnump (caddr x)) (mnump (caddr y))))
                        (great (cadr x) (cadr y))
                        (great (caddr x) (caddr y)))
                    (great x (cadr y))))
               ((maxima-constantp (cadr y)) (great (cadr x) y))
               ((mnump (caddr x))
                (great (cadr x) (if (mnump (caddr y)) (cadr y) y)))
               ((mnump (caddr y)) (great x (cadr y)))
               (t
                (let ((x1 (simpln1 x)) (y1 (simpln1 y)))
                  (if (alike1 x1 y1)
                      (great (cadr x) (cadr y))
                      (great x1 y1))))))
        ((alike1 (cadr x) y) (great (caddr x) 1))
        ((mnump (caddr x)) (great (cadr x) y))
        (t (great (simpln1 x) (simpln (list '(%log) y) 1 t)))))

