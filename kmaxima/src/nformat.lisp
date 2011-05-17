;;; ----------------------------------------------------------------------------
;;; nformat.lisp
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

(defmvar $powerdisp nil)
(defmvar $pfeformat nil)
(defmvar $%edispflag nil)
(defmvar $exptdispflag t)
(defmvar $sqrtdispflag t)
(defmvar $negsumdispflag t)

(defun nformat (form)
  (cond ((atom form)
         (cond ((complexp form)
                (list '(mplus) (realpart form)
                               (cons '(mtimes)
                                     (if (minusp (imagpart form))
                                         (list (cons '(mminus)
                                                     (list (- (imagpart form))))
                                               '$%i)
                                         (list (imagpart form) '$%i)))))
               ((and (numberp form) (minusp form))
                (list '(mminus) (- form)))
               ((eq t form) '$true)
               ((eq nil form) '$false)
               (t form)))
        ((atom (car form)) form)
        ((eq 'rat (caar form))
         (cond ((minusp (cadr form))
                (list '(mminus) (list '(rat) (- (cadr form)) (caddr form))))
               (t (cons '(rat) (cdr form)))))
        ((null (cdar form)) form)
        ((eq 'mplus (caar form)) (form-mplus form))
        ((eq 'mtimes (caar form)) (form-mtimes form))
        ((eq 'mexpt (caar form)) (form-mexpt form))
        (t form)))

(defun form-mplus (form &aux args trunc)
  (setq args (mapcar #'nformat (cdr form)))
  (setq trunc (member 'trunc (cdar form) :test #'eq))
  (cons (if trunc '(mplus trunc) '(mplus))
        (cond ((and (member 'ratsimp (cdar form) :test #'eq)
                    (not (member 'simp (cdar form) :test #'eq)))
               (if $powerdisp (nreverse args) args))
              ((and trunc
                    (not (member 'simp (cdar form) :test #'eq)))
               (nreverse args))
              ((or $powerdisp
                   trunc
                   (member 'cf (cdar form) :test #'eq))
               args)
              ((and $negsumdispflag (null (cdddr form)))
               (if (and (not (mminusp (car args)))
                        (mminusp (cadr args)))
                   args
                   (nreverse args)))
              (t (nreverse args)))))

(defun form-mtimes (form)
  (cond ((null (cdr form)) '((mtimes)))
        ((equal -1 (cadr form)) (list '(mminus) (form-mtimes (cdr form))))
        (t
         (prog (num den minus flag)
           (do ((l (cdr form) (cdr l)) (dummy))
               ((null l))
             (setq dummy (nformat (car l)))
             (cond ((atom dummy) (setq num (cons dummy num)))
                   ((eq 'mminus (caar dummy))
                    (setq minus (not minus) l (append dummy (cdr l))))
                   ((or (eq 'mquotient (caar dummy))
                        (and (not $pfeformat) (eq 'rat (caar dummy))))
                    (cond ((not (equal 1 (cadr dummy)))
                           (setq num (cons (cadr dummy) num))))
                    (setq den (cons (caddr dummy) den)))
                   (t (setq num (cons dummy num)))))
           (setq num (cond ((null num) 1)
                           ((null (cdr num)) (car num))
                           (t (cons '(mtimes) (nreverse num))))
                 den (cond ((null den) (setq flag t) nil)
                           ((null (cdr den)) (car den))
                           (t (cons '(mtimes) (nreverse den)))))
           (if (not flag)
               (setq num (list '(mquotient) num den)))
           (return (if minus (list '(mminus) num) num))))))

(defun form-mexpt (form &aux expr)
  (cond ((and $sqrtdispflag (alike1 '((rat) 1 2) (caddr form)))
         (list '(%sqrt) (cadr form)))
        ((and $sqrtdispflag (alike1 '((rat) -1 2) (caddr form)))
         (list '(mquotient) 1 (list '(%sqrt) (cadr form))))
        ((and (or (and $%edispflag (eq '$%e (cadr form)))
                  (and $exptdispflag (not (eq '$%e (cadr form)))))
              (not (atom (setq expr (nformat (caddr form)))))
              (eq 'mminus (caar expr)))
         (list '(mquotient) 1 (if (equal 1 (cadr expr)) 
                                  (cadr form)
                                  (list '(mexpt) (cadr form) (cadr expr)))))
        (t (cons '(mexpt) (cdr form)))))

;;; ----------------------------------------------------------------------------
