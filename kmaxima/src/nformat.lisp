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

(defmvar $powerdispflag nil)
(defmvar $ratdispflag nil)
(defmvar $%edispflag nil)
(defmvar $exptdispflag t)
(defmvar $sqrtdispflag t)
(defmvar $negsumdispflag t)

(defun nformat (form)
  (cond ((atom form)
         (cond ((and (numberp form) (minusp form))
                (list '(mminus) (- form)))
               (t form)))
        ((atom (car form)) form)
        ((eq 'rat (caar form))
         (cond ((minusp (cadr form))
                (list '(mminus) (list '(rat) (- (cadr form)) (caddr form))))
               (t (cons '(rat) (cdr form)))))
        ((null (cdar form)) form)
        ((eq 'mplus (caar form)) (nformat-mplus form))
        ((eq 'mtimes (caar form)) (nformat-mtimes form))
        ((eq 'mexpt (caar form)) (nformat-mexpt form))
        (t form)))

(defun nformat-all (form)
  (setq form (nformat form))
  (if (atom form)
      form
      (cons (delete 'simp (copy-list (car form)) :count 1 :test #'eq)
            (mapcar #'nformat-all (cdr form)))))

(defun nformat-mplus (form &aux args)
  (setq args (mapcar #'nformat (cdr form)))
  (cons '(mplus)
        (cond ($powerdispflag
               args)
              ((and $negsumdispflag
                    (null (cdddr form)))
               (if (and (not (mminusp (car args)))
                        (mminusp (cadr args)))
                   args
                   (nreverse args)))
              (t (nreverse args)))))

(defun nformat-mtimes (form)
  (cond ((null (cdr form)) '((mtimes)))
        ((equal -1 (cadr form))
         (list '(mminus) (nformat-mtimes (cdr form))))
        (t
         (prog (num den minus flag)
           (do ((l (cdr form) (cdr l))
                (fact))
               ((null l))
             (setq fact (nformat (car l)))
             (cond ((atom fact) (setq num (cons fact num)))
                   ((eq 'mminus (caar fact))
                    (setq minus (not minus)
                          l (append fact (cdr l))))
                   ((or (eq 'mquotient (caar fact))
                        (and (not $ratdispflag)
                             (eq 'rat (caar fact))))
                    (cond ((not (equal 1 (cadr fact)))
                           (setq num (cons (cadr fact) num))))
                    (setq den (cons (caddr fact) den)))
                   (t (setq num (cons fact num)))))
           (setq num (cond ((null num) 1)
                           ((null (cdr num)) (car num))
                           (t (cons '(mtimes) (nreverse num))))
                 den (cond ((null den) (setq flag t) nil)
                           ((null (cdr den)) (car den))
                           (t (cons '(mtimes) (nreverse den)))))
           (if (not flag)
               (setq num (list '(mquotient) num den)))
           (return (if minus (list '(mminus) num) num))))))

(defun nformat-mexpt (form &aux expr)
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
