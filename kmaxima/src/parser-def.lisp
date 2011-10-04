;;; ----------------------------------------------------------------------------
;;; parser-def.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This file contains modified code from:
;;;
;;; Copyright (C) 1984, 1987 William Schelter, University of Texas
;;; Copyright (C) 1980 Massachusetts Institute of Technology
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

(defmacro led-propl () ''(led))
(defmacro nud-propl () ''(nud))

(defun inherit-propl (op-to op-from getl)
  (let ((propl (getpropl op-from getl)))
    (if propl
        (progn
          (remprop op-to (car propl))
          (putprop op-to (cadr propl) (car propl)))
        (merror "has no ~a properties. ~a ~a" getl op-from 'wrng-type-arg))))

(defun make-parser-fun-def (op p bvl body)
  (if (not (consp op))
      `(,(symbolconc 'def- p '-fun) ,op ,bvl ,(car bvl) . ,body)
      `(progn
         ,(make-parser-fun-def (car op) p bvl body)
         ,@(mapcar #'(lambda (x)
                       `(inherit-propl ',x
                                       ',(car op)
                                       (,(symbolconc p '-propl))))
                   (cdr op)))))

;;; ----------------------------------------------------------------------------

(defvar *symbols-defined* nil)
(defvar *maxima-operators* nil)

(defmacro define-initial-symbols (&rest l)
  (let ((*symbols-defined* nil)
        (*maxima-operators* nil))
    (define-initial-symbols* l)
    `(progn
      (setq *symbols-defined* (copy-list ',*symbols-defined*))
      (setq *maxima-operators* (subst () () ',*maxima-operators*)))))

(defun define-initial-symbols* (l)
  (setq *symbols-defined*
        (sort (copy-list l)
              #'(lambda (x y)
                  (< (length (exploden x)) (length (exploden y))))))
  (setq *maxima-operators* (cstrsetup *symbols-defined*)))

(defun define-symbol (x)
  (define-initial-symbols* (cons x *symbols-defined*))
  (symbolconc '$ (maybe-invert-string x)))

(defun undefine-symbol (opr)
  (define-initial-symbols* (delete opr *symbols-defined* :test #'equal)))

(defun cstrsetup (arg)
  (labels ((add2cstr1 (x tree)
             (cond ((null tree) x)
                   ((atom (car tree))
                    (cond ((equal (car tree) (car x))
                           (rplacd tree (add2cstr1 (cdr x) (cdr tree))))
                          (t
                           (list tree (cond ((atom (car x)) x)
                                            ((equal (caar x) 'ans) (car x))
                                            (t x))))))
                   ((equal (caar tree) (car x))
                    (rplacd (car tree) (add2cstr1 (cdr x) (cdar tree)))
                    tree)
                   ((null (cdr tree))
                    (rplacd tree (list x))
                    tree)
                   (t
                    (rplacd tree (add2cstr1 x (cdr tree)))
                    tree)))
           (add2cstr (x tree ans)
             (add2cstr1 (nconc (exploden x) (cons (list 'ans ans) nil)) tree)))
    (do ((arg arg (cdr arg))
         (tree nil))
        ((null arg) (list* () '(ans ()) tree))
      (if (atom (car arg))
          (setq tree
                (add2cstr (car arg)
                          tree
                          (symbolconc '$
                                      (if (stringp (car arg))
                                          (maybe-invert-string (car arg))
                                          (car arg)))))
          (setq tree (add2cstr (caar arg) tree (cadar arg)))))))

;;; ----------------------------------------------------------------------------

(let ((opr-table (make-hash-table :test #'equal)))
  
  (defun getopr0 (x)
    (or (getprop x 'opr)
        (and (stringp x)
             (gethash x opr-table))))
  
  (defun putopr (x y)
    (or (and (symbolp x) (putprop x y 'opr))
        (and (stringp x) (setf (gethash x opr-table) y))))
  
  (defun remopr (x)
    (or (and (symbolp x) (remprop x 'opr))
        (and (stringp x) (remhash x opr-table)))))

(defun getopr (x)
  (or (getopr0 x) x))

(defun getop (x)
  (or (getprop x 'op) x))

(mapc #'(lambda (x)
          (putprop (car x) (cadr x) 'op)
          (putopr (cadr x) (car x)))
      '((mplus "+")      (mminus "-")    (mtimes "*")
        (mexpt "**")     (mexpt "^")     (mnctimes ".")
        (rat "/")        (mquotient "/") (mncexpt "^^")
        (mequal "=")     (mgreaterp ">") (mlessp "<")
        (mleqp "<=")     (mgeqp ">=")    (mnotequal "#")
        (mand "and")     (mor "or")      (mnot "not")
        (msetq ":")      (mdefine ":=")  (mdefmacro "::=")
        (mquote "'")     (mlist "[")     (mset "::")
        (mfactorial "!") (mprogn "(")    (mcond "if")))

;;; ----------------------------------------------------------------------------
