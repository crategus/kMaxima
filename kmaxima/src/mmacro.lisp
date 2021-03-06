;;; ----------------------------------------------------------------------------
;;; mmacro.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This file contains modified code from:
;;;
;;; Copyright (C) 1984, 1987 William Schelter, University of Texas
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *read-default-float-format* 'double-float))

;;; ----------------------------------------------------------------------------

(defmacro ncons (x)
  `(cons ,x nil))

;;; ----------------------------------------------------------------------------

(defmacro float (x &optional (y 1d0))
  `(cl:float ,x ,y))

;;; ----------------------------------------------------------------------------

(defmacro while (condition &rest body)
  `(do ()
       ((not ,condition))
     ,@body))

;;; ----------------------------------------------------------------------------

(defmacro defun-prop (f arg &body body)
  `(setf (get ',(first f) ',(second f)) #'(lambda ,arg ,@body)))

(defmacro defmspec (func arg &body body)
  `(defun-prop (,func mspec) ,arg ,@body))

;;; ----------------------------------------------------------------------------

(defvar *variable-initial-values* (make-hash-table))

(defmacro defmvar (var &rest val-and-doc)
  (cond ((> (length val-and-doc) 2)
         (setq val-and-doc (list (car val-and-doc) (second val-and-doc)))))
  `(progn
     (unless (gethash ',var *variable-initial-values*)
       (setf (gethash ',var *variable-initial-values*) ,(first val-and-doc)))
     (defvar ,var ,@val-and-doc)))

;;; ----------------------------------------------------------------------------

(defvar *errset* nil)

(defmacro errset (&rest l)
  `(handler-case (list ,(car l))
     (error (e) (when *errset* (error e)))))

;;; ----------------------------------------------------------------------------
