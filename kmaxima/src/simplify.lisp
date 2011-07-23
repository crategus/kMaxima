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

(defmvar $negdistrib t)

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

(defun simplifya (x y)
  (declare (ignore y))
  x)

;;; ----------------------------------------------------------------------------
