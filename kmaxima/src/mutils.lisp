;;; ----------------------------------------------------------------------------
;;; mutils.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This file contains modified code from:
;;;
;;; Copyright (C) 1984, 1987 William Schelter, University of Texas
;;; Copyright (C) 1981 Massachusetts Institute of Technology
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

(defun mfunctionp (x)
  (cond ((symbolp x)
         (and (not (macro-function x))
              (fboundp x) t))
        ((functionp x))))

;;; ----------------------------------------------------------------------------

(defun fixnump (n)
  (typep n 'fixnum))

;;; ----------------------------------------------------------------------------

(defvar *alphabet* (list #\_ #\%))

(defun alphabetp (ch)
  (and (characterp ch)
       (or (alpha-char-p ch)
           (member ch *alphabet*))))

;;; ----------------------------------------------------------------------------

(defun mminusp (x)
  (and (not (atom x)) (eq (caar x) 'mminusp)))

(defun mlistp (x)
  (and (not (atom x)) (not (atom (car x))) (eq (caar x) 'mlist)))

;;; ----------------------------------------------------------------------------

(defun putprop (sym val indic)
  (and (symbolp sym)
       (setf (get sym indic) val)))

(defmacro defprop (sym val indic)
  `(putprop ',sym ',val ',indic))

(defun getprop (sym indic)
  (and (symbolp sym)
       (get sym indic)))

(defun getpropl (sym indicator-list)
  (cond ((symbolp sym)
         (setq sym (symbol-plist sym))
         (loop for tail on sym by #'cddr
               when (member (car tail) indicator-list :test #'eq)
               do (return tail)))
        (t (return-from getpropl nil))))

;;; ----------------------------------------------------------------------------

(defun add2lnc (item llist)
  (unless (memalike item (if (mlistp llist) (cdr llist) llist))
    (unless (atom item)
      (setf llist
            (delete (assoc (car item) llist :test #'equal)
                    llist :count 1 :test #'equal)))
    (nconc llist (list item))))

;;; ----------------------------------------------------------------------------

(defmvar $fpprintprec 0)

(defvar *maxfpprintprec* (ceiling (log (expt 2 (float-digits 1.0d0)) 10.0)))

(defun exploden (sym)
  (declare (special *maxfpprintprec* $fpprintprec))
  (let (str)
    (cond ((symbolp sym)
           (setq str (print-invert-case sym)))
          ((floatp sym)
           (let ((a (abs sym))
                 (printprec (if (or (= $fpprintprec 0)
                                    (> $fpprintprec *maxfpprintprec*))
                                *maxfpprintprec*
                                $fpprintprec)))
             (multiple-value-bind (form width)
               (cond ((or (zerop a) (<= 1 a 1e7))
                      (values "~vf" (+ 1 printprec)))
                     ((<= 0.001 a 1)
                      (values "~vf" (+ printprec
                                       (cond ((< a 0.01) 3)
                                             ((< a 0.1) 2)
                                             (t 1)))))
                     (t
                      (values "~ve" (+ 5 printprec))))
               (setq str (format nil form width sym)))
             (setq str (string-trim " " str))))
          ((integerp sym)
           (let ((leading-digit (if (> *print-base* 10) #\0 )))
             (setq str (format nil "~A" sym))
             (setq str (coerce str 'list))
             (if (and leading-digit
                      (not (digit-char-p (car str) 10)))
                 (setq str (cons leading-digit str)))
             (return-from exploden str)))
          (t (setq str (format nil "~A" sym))))
    (coerce str 'list)))

;;; ----------------------------------------------------------------------------

(let ((string-for-implode
       (make-array 20 :fill-pointer 0 
                      :adjustable t 
                      :element-type '#.(array-element-type "a"))))
  (defun implode (lis)
    (let ((ar string-for-implode)
          (len (length lis)))
      (unless (> (array-total-size ar) len)
        (setq ar (adjust-array ar (+ len 20))))
      (setf (fill-pointer ar) len)
      (loop
        for v in lis
        for i below len
        do
        (setf (aref ar i)
              (cond ((characterp v) v)
                    ((symbolp v) (char (symbol-name v) 0))
                    ((numberp v) (code-char v)))))
      (intern-invert-case ar))))

;;; ----------------------------------------------------------------------------

(defun symbolconc (&rest syms)
  (intern (apply #'concatenate 'string
                 (mapcar #'(lambda (sym)
                             (cond ((floatp sym)
                                    (format nil "~S" sym))
                                   ((integerp sym)
                                    (format nil "~D" sym))
                                   ((symbolp sym)
                                    (symbol-name sym))
                                   (t sym)))
                         syms))))

;;; ----------------------------------------------------------------------------

(let ((local-table (copy-readtable nil)))
  (setf (readtable-case local-table) :invert)
  (defun print-invert-case (sym)
    (let ((*readtable* local-table)
          (*print-case* :upcase))
      (princ-to-string sym))))

(defun maybe-invert-string (str)
  (let ((all-upper t)
        (all-lower t)
        (len (length str)))
    (dotimes (i len)
      (let ((ch (char str i)))
        (when (both-case-p ch)
          (if (upper-case-p ch)
              (setq all-lower nil)
              (setq all-upper nil)))))
    (cond (all-upper
            (string-downcase str))
          (all-lower
           (string-upcase str))
          (t
           str))))

(defun intern-invert-case (str)
  (intern (maybe-invert-string str) :kmaxima))

;;; ----------------------------------------------------------------------------

(defun maxima-symbol-p (sym)
  (if (or (symbolp sym)
          (stringp sym))
      (car (member (char (string sym) 0) '(#\$ #\%)))))

(defun stripdollar (x)
  (cond ((numberp x) x)
        ((null x) 'false)
        ((eq x t) 'true) 
        ((maxima-symbol-p x)
         (intern (subseq (string x) 1)))
        (t x)))

;;; ----------------------------------------------------------------------------

(defun getalias (x)
  (cond ((getprop x 'alias))
        ((eq x '$false) nil)
        (t x)))

;;; ----------------------------------------------------------------------------

(defun amperchk (name)
  (cond ((symbolp name) name)
        ((stringp name)
         (getalias (or (getopr0 name)
                       (implode (cons #\$ (coerce name 'list))))))))

;;; ----------------------------------------------------------------------------

(defmvar $aliases '((mlist simp)))

(defmspec $alias (form)
  (if (oddp (length (setq form (cdr form))))
      (merror "alias: takes an even number of arguments."))
  (do ((l nil (cons (alias (pop form) (pop form)) l)))
      ((null form)
       `((mlist simp) ,@(nreverse l)))))

(defun alias (x y)
  (unless (and (symbolp x) (symbolp y))
    (merror "alias: the arguments must be symbolic names: found ~M and ~M"
            x y))
  (cond ((eq x y) y)
        ((get x 'reversealias)
         (if (not (eq x y))
             (merror "alias: ~M already is aliased." x)))
        (t
         (putprop x y 'alias)
         (putprop y x 'reversealias)
         (add2lnc y $aliases)
         y)))

(defun remalias (x &optional remp)
  (let ((y (and (or remp
                    (member x (cdr $aliases) :test #'equal))
                (getprop x 'reversealias))))
    (cond ((and y (eq x '%derivative))
           (remprop x 'reversealias)
           (setf $aliases (delete x $aliases :count 1 :test #'eq))
           (remprop '$diff 'alias) '$diff)
          (y
           (remprop x 'reversealias)
           (remprop x 'noun)
           (setf $aliases (delete x $aliases :count 1 :test #'eq))
           (remprop (setq x y) 'alias)
           (remprop x 'verb)
           x))))

;;; ----------------------------------------------------------------------------

(defun $nounify (x)
  (if (not (or (symbolp x) (stringp x)))
      (merror "nounify: argument must be a symbol or a string."))
  (setq x (amperchk x))
  (cond ((getprop x 'verb))
        ((getprop x 'noun) x)
        (t
         (let* ((y (exploden x))
                (u (eql (car y) #\$)))
           (cond ((or u (not (eql (car y) #\%)))
                  (setq y (implode (cons #\% (if u (cdr y) y))))
                  (putprop y x 'noun)
                  (putprop x y 'verb))
                 (t x))))))

(defun $verbify (x)
  (if (not (or (symbolp x) (stringp x)))
      (merror "verbify: argument must be a symbol or a string."))
  (setq x (amperchk x))
  (cond ((getprop x 'noun))
        ((eq x '||) x)
        ((and (char= (char (symbol-name x) 0) #\%)
              (prog2
                ($nounify (implode (cons #\$ (cdr (exploden x)))))
                (getprop x 'noun))))
        (t x)))

;;; ----------------------------------------------------------------------------

(defun alike1 (x y)
  (labels ((memqarr (ll)
             (if (member 'array ll :test #'eq) t)))
    (cond ((eq x y))
          ((atom x)
           (cond ((arrayp x)
                  (and (arrayp y) (lisp-array-alike1 x y)))
                 (t (equal x y))))
          ((atom y) nil)
          (t
           (and (not (atom (car x)))
                (not (atom (car y)))
                (eq (caar x) (caar y))
                (eq (memqarr (cdar x)) (memqarr (cdar y)))
                (alike (cdr x) (cdr y)))))))

(defun lisp-array-alike1 (x y)
  (and (equal (array-dimensions x) (array-dimensions y))
       (progn
         (dotimes (i (array-total-size x))
           (if (not (alike1 (row-major-aref x i) (row-major-aref y i)))
               (return-from lisp-array-alike1 nil)))
         t)))

(defun alike (x y)
  (do ((x x (cdr x))
       (y y (cdr y)))
      ((atom x) (equal x y))
    (if (or (atom y) (not (alike1 (car x) (car y))))
        (return nil))))

(defun memalike (x l)
  (do ((l l (cdr l)))
      ((null l))
    (when (alike1 x (car l)) (return l))))

;;; ----------------------------------------------------------------------------

(defun show-symbols (&optional indic)
  (with-package-iterator (next-symbol (list-all-packages) :internal :external)
    (loop
      (multiple-value-bind (more? symbol) (next-symbol)
        (if more?
            (progn
              (cond
                ((get symbol indic)
                 (format t "~&~15A          ~A" symbol (get symbol indic)))
                ))
          (return))))))

;;; ----------------------------------------------------------------------------
