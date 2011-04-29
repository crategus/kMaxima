;;; ----------------------------------------------------------------------------
;;; ldisplay.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This file contains modified code from:
;;;
;;; Copyright (C) 1984, 1987 William Schelter,University of Texas
;;; Copyright (C) 1979 Massachusetts Institute of Technology
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

(defvar *display-labels-p* t)

(defun linear-display (form)
  (declare (special *chrps* *display-labels-p*))
  (fresh-line *standard-output*)
  (cond ((not (atom form))
         (cond ((eq (caar form) 'mlabel)
                (setq *chrps* 0)
                (cond ((and (cadr form) *display-labels-p*)
                       (princ "(")
                       (setq *chrps*
                             (+  3 (length (mgrind (cadr form) nil))))
                       (princ ") ")))
                (mprint (msize (caddr form) nil nil 'mparen 'mparen)
                        *standard-output*))
               ((eq (caar form) 'mtext)
                (do ((form (cdr form) (cdr form))
                     (fortranp))
                    ((null form))
                  (setq fortranp (atom (car form)))
                  (mgrind (car form) *standard-output*)))
               (t
                (mgrind form *standard-output*))))
        (t
         (mgrind form *standard-output*)))
  (terpri))

;;; ----------------------------------------------------------------------------