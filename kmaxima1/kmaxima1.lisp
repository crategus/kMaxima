;;; ----------------------------------------------------------------------------
;;; kmaxima1.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
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

(in-package :cl-user)

(defpackage :kmaxima
  (:nicknames :kmaxima)
  (:use :cl))

(in-package :kmaxima)

;;; ----------------------------------------------------------------------------

(defvar *maxima-version* 0.1)

(defun maxima-banner ()
  (format t "~&kMaxima ~a~%" *maxima-version*)
  (format t "using Lisp ~a ~a~%" (lisp-implementation-type)
                                 (lisp-implementation-version))
  (format t "Distributed under the GNU Public License. ~
             See the file COPYING.~%")
  (format t "Dedicated to the memory of William Schelter.~%"))

;;; ----------------------------------------------------------------------------

(defun bye ()
  #+(or cmu scl clisp) (ext:quit)
  #+sbcl               (sb-ext:quit)
  #+allegro            (excl:exit)
  #+(or mcl openmcl)   (ccl:quit)
  #+gcl                (lisp:quit)
  #+ecl                (si:quit)
  #+lispworks          (lispworks:quit))

(defun $quit ()
  (throw 'maxima-quit-to-lisp 0))

;;; ----------------------------------------------------------------------------

(defun putprop (sym val indic)
  (and (symbolp sym)
       (setf (get sym indic) val)))

(defmacro defprop (sym val indic)
  `(putprop ',sym ',val ',indic))

(defun getprop (sym indic)
  (and (symbolp sym)
       (get sym indic)))

;;; ----------------------------------------------------------------------------

(defun mfunctionp (x)
  (cond ((symbolp x)
         (and (not (macro-function x))
              (fboundp x) t))
        ((functionp x))))

;;; ----------------------------------------------------------------------------

(proclaim '(ftype (function (*) *) meval))

(defun mevalargs (args)
  (mapcar #'meval args))

;;; ----------------------------------------------------------------------------

(defun meval (form &aux u)
  (cond 
    ((atom form)
     (cond ((not (symbolp form))
            form)
           ((not (boundp form))
            form)
           (t (symbol-value form))))
    ((consp (car form))
     (let ((op (caar form)))
       (cond ((mfunctionp op)
              (apply op (mevalargs (cdr form))))
             ((setq u (getprop op 'mspec))
              (apply u (cons form nil)))
             ((macro-function op)
              (eval (cons op (cdr form))))
             (t
              (cons (car form) (mevalargs (cdr form)))))))
    (t (eval form))))

;;; ----------------------------------------------------------------------------

(defun maxima-toplevel-loop (input-stream mode)
  (declare (ignore input-stream mode))
  (loop
    (format t "~%~a> " (package-name *package*))
    (finish-output)
    (format t "~{~&~S~}" (multiple-value-list (meval (read))))))

;;; ----------------------------------------------------------------------------

(defvar *maxima-quiet* nil)
(defvar *maxima-epilog* "")

(let ((maxima-started nil))
  (defun maxima-toplevel (input-stream mode)
    (in-package :kmaxima)
    (if maxima-started
        (format t "kMaxima restarted.~%")
        (progn
          (if (not *maxima-quiet*) (maxima-banner))
          (setq maxima-started t)))
    (catch 'maxima-quit-toplevel
           (loop
             (catch 'maxima-continue
                    (maxima-toplevel-loop input-stream mode)
                    (format t *maxima-epilog*)
                    (bye))))))

;;; ----------------------------------------------------------------------------

(defun cl-user::run ()
  (in-package :kmaxima)
  (let ((input-stream *standard-input*)
        (mode nil))
    (catch 'maxima-quit-to-lisp
           (loop
            (with-simple-restart (kmaxima "Return to kMaxima top level.")
              (maxima-toplevel input-stream mode))))))

(import 'cl-user::run)

;;; ----------------------------------------------------------------------------

(defmacro defun-prop (f arg &body body)
  `(setf (get ',(first f) ',(second f)) #'(lambda ,arg ,@body)))

(defmacro defmspec (function . rest)
  `(progn
     (defun-prop (,function mspec) ,@rest)))

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

(defun $writefile (filename)
  (let ((msg (dribble filename)))
    (format t "~&~A~&" msg)
    '$done))

(defun $closefile ()
  (let ((msg (dribble)))
    (format t "~&~A~&" msg))
  '$done)

;;; ----------------------------------------------------------------------------

(defun merror (message &rest args)
  (let ((msg `(t ,message ,@args)))
    (apply #'format msg)
    (format t "~& -- an error. To debug this try: debugmode(true);~%")
    (throw 'maxima-continue 'maxima-error)))

;;; ----------------------------------------------------------------------------

(defvar *values* nil)
(defvar *options* nil)

(defmvar $optionset nil)

(defun mset (x y)
  (cond ((symbolp x)
         (let ((f (getprop x 'assign)))
           (if (and f (or (not (eq x y))
                          (eq f 'neverset)))
               (if (eq (funcall f x y) 'munbindp)
                   (return-from mset nil))))
         (cond ((not (boundp x))
                (push x *values*))
               ((and (not (eq x y))
                     (boundp x)
                     (not (member x *values*)))
                (if $optionset
                    (format t "assignment: assigning to option ~A~%" x))
                (push x *options*)))
         (return-from mset (setf (symbol-value x) y)))
        (t (merror "assignment: cannot assign to ~A~%" x))))

;;; ----------------------------------------------------------------------------

(defvar *munbindp* nil)

(defun mseterror (var val)
  (declare (special *munbindp*))
  (if *munbindp*
      'munbindp
      (merror "assignment: cannot assign ~A to ~A" val var)))

;;; ----------------------------------------------------------------------------

(defun neverset (var val)
  (mseterror var val))

(defun booleset (x y)
  (if (not (member y '(t nil $false $true)))
      (mseterror x y)))

(defun shadowset (var val)
  (mset (get var 'shadowvar) val))

;;; ----------------------------------------------------------------------------

(defun $values ()
  (cons '(mlist simp) (copy-list *values*)))

(defun $options ()
  (cons '(mlist simp) (copy-list *options*)))

;;; ----------------------------------------------------------------------------

(defmspec mquote (form)
  (cadr form))

(defmspec msetq (l)
  (mset (cadr l) (meval (caddr l))))

;;; ----------------------------------------------------------------------------

(defun reset1 (args)
  (declare (special *variable-initial-values*))
  (labels ((maybe-reset (key val)
             (let ((reset nil))
               (when (and (boundp key)
                          ;; equalp must be generalized for Maxima forms.
                          (not (equalp (symbol-value key) val)))
                 (setq reset key)
                 (let ((*munbindp* t)    ; no error, when reseting
                       ($optionset nil)) ; no message, when reseting
                   (declare (special *munbindp* $optionset))
                   (meval `((msetq) ,key ((mquote) ,val)))))
               reset)))
    (let ((actually-reset nil))
      (if args
        (mapcar
          #'(lambda (key)
              (multiple-value-bind (val found-p)
                  (gethash key *variable-initial-values*)
                (if found-p
                    (if (maybe-reset key val)
                        (push key actually-reset)))))
          args)
        (maphash
          #'(lambda (key val)
              (if (maybe-reset key val)
                  (push key actually-reset)))
          *variable-initial-values*))
      (cons '(mlist) (nreverse actually-reset)))))

(defmspec $reset (l)
  (reset1 (cdr l)))

;;; ----------------------------------------------------------------------------
