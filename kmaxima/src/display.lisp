;;; ----------------------------------------------------------------------------
;;; display.lisp
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

(defmvar $display2d nil)
(defmvar $leftjust nil)
(defmvar $display_format_internal nil)
(defmvar $noundisp nil)

(defvar *display-labels-p* t)
(defvar *displayp* nil)
(defvar *linearray* (make-array 80. :initial-element nil))

(defvar *lines*     1)
(defvar *level*     0)
(defvar *break*     0)
(defvar *size*      2)

(defvar *bkpt*    nil)
(defvar *bkptout*   0)
(defvar *bkptwd*    0)
(defvar *bkptdp*    0)
(defvar *bkptht*    1)

(defvar *bkptlevel* 0)

(defvar *width*     0)
(defvar *height*    0)
(defvar *depth*     0)
(defvar *right*     0)

(defvar *maxht*     1)
(defvar *maxdp*     0)

(defvar *oldrow*    0)
(defvar *oldcol*    0)

(defvar *mratp*   nil)

;;; ----------------------------------------------------------------------------

(defmacro push-string (str sym)
  `(setq ,sym (list* ,@(nreverse (exploden str)) ,sym)))

(defmacro displa-def (op dim-function &rest rest &aux l-dissym r-dissym lbp rbp)
  (dolist (x rest)
    (cond ((typep x 'string)
           (if l-dissym (setq r-dissym x) (setq l-dissym x)))
          ((integerp x)
           (if rbp (setq lbp rbp))
           (setq rbp x))
          (t (merror "DISPLA-DEF: unrecognized object: ~a" x))))
  (when l-dissym
    (setq l-dissym (if r-dissym
                       (cons (exploden l-dissym) (exploden r-dissym))
                       (exploden l-dissym))))
  `(progn 
    (defprop ,op ,dim-function dimension)
    ,(when l-dissym  `(defprop ,op ,l-dissym dissym))
    ,(when lbp       `(defprop ,op ,lbp lbp))
    ,(when rbp       `(defprop ,op ,rbp rbp))))

;;; ----------------------------------------------------------------------------

(defun mdisplay (form)
  (declare (special *linel*))
  (when (not #.ttyoff)
    (cond ($display2d
           (let ((*displayp* t)
                 (*linearray* (if *displayp* (make-array 80.) *linearray*))
                 (*mratp* (checkrat form))
                 (*maxht*     1) (*maxdp*  0) (*width*  0)
                 (*height*    0) (*depth*  0) (*level*  0) (*size*    2)
                 (*break*     0) (*right*  0) (*lines*  1) (*bkpt*  nil)
                 (*bkptwd*    0) (*bkptht* 1) (*bkptdp* 0) (*bkptout* 0)
                 (*bkptlevel* 0) (in-p   nil))
             (unwind-protect
               (progn
                 (setq form (dimension form nil 'mparen 'mparen 0 0))
                 (checkbreak form *width*)
                 (output form (if (and (not $leftjust) (= 2 *lines*))
                                  (- *linel* (- *width* *bkptout*))
                                  0)))
               (fill *linearray* nil))))
          (t
           (fresh-line *standard-output*)
           (mgrind form *standard-output*)
           (terpri)))))

;;; ----------------------------------------------------------------------------

(defun checkrat (form)
  (declare (ignore form))
  nil)

(defun checkfit (w)
  (or (not *break*)
      (<= (- (+ w *break* *right* 1) *bkptwd*) *linel*)))

(defun checkbreak (result w)
  (cond ((not *break*))
        ((> (- (setq w (+ w *break*)) *bkptout*) *linel*)
         (if (or (null *bkpt*)
                 (eq result *bkpt*))
             (merror "display: failed to break up a long expression.~%~
                      display: change 'linel' slightly and try again."))
         (do ((l result (cdr l)))
             ((eq *bkpt* (cdr l)) (rplacd l nil))
           (if (null l)
               (merror "display: 'checkbreak' not found.")))
         (output *bkpt* 0)
         (setq *lines* (1+ *lines*)
               *bkpt* result
               *bkptout* *bkptwd*
               *bkptwd* w
               *bkptht* *maxht*
               *bkptdp* *maxdp*
               *bkptlevel* *level*
               *maxht* 1
               *maxdp* 0))
        ((or (null *bkpt*)
             (<= *level* *bkptlevel*)
             (> (truncate *linel* 2) (- *bkptwd* *bkptout*)))
         (setq *bkpt* result
               *bkptwd* w
               *bkptlevel* *level*
               *bkptht* (max *maxht* *bkptht*)
               *bkptdp* (max *maxdp* *bkptdp*)
               *maxht* 1
               *maxdp* 0))))

(defun forcebreak (result w)
  (output result 0)
  (setq *lines* (+ 2 *lines*)
        *bkpt* nil
        *bkptout* (+ w *break*)
        *maxht* 1
        *maxdp* 0))

;;; ----------------------------------------------------------------------------

(defun output (result w)
  (if (not (interactive-stream-p *standard-input*))
      (fresh-line))
  (if (not #.ttyoff)
      (output-linear (nreverse result) w)))

(defun output-linear (result w)
  (declare (special *bkptdp* *bkptht* *linearray*))
  (draw-linear result *bkptdp* w)
  (do ((i (1- (+ *bkptht* *bkptdp*)) (1- i)))
      ((< i 0))
    (cond ((null (aref *linearray* i)))
          (t (output-linear-one-line i)))))

(defun output-linear-one-line (i)
  (declare (special *linearray*))
  (labels ((tyotbsp (n)
             (do ()
                 ((< n 1))
               (write-char #\space)
               (decf n))))
    (let (line (n 0))
      (setq line (aref *linearray* i)
            line (nreverse (cdr line))
            n (car line))
      (setf (aref *linearray* i) nil)
      (tyotbsp n)
      (loop for v in (cdr line) do (write-char v))
      (terpri))))

(defun draw-linear (dmstr *oldrow* *oldcol*)
  (declare (special *linearray* *oldrow* *oldcol*))
  (do ((line))
      ((null dmstr))
    (cond ((atom (car dmstr))
           (setq line (aref *linearray* *oldrow*))
           (cond ((null line) (setq line (list *oldcol*)))
                 (t
                  (prog (n)
                    (setq n (car line)
                          line (cdr line))
                    (do ()
                        ((<= *oldcol* n))
                      (push #\space line)
                      (incf n)))))
           (do ()
               ((or (null dmstr) (not (atom (car dmstr))))
                (setf (aref *linearray* *oldrow*) (cons *oldcol* line)))
             (incf *oldcol*)
             (push (car dmstr) line)
             (pop dmstr)))
          ((integerp (caar dmstr))
           (setq *oldcol* (draw-linear (reverse  (cddar dmstr))
                                       (+ *oldrow* (cadar dmstr))
                                       (+ *oldcol* (caar dmstr))))
           (pop dmstr))
          (t
           (setq *oldcol* (apply (caar dmstr) (cdar dmstr)))
           (pop dmstr))))
  *oldcol*)

;;; ----------------------------------------------------------------------------

(defun d-hbar (w &optional (char #\-) &aux nl)
  (dotimes (i w)
    (push char nl))
  (draw-linear nl *oldrow* *oldcol*))

(defun d-vbar (h d &optional (char #\|))
  (setq d (- d))
  (do ((i (- h 2) (1- i))
       (nl `((0 ,(1- h) ,char))))
      ((< i d) (draw-linear (nreverse nl) *oldrow* *oldcol*))
    (push `(-1 ,i ,char) nl)))

(defun d-integralsign (&aux dmstr)
  (setq dmstr `((0 2 #\/) (-1 1 #\[) (-1 0 #\I) (-1 -1 #\]) (-1 -2 #\/)))
  (draw-linear dmstr *oldrow* *oldcol*))

(defun d-prodsign (&aux dmstr)
  (setq dmstr '((0 2 #\\ (d-hbar 3 #\=) #\/)
                (-4 0) (d-vbar 2 1 #\!) #\space (d-vbar 2 1 #\!) (1 0)))
  (draw-linear dmstr *oldrow* *oldcol*))

(defun d-sumsign (&aux dmstr)
  (setq dmstr '((0 2 (d-hbar 4 #\=))
                (-4 1 #\\ ) #\> (-2 -1 #\/) (-1 -2 (d-hbar 4 #\=))))
  (draw-linear dmstr *oldrow* *oldcol*))

(defun d-matrix (dir h d)
  (d-vbar h d (car (coerce (if (eq dir 'right) $rmxchar $lmxchar) 'list))))

(defun d-box (h d w body &aux char dmstr)
  (setq char (car (coerce $boxchar 'list)))
  (setq dmstr `((0 ,h (d-hbar ,(+ 2 w) ,char))
                (,(- (+ w 2)) 0)
                (d-vbar ,h ,d ,char)
                ,@body
                (,(- (1+ w)) ,(- (1+ d)) (d-hbar ,(+ w 2) ,char))
                (-1 0)
                (d-vbar ,h ,d ,char)))
      (draw-linear dmstr *oldrow* *oldcol*))

;;; ----------------------------------------------------------------------------

(defun nformat-check (form)
  (if (and $display_format_internal
           (not (or (atom form) (atom (car form)))))
      form
      (nformat form)))

;;; ----------------------------------------------------------------------------

(defun dimension (form result *lop* *rop* w *right*)
  (let ((*level* (1+ *level*))
        (*break* (if (and w *break*) (+ w *break*))))
    (format t "in DIMENSION: w = ~A   *level* = ~A   *break* = ~A~%~%"
            w *level* *break*)
    
    (format t "maxht  = ~A   maxdp = ~A~%" *maxht* *maxdp*)
    (format t "width  = ~A   right = ~A~%" *width* *right*)
    (format t "height = ~A   depth = ~A~%" *height* *depth*)
    (format t "size   = ~A   lines = ~A~%~%" *size* *lines*)

    (format t "bkpt = ~A   bkptout = ~A~%" *bkpt* *bkptout*)
    (format t "bkptwd = ~A   bkptht = ~A~%" *bkptwd* *bkptht*)
    (format t "bkptdp = ~A   bkptlevel = ~A~%~%" *bkptdp* *bkptlevel*)
    
    (setq form (nformat-check form))
    (cond ((atom form)
           (dimension-atom form result))
          ((and (atom (car form)) (setq form (cons '(mprogn) form)) nil))
          ((or (<= (lbp (caar form)) (rbp *lop*)) 
               (> (lbp *rop*) (rbp (caar form))))
           (dimension-paren form result))
          ((member 'array (car form) :test #'eq)
           (dimension-array form result))
          ((getprop (caar form) 'dimension)
           (funcall (getprop (caar form) 'dimension) form result))
          (t
           (dimension-function form result)))))

(defvar atom-context 'dimension-list)

(defun dimension-atom (form result)
  (cond ((and (symbolp form)
              (getprop form atom-context))
         (funcall (getprop form atom-context) form result))
        ((typep form 'string)
         (dimension-string (makestring form) result))
        (t
         (dimension-string (makestring form) result))))

(defun dimension-string (form result &aux crp)
  (declare (special *linel* *break* *bkptout* *width* *height* *depth*))
  (setq *width*  0
        *height* 1
        *depth*  0)
  (do ((l form (cdr l)))
      ((null l))
    (incf *width*)
    (if (char= (car l) #\newline) (setq crp t)))
  (cond ((or (and (checkfit *width*) (not crp))
             (not *break*))
         (nreconc form result))
        (t
         (setq *width* 0)
         (do ((l form)
              (w (- *linel* (- *break* *bkptout*))))
             ((null l) (checkbreak result *width*) result)
           (setq form l
                 l (cdr l))
           (cond ((char= (car form) #\newline)
                  (forcebreak result *width*)
                  (setq result nil
                        w (+ *linel* *width*)))
                 (t
                  (incf *width*)
                  (when (and (= w *width*) l)
                    (forcebreak (cons #\\ result) *width*)
                    (setq result nil
                          w (+ *linel* *width*))
                    (incf *width*))
                  (setq result (rplacd form result))))))))

(defun dimension-paren (form result)
  (setq result
        (cons #\) (dimension form
                             (cons #\( result) 'mparen 'mparen 1 (1+ *right*))))
  (incf *width* 2)
  result)

(defun dimension-function (form result)
  (prog (fun (w 0) (h 0) (d 0))
    (cond ((or (not $noundisp) (not (symbolp (caar form)))))
          ((and (getprop (caar form) 'verb)
                (getprop (caar form) 'alias))
           (push-string "''" result)
           (setq w 2))
          ((and (getprop (caar form) 'noun)
                (not (member (caar form) (cdr $aliases) :test #'eq))
                (not (getprop (caar form) 'reversealias)))
           (setq result (cons #\' result)
                 w 1)))
    (if (eq (caar form) 'mqapply)
        (setq fun (cadr form)
              form (cdr form))
        (setq fun (caar form)))
    (setq result (let ((atom-context 'dimension-function))
                   (dimension fun result *lop* 'mparen 0 1))
          w (+ w *width*)
          h *height*
          d *depth*)
    (cond ((null (cdr form))
           (setq result (list* #\) #\( result)
                 *width* (+ 2 w)))
          (t
           (setq result (let ((*lop* 'mparen)
                              (*rop* 'mparen)
                              (*break* (if *break* (+ 1 w *break*))))
                          (cons #\) (dimension-list form (cons #\( result))))
                 *width* (+ 2 w *width*)
                 *height* (max h *height*)
                 *depth* (max d *depth*))))
    (return result)))

(defun dimension-list (form result)
  (prog ((w 0) (h 0) (d 0))
    (setq result (dimension (cadr form) result *lop* 'mcomma 0 *right*)
          w *width*
          h *height*
          d *depth*)
    (do ((l (cddr form) (cdr l)))
        ((null l))
      (push-string ", " result)
      (incf w 2)
      (checkbreak result w)
      (setq result (dimension (car l) result 'mcomma 'mcomma w *right*)
            w (+ w *width*)
            h (max h *height*)
            d (max d *depth*)))
    (setq *width* w
          *height* h
          *depth* d)
    (return result)))

;;; ----------------------------------------------------------------------------

(displa-def mplus dim-mplus)
(defprop munaryplus (#\+ #\space) dissym)

(defun dim-mplus (form result)
  (cond ((and (null (cddr form))
              (not (member (cadar form) '(trunc exact) :test #'eq)))
         (if (null (cdr form))
             (dimension-function form result)
             (dimension-prefix (cons '(munaryplus) (cdr form)) result)))
        (t
         (setq result (dimension (cadr form) result *lop* 'mplus 0 0))
         (checkbreak result *width*)
         (do ((l (cddr form) (cdr l))
              (w *width*)
              (h *height*)
              (d *depth*)
              (trunc (member 'trunc (cdar form) :test #'eq))
              (dissym))
             ((null l)
              (if trunc
                  (setq *width* (+ 8 w)
                        *height* h
                        *depth* d)
                  (push-string " + . . ." result))
              result)
           (if (mminusp (car l))
               (setq dissym '(#\space #\- #\space) form (cadar l))
               (setq dissym '(#\space #\+ #\space) form (car l)))
           (cond ((and (not trunc) (null (cdr l)))
                  (setq result
                        (dimension form (append dissym result) 
                                        'mplus *rop* (+ 3 w) *right*)
                        *width* (+ 3 w *width*)
                        *height* (max h *height*)
                        *depth* (max d *depth*))
                  (return result))
                 (t
                  (setq result
                        (dimension form (append dissym result)
                                        'mplus 'mplus (+ 3 w) 0)
                        w (+ 3 w *width*)
                        h (max h *height*)
                        d (max d *depth*))
                  (checkbreak result w)))))))

;;; ----------------------------------------------------------------------------

(displa-def rat dim-rat "/")

(defun dim-rat (form result)
  (if $pfeformat
      (dimension-nary form result)
      (dim-mquotient form result)))

(displa-def mquotient dim-mquotient "/")

(defun dim-mquotient (form result)
  (unless (= (length (cdr form)) 2)
    (return-from dim-mquotient (dimension-function form result)))
  (prog (num (w 0) (h 0) (d 0) den)
    (when (and (= 1 *size*) (atom (cadr form)) (atom (caddr form)))
      (return (dimension-nary form result)))
    (setq num (dimension (cadr form) nil 'mparen 'mparen nil *right*)
          w *width*
          h *height*
          d *depth*)
    (unless (checkfit w)
      (return (dimension-nary form result)))
    (setq den (dimension (caddr form) nil 'mparen 'mparen nil *right*))
    (unless (checkfit *width*)
      (return (dimension-nary form result)))
    (return (dratio result num w h d den *width* *height* *depth*))))

(defvar x1)
(defvar x2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq x1 'h1 x2 'd2))

(defun dratio (result num w1 h1 d1 den w2 h2 d2)
  (setq *width* (max w1 w2)
        *height* (+ 1 h1 d1)
        *depth* (+ h2 d2))
  (setq #.x1 (truncate (- *width* w1) 2)
        #.x2 (truncate (- *width* w2) 2))
  (update-heights *height* *depth*)
  (push `(,#.x1 ,(1+ d1) . ,num) result)
  (push `(,(- #.x2 (+ #.x1 w1)) ,(- h2) . ,den) result)
  (push `(,(- 0 #.x2 w2) 0) result)
  (push `(d-hbar ,*width*) result)
  result)

(defun update-heights (ht* dp*)
  (if *break*
      (setq *maxht* (max *maxht* ht*)
            *maxdp* (max *maxdp* dp*))))

;;; ----------------------------------------------------------------------------

(displa-def mtimes dimension-nary " ")
(displa-def mnctimes dimension-nary " . ")

(defun dimension-nary (form result)
  (cond ((null (cddr form))
         (dimension-function form result))
        (t
         (prog (dissym (symlength 0) (w 0) (h 0) (d 0) helper)
           (setq dissym (getprop (caar form) 'dissym)
                 symlength (length dissym)
                 helper (or (getprop (caar form) 'dimension-nary-helper)
                            'dimnary)
                 result (funcall helper
                                 (cadr form)
                                 result *lop* (caar form) (caar form) 0)
                 w *width*
                 h *height*
                 d *depth*)
           (do ((l (cddr form) (cdr l)))
               (nil)
             (checkbreak result w)
             (setq result (revappend dissym result)
                   w (+ symlength w))
             (cond ((null (cdr l))
                    (setq result
                          (funcall helper 
                                   (car l)
                                   result (caar form) (caar form) *rop* w)
                          *width* (+ w *width*)
                          *height* (max h *height*)
                          *depth* (max d *depth*))
                    (return t))
                   (t
                    (setq result
                          (funcall helper
                                   (car l)
                                   result (caar form) (caar form) (caar form) w)
                          w (+ w *width*)
                          h (max h *height*)
                          d (max d *depth*)))))
           (return result)))))

(defun dimnary (form result *lop* op *rop* w)
  (declare (ignore op))
  (if (and (consp form)
           (member (getprop (caar form) 'dimension)
                   '(dimension-infix dimension-nary)))
      (progn
        (format t "in DIMNARY call DIMENSION-PAREN~%")
        (setq result (dimension-paren form result))
        (checkbreak result *width*)
        result)
      (progn
        (format t "in DIMNARY call DIMENSION~%")
        (dimension form result *lop* *rop* w *right*))))

 (defun dimnary-old (form result *lop* op *rop* w)
  (if (and (not (atom form)) (eq (caar form) op))
      (progn
        (format t "in DIMNARY call DIMENSION-PAREN~%")
        (dimension-paren form result))
      (progn
        (format t "in DIMNARY call DIMENSION~%")
        (dimension form result *lop* *rop* w *right*))))

;;; ----------------------------------------------------------------------------

(displa-def mlabel dim-mlabel 0 0)

(defun dim-mlabel (form result)
  (prog (dummy (w 0) (h 0) (d 0))
    (cond ((eq nil (cadr form))
           (setq w 0 h 0 d 0))
          (*mratp*
           (setq result
                 (append *mratp*
                         (if *display-labels-p*
                             (dimension-paren (cadr form) result)))
                 w (+ 4 *width*)
                 h *height*
                 d *depth*))
          (t
           (setq result 
                 (cons #\space 
                       (if *display-labels-p*
                           (dimension-paren (cadr form) result)))
                 w (1+ *width*)
                 h *height*
                 d *depth*)))
    (let ((*level* *linel*)) (checkbreak result w))
    (setq dummy (list 0 0))
    (setq result
          (dimension (caddr form) (cons dummy result) 'mlabel *rop* w *right*))
    (cond ((and (not $leftjust) (= 0 *bkptout*))
           (rplaca dummy (max 0 (- (truncate (- *linel* *width*) 2) w)))
           (setq *width* (+ (car dummy) *width*))))
    (setq *width* (+ w *width*)
          *height* (max h *height*)
          *depth* (max d *depth*))
    (return result)))

;;; ----------------------------------------------------------------------------
