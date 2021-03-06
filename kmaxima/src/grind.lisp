;;; ----------------------------------------------------------------------------
;;; grind.lisp
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

(defvar *linel* 79)

(defmvar $linel 79)
(defprop $linel shadowset assign)
(defprop $linel *linel* shadowvar)

;;; ----------------------------------------------------------------------------

(defun strsym (x) 
  (or (getprop x 'strsym) (getprop x 'dissym)))

;;; ----------------------------------------------------------------------------

(defmvar $stringdispflag nil)
(defmvar $lispdispflag nil)

(defun makestring (x)
  (declare (special $aliases))
  (let (y)
    (cond ((numberp x) (exploden x))
          ((stringp x)
           (setq y (coerce x 'list))
           (if $stringdispflag
               (cons #\" (nconc y (list #\")))
               y))
          ((not (symbolp x)) (exploden x))
          ((and (setq y (getprop x 'reversealias))
                (not (and (member x $aliases :test #'eq)
                          (getprop x 'noun))))
           (exploden (stripdollar y)))
          ((not (eq (getop x) x))
           (makestring (getop x)))
          ((null (setq y (exploden x))))
          ((or (char= #\$ (car y))
               (char= #\% (car y)))
           (cdr y))
          ($lispdispflag (cons #\? y))
          (t y))))

(defun makestring1 (x)
  (let (($stringdispflag nil) ($lispdispflag nil))
    (makestring x)))

;;; ----------------------------------------------------------------------------

(defun mstring (x)
  (labels ((string1 (x l)
             (cond ((atom x) (cons x l))
                   (t
                    (do ((x (cdr x) (cdr x)))
                        ((null x) l)
                      (setq l (string1 (car x) l)))))))
    (nreverse (string1 (msize x nil nil 'mparen 'mparen) nil))))

;;; ----------------------------------------------------------------------------

(let ((chrps 0))
  
  (defun mgrind (form out)
    (setq chrps 0)
    (mprint (msize form nil nil 'mparen 'mparen) out))
  
  (defun mprint (form out)
    (labels ((mtyotbsp (n out)
               (declare (fixnum n))
               (incf chrps n)
               (dotimes (i n)
                 (write-char #\space out)))
             (charpos ()
               (- *linel* chrps)))
      (cond ((characterp form)
             (incf chrps)
             (write-char form out))
            ((< (car form) (charpos))
             (mapc #'(lambda (l) (mprint l out)) (cdr form)))
            (t 
             (prog ((i chrps))
               (mprint (cadr form) out)
               (cond ((null (cddr form)) (return nil))
                     ((and (or (atom (cadr form)) (< (caadr form) (charpos)))
                           (or (> (charpos) (truncate *linel* 2))
                               (atom (caddr form))
                               (< (caaddr form) (charpos))))
                      (setq i chrps)
                      (mprint (caddr form) out))
                     (t
                      (incf i)
                      (setq chrps 0)
                      (terpri out)
                      (mtyotbsp i out)
                      (mprint (caddr form) out)))
               (do ((l (cdddr form) (cdr l)))
                   ((null l))
                 (cond ((or (atom (car l)) (< (caar l) (charpos))) nil)
                       (t
                        (setq chrps 0)
                        (terpri out)
                        (mtyotbsp i out)))
                 (mprint (car l) out)))))))
)

;;; ----------------------------------------------------------------------------

(defvar *lop* nil)
(defvar *rop* nil)

(defun msize (x l r *lop* *rop*)
  (setq x (nformat x))
  (cond ((atom x) (msize-atom x l r))
        ((and (atom (car x)) (setq x (cons '(mprogn) x)) nil))
        ((or (<= (lbp (caar x)) (rbp *lop*))
             (> (lbp *rop*) (rbp (caar x))))
         (msize-paren x l r))
        ((member 'array (cdar x) :test #'eq) (msize-array x l r))
        ((getprop (caar x) 'grind)
         (the (values t) (funcall (get (caar x) 'grind) x l r)))
        (t (msize-function x l r nil))))

;;; ----------------------------------------------------------------------------

(defun msize-paren (x l r)
  (msize x (cons #\( l) (cons #\) r) 'mparen 'mparen))

(defun msize-atom (x l r)
  (declare (special $aliases))
  (labels ((slash (x)
             (do ((l (cdr x) (cdr l)))
                 ((null l))
               (if (or (digit-char-p (car l))
                       (alphabetp (car l)))
                   nil
                   (progn
                     (rplacd l (cons (car l) (cdr l)))
                     (rplaca l #\\ ) (setq l (cdr l)))))
             (if (alphabetp (car x))
                 x
                 (cons #\\ x))))
    (prog (y)
      (cond ((numberp x) (setq y (exploden x)))
            ((eq x nil) (setq y (exploden (stripdollar '$false))))
            ((eq x t) (setq y (exploden (stripdollar '$true))))
            ((stringp x)
             (setq y (coerce x 'list))
             (do ((l y (cdr l)))
                 ((null l))
               (when (member (car l) '(#\" #\\ ) :test #'equal)
                 (rplacd l (cons (car l) (cdr l)))
                 (rplaca l #\\ )
                 (setq l (cdr l))))
             (setq y (cons #\" (nconc y (list #\")))))
            ((and (setq y (getprop x 'reversealias))
                  (not (and (member x $aliases :test #'eq)
                            (getprop x 'noun))))
             (setq y (exploden (stripdollar y))))
            ((setq y (getprop x 'noun))
             (return (msize-atom y l r)))
            ((null (setq y (exploden x))))
            ((char= #\$ (car y)) (setq y (slash (cdr y))))
            (t (setq y (cons #\? (slash y)))))
      (return (msz y l r)))))

(defun msz (x l r)
  (setq x (nreconc l (nconc x r)))
  (cons (length x) x))

(defun msize-array (x l r &aux f)
  (declare (special $aliases))
  (if (eq (caar x) 'mqapply)
      (setq f (cadr x)
            x (cdr x))
      (setq f (caar x)))
  (cond ((and (getprop (caar x) 'verb)
              (getprop (caar x) 'alias))
         (setq l (revappend '(#\' #\' ) l)))
        ((and (getprop (caar x) 'noun)
              (not (member (caar x) (cdr $aliases) :test #'eq))
              (not (get (caar x) 'reversealias)))
         (setq l (cons #\' l))))
  (setq l (msize f l (list #\[ ) *lop* 'mfunction)
        r (msize-list (cdr x) nil (cons #\] r)))
  (cons (+ (car l) (car r)) (cons l (cdr r))))

(defun msize-function (x l r op)
  (declare (special $aliases))
  (cond ((not (symbolp (caar x))))
        ((and (getprop (caar x) 'verb)
              (getprop (caar x) 'alias))
         (setq l (revappend '(#\' #\' ) l)))
        ((and (getprop (caar x) 'noun)
              (not (member (caar x) (cdr $aliases) :test #'eq))
              (not (getprop (caar x) 'reversealias)))
         (setq l (cons #\' l))))
  (setq l (msize (if op
                     (getop (caar x))
                     (caar x))
                 l
                 (list #\( ) 'mparen 'mparen)
        r (msize-list (cdr x) nil (cons #\) r)))
  (cons (+ (car l) (car r)) (cons l (cdr r))))

(defun msize-list (x l r)
  (if (null x)
      (msz nil l r)
      (do ((nl) (w 0))
          ((null (cdr x))
           (setq nl (cons (msize (car x) l r 'mparen 'mparen) nl))
           (cons (+ w (caar nl)) (nreverse nl)))
        (declare (fixnum w))
        (setq nl (cons (msize (car x) l (list #\, ) 'mparen 'mparen) nl)
              w (+ w (caar nl))
              x (cdr x) l nil))))

;;; ----------------------------------------------------------------------------

(defun msize-prefix (x l r)
  (msize (cadr x) (revappend (strsym (caar x)) l) r (caar x) *rop*))

(defun msize-infix (x l r)
  (if (not (= (length (cdr x)) 2))
    (return-from msize-infix (msize-function x l r t)))
  (setq l (msize (cadr x) l nil *lop* (caar x))
        r (msize (caddr x) (reverse (strsym (caar x))) r (caar x) *rop*))
  (list (+ (car l) (car r)) l r))

(defun msize-postfix (x l r)
  (msize (cadr x) l (append (strsym (caar x)) r) *lop* (caar x)))

(defun msize-nofix (x l r)
  (msize (caar x) l r (caar x) *rop*))

(defun msize-matchfix (x l r)
  (setq l (nreconc l (car (strsym (caar x))))
        l (cons (length l) l)
        r (append (cdr (strsym (caar x))) r)
        x (msize-list (cdr x) nil r))
  (cons (+ (car l) (car x)) (cons l (cdr x))))

(defun msize-nary (x l r)
  (msznary x l r (strsym (caar x))))

(defun msznary (x l r strsym)
  (cond ((null (cddr x)) (msize-function x l r t))
        (t
         (setq l (msize (cadr x) l nil *lop* (caar x)))
         (do ((ol (cddr x) (cdr ol)) (nl (list l)) (w (car l)))
             ((null (cdr ol))
              (setq r (msize (car ol) (reverse strsym) r (caar x) *rop*))
              (cons (+ (car r) w) (nreverse (cons r nl))))
           (declare (fixnum w))
           (setq nl
                 (cons (msize (car ol)
                              (reverse strsym) nil (caar x) (caar x))
                       nl)
                 w (+ (caar nl) w))))))

;;; ----------------------------------------------------------------------------

(defprop mparen -1 lbp)
(defprop mparen -1 rbp)

(defprop mprogn msize-matchfix grind)
(defprop mprogn ((#\( ) #\) ) strsym)

(defprop mlist msize-matchfix grind)
(defprop mlist ((#\[ ) #\] ) strsym)

;;; ----------------------------------------------------------------------------

(defprop mlabel msize-mlabel grind)

(defun msize-mlabel (x l r)
  (declare (special *display-labels-p*))
  (cond (*display-labels-p*
         (setq l (msize (cadr x) (list #\( ) (list #\) #\ ) nil nil)
               r (msize (caddr x) nil r 'mparen 'mparen))
         (cons (+ (car l) (car r)) (cons l (cons r nil))))
        (t (msize (caddr x) l r 'mparen 'mparen))))

;;; ----------------------------------------------------------------------------

(defprop mtext msize-mtext grind)

(defun msize-mtext (x l r)
  (setq x (cdr x))
  (if (null x)
      (msz nil l r)
      (do ((nl) (w 0))
          ((null (cdr x))
           (setq nl (cons (if (atom (car x))
                              (msz (makestring (car x)) l r)
                              (msize (car x) l r *lop* *rop*))
                          nl))
           (cons (+ w (caar nl)) (nreverse nl)))
        (setq nl (cons (if (atom (car x))
                           (msz (makestring (car x)) l r)
                           (msize (car x) l nil *lop* *rop*))
                       nl)
              w (+ w (caar nl))
              x (cdr x)
              l nil))))

(defprop mqapply msize-mqapply grind)

(defun msize-mqapply (x l r)
  (setq l (msize (cadr x) l (list #\( ) *lop* 'mfunction)
        r (msize-list (cddr x) nil (cons #\) r)))
  (cons (+ (car l) (car r)) (cons l (cdr r))))

(defprop mquote msize-prefix grind)

(defprop msetq msize-infix grind)
(defprop msetq (#\:) strsym)
(defprop msetq 180 lbp)
(defprop msetq  20 rbp)

(defprop mset msize-infix grind)
(defprop mset (#\: #\:) strsym)
(defprop mset 180 lbp)
(defprop mset  20 rbp)

(defprop mdefine msize-mdef grind)
(defprop mdefine (#\: #\=) strsym)
(defprop mdefine 180 lbp)
(defprop mdefine  20 rbp)

(defprop mdefmacro msize-mdef grind)
(defprop mdefmacro (#\: #\: #\=) strsym)
(defprop mdefmacro 180 lbp)
(defprop mdefmacro  20 rbp)

(defun msize-mdef (x l r)
  (setq l (msize (cadr x) l (copy-list (strsym (caar x))) *lop* (caar x))
        r (msize (caddr x) nil r (caar x) *rop*))
  (cond ((not (atom (cadr l)))
         (setq x (cons (- (car l) (caadr l)) (cddr l)))
         (if (and (not (atom (cadr r)))
                  (not (atom (caddr r)))
                  (< (+ (car l) (caadr r) (caaddr r)) *linel*))
             (setq x (nconc x (list (cadr r) (caddr r)))
                   r (cons (car r) (cdddr r))))
         (cons (+ (car l) (car r)) (cons (cadr l) (cons x (cdr r)))))
        (t
         (cons (+ (car l) (car r)) (cons l (ncons r))))))

;;; ----------------------------------------------------------------------------

(defprop mplus msize-mplus grind)
(defprop mplus 100 lbp)
(defprop mplus 100 rbp)

(defun msize-mplus (x l r)
  (cond ((null (cddr x))
         (if (null (cdr x))
             (msize-function x l r t)
             (msize (cadr x) (append (list #\+ ) l) r 'mplus *rop*)))
        (t
         (setq l (msize (cadr x) l nil *lop* 'mplus)
               x (cddr x))
         (do ((nl (list l)) (w (car l)) (dissym))
             ((null (cdr x))
              (if (mminusp (car x))
                  (setq l (cadar x) 
                        dissym (list #\- ))
                  (setq l (car x)
                        dissym (list #\+ )))
              (setq r (msize l dissym r 'mplus *rop*))
              (cons (+ (car r) w) (nreverse (cons r nl))))
           (declare (fixnum w))
           (if (mminusp (car x))
               (setq l (cadar x) dissym (list #\- ))
               (setq l (car x) dissym (list #\+ )))
           (setq nl (cons (msize l dissym nil 'mplus 'mplus) nl)
                 w (+ (caar nl) w)
                 x (cdr x))))))

(defprop mminus msize-mminus grind)
(defprop mminus (#\-) strsym)
(defprop mminus 100 rbp)
(defprop mminus 100 lbp)

(defun msize-mminus (x l r)
  (cond ((null (cddr x))
         (if (null (cdr x))
             (msize-function x l r t)
             (msize (cadr x) (append (list #\- ) l) r 'mminus *rop*)))
        (t
         (setq l (msize (cadr x) l nil *lop* 'mminus)
               x (cddr x))
         (do ((nl (list l)) (w (car l)) (dissym))
             ((null (cdr x))
              (if (mminusp (car x))
                  (setq l (cadar x) 
                        dissym (list #\+ ))
                  (setq l (car x) 
                        dissym (list #\- )))
              (setq r (msize l dissym r 'mminus *rop*))
              (cons (+ (car r) w) (nreverse (cons r nl))))
           (declare (fixnum w))
           (if (mminusp (car x))
               (setq l (cadar x) dissym (list #\+ ))
               (setq l (car x) dissym (list #\- )))
           (setq nl (cons (msize l dissym nil 'mplus 'mminus) nl)
                 w (+ (caar nl) w)
                 x (cdr x))))))

(defprop mtimes msize-mtimes grind)
(defprop mtimes 120 lbp)
(defprop mtimes 120 rbp)

(defun msize-mtimes (x l r)
  (msznary x l r '(#\* )))

(defprop mnctimes msize-nary grind)
(defprop mnctimes 130 lbp)
(defprop mnctimes 129 rbp)

(defprop mexpt msize-mexpt grind)
(defprop mexpt 140 lbp)
(defprop mexpt 139 rbp)

(defun msize-mexpt (x l r)
  (setq l (msize (cadr x) l nil *lop* 'mexpt)
        r (if (mminusp (setq x (nformat (caddr x))))
              (msize (cadr x) (reverse '(#\^ #\-)) r 'mexpt *rop*)
              (msize x (list #\^) r 'mexpt *rop*)))
  (list (+ (car l) (car r)) l r))

(defprop mncexpt msize-infix grind)
(defprop mncexpt (#\^ #\^) strsym)
(defprop mncexpt 140 lbp)
(defprop mncexpt 139 rbp)

(defprop mquotient msize-infix grind)
(defprop mquotient (#\/) strsym)
(defprop mquotient 120 lbp)
(defprop mquotient 120 rbp)

(defprop rat msize-infix grind)
(defprop rat (#\/) strsym)
(defprop rat 120 lbp)
(defprop rat 120 rbp)

(defprop mfactorial msize-postfix grind)
(defprop mfactorial 160 lbp)

;;; ----------------------------------------------------------------------------

(defprop mequal msize-infix grind)
(defprop mequal (#\=) strsym)
(defprop mequal 80 lbp)
(defprop mequal 80 rbp)

(defprop mnotequal msize-infix grind)
(defprop mnotequal 80 lbp)
(defprop mnotequal 80 rbp)

(defprop mgreaterp msize-infix grind)
(defprop mgreaterp 80 lbp)
(defprop mgreaterp 80 rbp)

(defprop mgeqp msize-infix grind)
(defprop mgeqp 80 lbp)
(defprop mgeqp 80 rbp)

(defprop mlessp msize-infix grind)
(defprop mlessp 80 lbp)
(defprop mlessp 80 rbp)

(defprop mleqp msize-infix grind)
(defprop mleqp 80 lbp)
(defprop mleqp 80 rbp)

(defprop mnot msize-prefix grind)
(defprop mnot 70 rbp)

(defprop mand msize-nary grind)
(defprop mand 65 lbp)
(defprop mand 65 rbp)

(defprop mor msize-nary grind)
(defprop mor 60 lbp)
(defprop mor 60 rbp)

;;; ----------------------------------------------------------------------------

(defprop mcond msize-mcond grind)
(defprop mcond 45 lbp)
(defprop mcond 45 rbp)

(defun msize-mcond (x l r)
  (labels ((strmcond (x)
             (let ((l (reverse (cdr x))))
               (if (and (or (eq (car l) nil)
                            (eq (car l) '$false))
                        (eq (cadr l) t))
                   (setq l (reverse (cddr l)))
                   (setq l (reverse l)))
               (append `($if)
                       (do ((l l (cddr l))
                            (sym nil '$elseif)
                            (res nil))
                           ((null (cddr l))
                            (if (and sym
                                     (not (eq t (car l))))
                                (append res `(,sym ,(car l) $then ,(cadr l)))
                                (if (eq t (car l))
                                    (append res `($else ,(cadr l)))
                                    (append res
                                            `(,(car l) $then ,(cadr l))))))
                         (setq res (append res
                                           (if sym
                                               `(,sym ,(car l)) `(,(car l)))
                                           `($then ,(cadr l)))))))))
    (msznary (cons '(mcond) (strmcond x)) l r '(#\space))))

;;; ----------------------------------------------------------------------------

(defprop mdo msize-mdo grind)
(defprop mdo 25 lbp)
(defprop mdo 25 rbp)

(defun msize-mdo (x l r)
  (labels ((strmdo (x)
             (nconc (cond ((second x) `($for ,(second x))))
                    (cond ((eql 1 (third x)) nil)
                          ((third x)  `($from ,(third x))))
                    (cond ((eql 1 (fourth x)) nil)
                          ((fourth x) `($step ,(fourth x)))
                          ((fifth x)  `($next ,(fifth x))))
                    (cond ((sixth x)  `($thru ,(sixth x))))
                    (cond ((null (seventh x)) nil)
                          ((and (consp (seventh x))
                                (eq 'mnot (caar (seventh x))))
                           `($while ,(cadr (seventh x))))
                          (t `($unless ,(seventh x))))
                    `($do ,(eighth x)))))
    (msznary (cons '(mdo) (strmdo x)) l r '(#\space))))

;;; ----------------------------------------------------------------------------

(defprop mdoin msize-mdoin grind)
(defprop mdoin 30 lbp)
(defprop mdoin 30 rbp)

(defun msize-mdoin (x l r)
  (labels ((strmdoin (x)
             (nconc `($for ,(second x) $in ,(third x))
                    (cond ((null (seventh x)) nil)
                          ((and (consp (seventh x))
                                (eq 'mnot (caar (seventh x))))
                           `($while ,(cadr (seventh x))))
                          (t `($unless ,(seventh x))))
                    `($do ,(eighth x)))))
    (msznary (cons '(mdo) (strmdoin x)) l r '(#\space))))

;;; ----------------------------------------------------------------------------
