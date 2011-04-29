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

(defvar *chrps* 0)
(defvar *linel* 79)

(defvar *lop* nil)
(defvar *rop* nil)

;;; ----------------------------------------------------------------------------

(defun strsym (x) 
  (or (getprop x 'strsym) (getprop x 'dissym)))

;;; ----------------------------------------------------------------------------

(defmvar $stringdisp nil)
(defmvar $lispdisp nil)

(defun makestring (x)
  (declare (special $aliases))
  (let (dummy)
    (cond ((numberp x) (exploden x))
          ((stringp x)
           (setq dummy (coerce x 'list))
           (if $stringdisp
               (cons #\" (nconc dummy (list #\")))
               dummy))
          ((not (symbolp x)) (exploden x))
          ((and (setq dummy (getprop x 'reversealias))
                (not (and (member x $aliases :test #'eq) 
                          (getprop x 'noun))))
           (exploden (stripdollar dummy)))
          ((not (eq (getop x) x))
           (makestring (getop x)))
          (t
           (setq dummy (exploden x))
           (cond ((null dummy) nil)
                 ((char= #\$ (car dummy)) (cdr dummy))
                 ((char= #\% (car dummy)) (cdr dummy))
                 ($lispdisp (cons #\? dummy))
                 (t dummy))))))

(defun makestring1 (x)
  (let (($stringdisp nil) ($lispdisp nil))
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

(defun mgrind (form out)
  (setq *chrps* 0)
  (mprint (msize form nil nil 'mparen 'mparen) out))

(defun mprint (form out)
  (labels ((mtyotbsp (n out)
             (declare (fixnum n))
             (incf *chrps* n)
             (dotimes (i n)
               (write-char #\space out)))
           (charpos ()
             (- *linel* *chrps*)))
    (cond ((characterp form)
           (incf *chrps*)
           (write-char form out))
          ((< (car form) (charpos))
           (mapc #'(lambda (l) (mprint l out)) (cdr form)))
          (t 
           (prog ((i *chrps*))
             (mprint (cadr form) out)
             (cond ((null (cddr form)) (return nil))
                   ((and (or (atom (cadr form)) (< (caadr form) (charpos)))
                         (or (> (charpos) (truncate *linel* 2))
                             (atom (caddr form))
                             (< (caaddr form) (charpos))))
                    (setq i *chrps*)
                    (mprint (caddr form) out))
                   (t
                    (incf i)
                    (setq *chrps* 0)
                    (terpri out)
                    (mtyotbsp i out)
                    (mprint (caddr form) out)))
             (do ((l (cdddr form) (cdr l)))
                 ((null l))
               (cond ((or (atom (car l)) (< (caar l) (charpos))) nil)
                     (t
                      (setq *chrps* 0)
                      (terpri out)
                      (mtyotbsp i out)))
               (mprint (car l) out)))))))

;;; ----------------------------------------------------------------------------

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

(defun msize-paren (x l r)
  (msize x (cons #\( l) (cons #\) r) 'mparen 'mparen))

(defun msize-atom (x l r)
  (declare (special $aliases))
  (labels ((slash (x)
             (do ((l (cdr x) (cdr l)))
                 ((null l))
               (if (or (digit-char-p (car l)) (alphabetp (car l)))
                   nil
                   (progn
                     (rplacd l (cons (car l) (cdr l)))
                     (rplaca l #\\ ) (setq l (cdr l)))))
             (if (alphabetp (car x)) x (cons #\\ x))))
    (prog (y)
      (cond ((numberp x) (setq y (exploden x)))
            ((stringp x)
             (setq y (coerce x 'list))
             (do ((l y (cdr l)))
                 ((null l))
               (cond ((member (car l) '(#\" #\\ ) :test #'equal)
                      (rplacd l (cons (car l) (cdr l)))
                      (rplaca l #\\ )
                      (setq l (cdr l)))))
             (setq y (cons #\" (nconc y (list #\")))))
            ((and (setq y (getprop x 'reversealias))
                  (not (and (member x $aliases :test #'eq)
                            (getprop x 'noun))))
             (setq y (exploden (stripdollar y))))
            ((null (setq y (exploden x))))
            ((getprop x 'noun) (return (msize-atom (getprop x 'noun) l r)))
            ((char= #\$ (car y)) (setq y (slash (cdr y))))
            (t (setq y (cons #\? (slash y)))))
      (return (msz y l r)))))

(defun msz (x l r)
  (setq x (nreconc l (nconc x r))) (cons (length x) x))

(defvar lb #\[)
(defvar rb #\])

(defun msize-array (x l r &aux f)
  (declare (special $aliases))
  (if (eq (caar x) 'mqapply) 
      (setq f (cadr x)
            x (cdr x))
      (setq f (caar x)))
  (cond ((and (symbolp (caar x))
              (getprop (caar x) 'verb)
              (getprop (caar x) 'alias))
         (setq l (revappend '(#\' #\') l)))
        ((and (symbolp (caar x))
              (get (caar x) 'noun)
              (not (member (caar x) (cdr $aliases) :test #'eq))
              (not (get (caar x) 'reversealias)))
         (setq l (cons #\' l))))
  (setq l (msize f l (list lb) *lop* 'mfunction)
        r (msize-list (cdr x) nil (cons rb r)))
  (cons (+ (car l) (car r)) (cons l (cdr r))))

(defun msize-function (x l r op)
  (declare (special $aliases))
  (cond ((not (symbolp (caar x))))
        ((and (getprop (caar x) 'verb) (getprop (caar x) 'alias))
         (setq l (revappend '(#\' #\') l)))
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

;;; ----------------------------------------------------------------------------

(defprop mprogn  msize-matchfix grind)
(defprop mprogn ((#\( ) #\) ) strsym)

(defprop mlist msize-matchfix grind)
(defprop mlist ((#\[ ) #\] ) strsym)

(defprop mqapply msz-mqapply grind)

(defun msz-mqapply (x l r)
  (setq l (msize (cadr x) l (list #\( ) *lop* 'mfunction)
        r (msize-list (cddr x) nil (cons #\) r)))
  (cons (+ (car l) (car r)) (cons l (cdr r))))

(defprop mquote msize-prefix grind)

(defprop msetq msize-infix grind)
(defprop msetq msize-infix grind)
(defprop msetq (#\:) strsym)
(defprop msetq 180 lbp)
(defprop msetq  20 rbp)

(defprop mset msize-infix grind)
(defprop mset (#\: #\:) strsym)
(defprop mset 180 lbp)
(defprop mset  20 rbp)

(defprop mdefine msz-mdef grind)
(defprop mdefine (#\: #\=) strsym)
(defprop mdefine 180 lbp)
(defprop mdefine  20 rbp)

(defprop mdefmacro msz-mdef grind)
(defprop mdefmacro (#\: #\: #\=) strsym)
(defprop mdefmacro 180 lbp)
(defprop mdefmacro  20 rbp)

(defun msz-mdef (x l r)
  (setq l (msize (cadr x) l (copy-list (strsym (caar x))) *lop* (caar x))
        r (msize (caddr x) nil r (caar x) *rop*))
  (setq x (cons (- (car l) (caadr l)) (cddr l)))
  (if (and (not (atom (cadr r))) (not (atom (caddr r)))
           (< (+ (car l) (caadr r) (caaddr r)) *linel*))
      (setq x (nconc x (list (cadr r) (caddr r)))
            r (cons (car r) (cdddr r))))
  (cons (+ (car l) (car r)) (cons (cadr l) (cons x (cdr r)))))

(defprop mfactorial msize-postfix grind)
(defprop mfactorial 160 lbp)

(defprop mexpt msz-mexpt grind)
(defprop mexpt 140 lbp)
(defprop mexpt 139 rbp)

(defun msz-mexpt (x l r)
  (setq l (msize (cadr x) l nil *lop* 'mexpt)
        r (if (mminusp (setq x (nformat (caddr x))))
              (msize (cadr x) (reverse '(#\^ #\-)) r 'mexpt *rop*)
              (msize x (list #\^) r 'mexpt *rop*)))
  (list (+ (car l) (car r)) l r))

(defprop mncexpt msize-infix grind)
(defprop mncexpt (#\^ #\^) strsym)
(defprop mncexpt 140 lbp)
(defprop mncexpt 139 rbp)

(defprop mnctimes msize-nary grind)
(defprop mnctimes 130 lbp)
(defprop mnctimes 129 rbp)

(defprop mtimes msz-mtimes grind)
(defprop mtimes 120 lbp)
(defprop mtimes 120 rbp)

(defun msz-mtimes (x l r) 
  (msznary x l r '(#\* )))

(defprop mquotient msize-infix grind)
(defprop mquotient (#\/) strsym)
(defprop mquotient 120 lbp)
(defprop mquotient 120 rbp)

(defprop rat msize-infix grind)
(defprop rat (#\/) strsym)
(defprop rat 120 lbp)
(defprop rat 120 rbp)

(defprop mplus msz-mplus grind)
(defprop mplus 100 lbp)
(defprop mplus 100 rbp)

(defun msz-mplus (x l r)
  (cond ((null (cddr x))
         (if (null (cdr x))
             (msize-function x l r t)
             (msize (cadr x) (append (list #\+ ) l) r 'mplus *rop*)))
        (t 
         (setq l (msize (cadr x) l nil *lop* 'mplus) x (cddr x))
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
               (setq l (cadar x) dissym (list #\-))
               (setq l (car x) dissym (list #\+)))
           (setq nl (cons (msize l dissym nil 'mplus 'mplus) nl)
                 w (+ (caar nl) w)
                 x (cdr x))))))

(defprop mminus msize-prefix grind)
(defprop mminus (#\-) strsym)
(defprop mminus 100 rbp)
(defprop mminus 100 lbp)

(defprop mequal msize-infix grind)
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

(defprop mcond msz-mcond grind)
(defprop mcond 45 lbp)
(defprop mcond 45 rbp)

(defprop %mcond msz-mcond grind)
(defprop %mcond 45 lbp)
(defprop %mcond 45 rbp)

(defun msz-mcond (x l r)
  (let ((if (nreconc l '(#\i #\f #\space))))
    (setq if (cons (length if) if)
          l (msize (cadr x) nil nil 'mcond 'mparen))
    (let ((args (cdddr x))
          (else-literal (reverse (exploden " else ")))
          (elseif-literal (reverse (exploden " elseif ")))
          (then-literal (reverse (exploden " then ")))
          (parts)
          (part))
      (let ((sgra (reverse args)))
        (if (and (or (eq (car sgra) nil) (eq (car sgra) '$false)) 
                 (eq (cadr sgra) t))
            (setq args (reverse (cddr sgra)))))
      (setq parts (list if l))
      (setq part (cond ((= (length args) 0)
                        `(,(msize (caddr x) 
                                  (copy-tree then-literal) 
                                  r 'mcond *rop*)))
                       (t
                        `(,(msize (caddr x) 
                                  (copy-tree then-literal) 
                                  nil 'mcond 'mparen))))
            parts (append parts part))
      (loop while (>= (length args) 2) do
            (let ((maybe-elseif (car args)) (else-or-then (cadr args)))
              (cond
                ((= (length args) 2)
                 (cond
                   ((eq maybe-elseif t)
                    (let ((else-arg else-or-then))
                      (setq part `(,(msize else-arg 
                                           (copy-tree else-literal) 
                                           r 'mcond *rop*))
                            parts (append parts part))))
                   (t
                    (let ((elseif-arg maybe-elseif) (then-arg else-or-then))
                      (setq part `(,(msize elseif-arg 
                                           (copy-tree elseif-literal) 
                                           nil 'mcond 'mparen)
                                   ,(msize then-arg 
                                           (copy-tree then-literal) 
                                           r 'mcond *rop*))
                            parts (append parts part))))))
                (t
                 (let ((elseif-arg maybe-elseif) (then-arg else-or-then))
                   (setq part `(,(msize elseif-arg 
                                        (copy-tree elseif-literal) 
                                        nil 'mcond 'mparen)
                                ,(msize then-arg 
                                        (copy-tree then-literal) 
                                        nil 'mcond 'mparen))
                         parts (append parts part))))))
            (setq args (cddr args)))
      (cons (apply '\+ (mapcar #'car parts)) parts))))

(defprop text-string msize-text-string grind)

(defun msize-text-string (x ll r)
  (declare (ignore ll r))
  (cons (length (cdr x)) (cdr x)))

(defprop mdo msz-mdo grind)
(defprop mdo 25 lbp)
(defprop mdo 25 rbp)

(defprop mdoin msz-mdoin grind)
(defprop mdoin 30 lbp)
(defprop mdoin 30 rbp)

(defprop %mdo msz-mdo grind)
(defprop %mdo 25 lbp)
(defprop %mdo 25 rbp)

(defprop %mdoin msz-mdoin grind)
(defprop %mdoin 30 lbp)
(defprop %mdoin 30 rbp)

(defun msz-mdo (x l r)
  (msznary (cons '(mdo) (strmdo x)) l r '(#\space)))

(defun msz-mdoin (x l r)
  (msznary (cons '(mdo) (strmdoin x)) l r '(#\space)))

(defun strmdo (x)
  (nconc (cond ((second x) `($for ,(second x))))
         (cond ((equal 1 (third x)) nil)
               ((third x)  `($from ,(third x))))
         (cond ((equal 1 (fourth x)) nil)
               ((fourth x) `($step ,(fourth x)))
               ((fifth x)  `($next ,(fifth x))))
         (cond ((sixth x)  `($thru ,(sixth x))))
         (cond ((null (seventh x)) nil)
               ((and (consp (seventh x)) (eq 'mnot (caar (seventh x))))
                `($while ,(cadr (seventh x))))
               (t `($unless ,(seventh x))))
         `($do ,(eighth x))))

(defun strmdoin (x)
  (nconc `($for ,(second x) $in ,(third x))
         (cond ((sixth x) `($thru ,(sixth x))))
         (cond ((null (seventh x)) nil)
               ((and (consp (seventh x)) (eq 'mnot (caar (seventh x))))
                `($while ,(cadr (seventh x))))
               (t `($unless ,(seventh x))))
         `($do ,(eighth x))))

;;; ----------------------------------------------------------------------------
