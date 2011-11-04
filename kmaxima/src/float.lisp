;;; ----------------------------------------------------------------------------
;;; float.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This file contains modified code from:
;;;
;;; Copyright (C) 1984, 1987 William Schelter,University of Texas
;;; Copyright (C) 1982 Massachusetts Institute of Technology
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

(eval-when (:compile-toplevel :load-toplevel :execute)
    (defconstant +machine-fixnum-precision+
                 (integer-length most-positive-fixnum)))

;;; ----------------------------------------------------------------------------

(defmvar $float2bf t)
(defmvar $bftorat nil)
(defmvar $bftrunc t)
(defmvar $fpprintprec 0)
(defmvar $maxfpprintprec (ceiling (log (expt 2 (float-digits 1.0)) 10.0)))
(defmvar $fpprec $maxfpprintprec)
(defmvar bigfloatzero '((bigfloat simp 56) 0 0))
(defmvar bigfloatone  '((bigfloat simp 56) #.(expt 2 55) 1))
(defmvar bfhalf       '((bigfloat simp 56) #.(expt 2 55) 0))
(defmvar bfmhalf      '((bigfloat simp 56) #.(- (expt 2 55)) 0))

;;; ----------------------------------------------------------------------------

(defvar fpprec)
(defvar *m)
(defvar *cancelled)
(defvar *decfp nil)

;;; ----------------------------------------------------------------------------

(defprop %cot %tan recip)
(defprop %tan %cot recip)
(defprop %csc %sin recip)
(defprop %sin %csc recip)
(defprop %sec %cos recip)
(defprop %cos %sec recip)

(defprop %coth %tanh recip)
(defprop %tanh %coth recip)
(defprop %csch %sinh recip)
(defprop %sinh %csch recip)
(defprop %sech %cosh recip)
(defprop %sech %cosh recip)

;;; ----------------------------------------------------------------------------

(defun hipart (x nn)
  (if (bignump nn)
      (abs x)
      (haipart x nn)))

(defun haipart (x n)
  (let ((x (abs x)))
    (if (< n 0)
        (if (< (integer-length x) (- n))
            x
            (logand x (1- (ash 1 (- n)))))
        (ash x (min (- n (integer-length x)) 0)))))

;;; ----------------------------------------------------------------------------

(defprop $fpprec fpprec1 assign)

(defun fpprec1 (assign-var q)
  (declare (ignore assign-var))
  (if (or (not (fixnump q)) (< q 1))
      (merror "fpprec: value must be a positive integer; found: ~M" q))
  (setq fpprec (+ 2 (integer-length (expt 10 q)))
        bigfloatone ($bfloat 1)
        bigfloatzero ($bfloat 0)
        bfhalf (list (car bigfloatone) (cadr bigfloatone) 0)
        bfmhalf (list (car bigfloatone) (- (cadr bigfloatone)) 0))
  q)

;;; ----------------------------------------------------------------------------

(defun dim-bigfloat (form result)
  (let (($lispdisp nil))
    (dimension-atom (make-maxima-symbol (fpformat form)) result)))

(defun fpformat (l)
  (if (not (member 'simp (cdar l) :test #'eq))
      (setq l (cons (cons (caar l) (cons 'simp (cdar l))) (cdr l))))
  (cond ((eql (cadr l) 0)
         (if (not (eql (caddr l) 0))
             (merror "fpformat: detected an incorrect form of 0.0b0: ~M, ~M~%"
                    (cadr l) (caddr l)))
         (list '|0| '|.| '|0| '|b| '|0|))
        (t
         (let ((extradigs (floor (1+ (/ (integer-length (caddr l))
                                        #.(/ (log 10.0) (log 2.0))))))
               (*m 1)
               (*cancelled 0))
           (setq l
                 (let ((*decfp t)
                       (fpprec (+ extradigs (decimalsin (- (caddar l) 2))))
                       (of (caddar l))
                       (l (cdr l))
                       (expon nil))
                   (setq expon (- (cadr l) of))
                   (setq l (if (minusp expon)
                               (fpquotient (intofp (car l))
                                           (fpintexpt 2 (- expon) of))
                               (fptimes* (intofp (car l))
                                         (fpintexpt 2 expon of))))
                   (incf fpprec (- extradigs))
                   (list (fpround (car l)) (+ (- extradigs) *m (cadr l))))))
         (let ((*print-base* 10)
               *print-radix*
               (l1 nil))
           (setq l1 (if (not $bftrunc)
                        (explodec (car l))
                        (do ((l (nreverse (explodec (car l))) (cdr l)))
                            ((not (eq '|0| (car l))) (nreverse l)))))
           (nconc (ncons (car l1))
                  (ncons '|.|)
                  (or (and (cdr l1)
                           (cond ((or (zerop $fpprintprec)
                                      (not (< $fpprintprec $fpprec))
                                      (null (cddr l1)))
                                  (cdr l1))
                                 (t
                                  (setq l1 (cdr l1))
                                  (do ((i $fpprintprec (1- i))
                                       (l2))
                                      ((or (< i 2) (null l1))
                                       (cond ((not $bftrunc) (nreverse l2))
                                             (t
                                              (do ((l3 l2 (cdr l3)))
                                                  ((not (eq '|0| (car l3)))
                                                   (nreverse l3))))))
                                    (setq l2 (cons (car l1) l2)
                                          l1 (cdr l1))))))
                      (ncons '|0|))
                  (ncons '|b|)
                  (explodec (1- (cadr l))))))))

(defun decimalsin (x)
  (do ((i (truncate (* 59 x) 196) (1+ i)))
      (nil)
    (when (> (integer-length (expt 10 i)) x)
      (return (1- i)))))

(defun fpintexpt (int nn fixprec)
  (setq fixprec (truncate fixprec (1- (integer-length int))))
  (let ((bas (intofp (expt int (min nn fixprec)))))
    (if (> nn fixprec)
        (fptimes* (intofp (expt int (rem nn fixprec)))
                  (fpexpt bas (truncate nn fixprec)))
        bas)))

;;; ----------------------------------------------------------------------------

(defun check-bigfloat (x)
  (prog ()
    (cond ((not (bigfloatp x)) (return nil))
          ((= fpprec (caddar x))
           (return x))
          ((> fpprec (caddar x))
           (setq x (bcons (list (fpshift (cadr x) (- fpprec (caddar x)))
                                (caddr x)))))
          (t
           (setq x (bcons (list (fpround (cadr x))
                                (+ (caddr x) *m fpprec (- (caddar x))))))))
    (return (if (eql (cadr x) 0) (bcons (list 0 0)) x))))

;;; ----------------------------------------------------------------------------

(defun bigfloat2rat (x)
  (declare (special $ratprint))
  (setq x (check-bigfloat x))
  (let (($float2bf t)
        (exp nil)
        (y nil)
        (sign nil))
    (setq exp (cond ((minusp (cadr x))
                     (setq sign t
                           y (fpration1 (cons (car x) (fpabs (cdr x)))))
                     (rplaca y (* -1 (car y))))
                    (t (fpration1 x))))
    (when $ratprint
      (princ "`rat' replaced ")
      (when sign (princ "-"))
      (princ (maknam (fpformat (cons (car x) (fpabs (cdr x))))))
      (princ " by ")
      (princ (car exp))
      (write-char #\/)
      (princ (cdr exp))
      (princ " = ")
      (setq x ($bfloat (list '(rat simp) (car exp) (cdr exp))))
      (when sign (princ "-"))
      (princ (maknam (fpformat (cons (car x) (fpabs (cdr x))))))
      (terpri))
    exp))

(defun fpration1 (x)
  (let ((fprateps (cdr ($bfloat (if $bftorat
                                    (list '(rat simp) 1 (exptrl 2 (1- fpprec)))
                                    $ratepsilon)))))
    (or (and (equal x bigfloatzero)
             (cons 0 1))
        (prog (y a)
          (return
            (do ((xx x (setq y (invertbigfloat
                                 (bcons 
                                   (fpdifference (cdr xx)
                                                 (cdr ($bfloat a)))))))
                 (num (setq a (fpentier x))
                      (+ (* (setq a (fpentier y)) num) onum))
                 (den 1 (+ (* a den) oden))
                 (onum 1 num)
                 (oden 0 den))
                ((and (not (zerop den))
                      (not (fpgreaterp
                             (fpabs
                               (fpquotient
                                 (fpdifference (cdr x)
                                               (fpquotient (cdr ($bfloat num))
                                                           (cdr ($bfloat den))))
                                 (cdr x)))
                             fprateps)))
                 (cons num den))))))))

;;; ----------------------------------------------------------------------------

(defun float-nan-p (x)
  (and (floatp x)
       (not (= x x))))

(defun float-inf-p (x)
  (and (floatp x)
       (not (float-nan-p x))
       (beyond-extreme-values x)))

(defun beyond-extreme-values (x)
  (multiple-value-bind (most-negative most-positive) (extreme-float-values x)
    (cond ((< x 0) (< x most-negative))
          ((> x 0) (> x most-positive))
          (t nil))))

(defun extreme-float-values (x)
  (case (type-of x)
    (short-float
     (values most-negative-short-float most-positive-short-float))
    (single-float
     (values most-negative-single-float most-positive-single-float))
    (double-float
     (values most-negative-double-float most-positive-double-float))
    (long-float
     (values most-negative-long-float most-positive-long-float))))

(defun floattofp (x)
  (when (float-nan-p x)
    (merror "bfloat: attempted conversion of floating point NaN (not-a-number).~%"))
  (when (float-inf-p x)
    (merror "bfloat: attempted conversion of floating-point infinity.~%"))
  (unless $float2bf
    (mtell "bfloat: converting float ~S to bigfloat.~%" x))
  (if (zerop x)
      (list 0 0)
      (multiple-value-bind (frac exp sign)
          (integer-decode-float x)
        (let ((scale (- fpprec (integer-length frac))))
          (list (ash (* sign frac) scale)
                (+ fpprec (- exp scale)))))))

(defun fp2flo (l)
  (let ((precision (caddar l))
        (mantissa (cadr l))
        (exponent (caddr l))
        (fpprec machine-mantissa-precision)
        (*m 0))
    (setq mantissa
          (/ (fpround mantissa) (expt 2.0 machine-mantissa-precision)))
    (let ((e (+ exponent (- precision) *m machine-mantissa-precision)))
      (if (>= e 1025)
          (merror "float: floating point overflow converting ~:M" l)
          (scale-float mantissa e)))))

(defun fixfloat (x)
  (let (($ratepsilon (expt 2.0 (- machine-mantissa-precision))))
    (maxima-rationalize x)))

;;; ----------------------------------------------------------------------------

(defun bcons (s)
  `((bigfloat simp ,fpprec) . ,s))

(defun intofp (l)
  (cond ((not (atom l)) ($bfloat l))
        ((floatp l) (floattofp l))
        ((eql l 0) '(0 0))
        ((eq l '$%pi) (fppi))
        ((eq l '$%e) (fpe))
        ((eq l '$%gamma) (fpgamma))
        (t (list (fpround l) (+ *m fpprec)))))

(defun fpround (l &aux (*print-base* 10) *print-radix*)
  (prog ()
     (cond
       ((null *decfp)
        (setq *m (- (integer-length l) fpprec))
        (when (= *m 0)
          (setq *cancelled 0)
          (return l))
        (setq adjust (fpshift 1 (1- *m)))
        (when (minusp l) (setq adjust (- adjust)))
        (incf l adjust)
        (setq *m (- (integer-length l) fpprec))
        (setq *cancelled (abs *m))
        (cond ((zerop (hipart l (- *m)))
               (return (fpshift (fpshift l (- -1 *m)) 1)))
              (t (return (fpshift l (- *m))))))
       (t
        (setq *m (- (length (exploden (abs l))) fpprec))
        (setq adjust (fpshift 1 (1- *m)))
        (when (minusp l) (setq adjust (- adjust)))
        (setq adjust (* 5 adjust))
        (setq *m (- (length (exploden (abs (setq l (+ l adjust))))) fpprec))
        (return (fpshift l (- *m)))))))

(defun fpshift (l n)
  (cond ((null *decfp)
         (cond ((and (minusp n) (minusp l))
                (- (ash (- l) n)))
               (t (ash l n))))
        ((> n 0)
         (* l (expt 10 n)))
        ((< n 0)
         (truncate l (expt 10 (- n))))
        (t l)))

(defun fpintpart (f)
  (prog (m)
     (setq m (- fpprec (cadr f)))
     (return (if (> m 0)
                 (truncate (car f) (expt 2 m))
                 (* (car f) (expt 2 (- m)))))))

(defun invertbigfloat (a)
  (let ((b (check-bigfloat a)))
    (if b
        (bcons (fpquotient (fpone) (cdr b)))
        (simplifya (list '(mexpt) a -1) nil))))

(defun fpend (a)
  (cond ((equal (car a) 0) (bcons a))
        ((numberp (car a))
         (setq a (list (fpround (car a)) (+ -8. *m (cadr a))))
         (bcons a))
        (t a)))

(defun fparcsimp (e)
  (if (and (mplusp e) (null (cdddr e))
           (mtimesp (caddr e)) (null (cdddr (caddr e)))
           (bigfloatp (cadr (caddr e)))
           (eq (caddr (caddr e)) '$%i)
           (< (caddr (cadr (caddr e))) (+ (- fpprec) 2)))
      (cadr e)
      e))

;;; ----------------------------------------------------------------------------

(defun $bfloat (x)
  (declare (special $ratprint))
  (let (y)
    (cond ((check-bigfloat x))
          ((or (numberp x)
               (member x '($%e $%pi $%gamma) :test #'eq))
           (bcons (intofp x)))
          ((or (atom x)
               (member 'array (cdar x) :test #'eq))
           (if (eq x '$%phi)
               ($bfloat '((mtimes simp)
                          ((rat simp) 1 2)
                          ((mplus simp) 1 ((mexpt simp) 5 ((rat simp) 1 2)))))
               x))
          ((eq (caar x) 'mexpt)
           (if (eq (cadr x) '$%e)
               (*fpexp ($bfloat (caddr x)))
               (exptbigfloat ($bfloat (cadr x)) (caddr x))))
          ((eq (caar x) 'mncexpt)
           (list '(mncexpt) ($bfloat (cadr x)) (caddr x)))
          ((eq (caar x) 'rat)
           (ratbigfloat (cdr x)))
          ((setq y (getprop (caar x) 'floatprog))
           (funcall y (mapcar #'$bfloat (cdr x))))
          ((or (trigp (caar x))
               (arcp (caar x)))
           (setq y ($bfloat (cadr x)))
           (if (bigfloatp y)
               (cond ((arcp (caar x))
                      (setq y ($bfloat (logarc (caar x) y)))
                      (if (free y '$%i)
                          y
                          (let ($ratprint) (fparcsimp ($rectform y)))))
                     ((member (caar x) '(%cot %sec %csc) :test #'eq)
                      (invertbigfloat
                        ($bfloat (list (ncons (getprop (caar x) 'recip)) y))))
                     (t ($bfloat (exponentialize (caar x) y))))
               (subst0 (list (ncons (caar x)) y) x)))
          (t (recur-apply #'$bfloat x)))))

;;; ----------------------------------------------------------------------------

(defprop rat ratbigfloat floatprog)

(defun ratbigfloat (r)
  (if (zerop (car r))
      (bcons (list 0 0))
      (bcons (float-ratio r))))

(defun float-ratio (x)
  (let* ((signed-num (first x))
         (plusp (plusp signed-num))
         (num (if plusp signed-num (- signed-num)))
         (den (second x))
         (digits fpprec)
         (scale 0))
    (declare (fixnum digits scale))
    (let ((den-twos (1- (integer-length (logxor den (1- den))))))
      (declare (fixnum den-twos))
      (decf scale den-twos)
      (setq den (ash den (- den-twos))))
    (let* ((num-len (integer-length num))
           (den-len (integer-length den))
           (delta (- den-len num-len))
           (shift (1+ (the fixnum (+ delta digits))))
           (shifted-num (ash num shift)))
      (declare (fixnum delta shift))
      (decf scale delta)
      (labels ((float-and-scale (bits)
                 (let* ((bits (ash bits -1))
                        (len (integer-length bits)))
                   (cond ((> len digits)
                          (assert (= len (the fixnum (1+ digits))))
                          (multiple-value-bind (f0)
                              (floatit (ash bits -1))
                            (list (first f0) (+ (second f0)
                                                (1+ scale)))))
                         (t
                          (multiple-value-bind (f0)
                              (floatit bits)
                            (list (first f0) (+ (second f0) scale)))))))
               (floatit (bits)
                 (let ((sign (if plusp 1 -1)))
                   (list (* sign bits) 0))))
        (loop
          (multiple-value-bind (fraction-and-guard rem)
              (truncate shifted-num den)
            (let ((extra (- (integer-length fraction-and-guard) digits)))
              (declare (fixnum extra))
              (cond ((/= extra 1)
                     (assert (> extra 1)))
                    ((oddp fraction-and-guard)
                     (return
                       (if (zerop rem)
                           (float-and-scale
                             (if (zerop (logand fraction-and-guard 2))
                                 fraction-and-guard
                                 (1+ fraction-and-guard)))
                           (float-and-scale (1+ fraction-and-guard)))))
                    (t
                     (return (float-and-scale fraction-and-guard)))))
            (setq shifted-num (ash shifted-num -1))
            (incf scale)))))))

;;; ----------------------------------------------------------------------------

(defprop mplus addbigfloat floatprog)

(defun addbigfloat (h)
  (prog (fans tst r nfans)
    (setq fans (setq tst bigfloatzero)
          nfans 0)
    (do ((l h (cdr l)))
        ((null l))
      (cond ((setq r (check-bigfloat (car l)))
             (setq fans (bcons (fpplus (cdr r) (cdr fans)))))
            (t
             (setq nfans (list '(mplus) (car l) nfans)))))
    (return (cond ((eql nfans 0) fans)
                  ((eql fans tst) nfans)
                  (t (simplifya (list '(mplus) fans nfans) nil))))))

(defun fpplus (a b)
  (prog (*m exp man sticky)
    (setq *cancelled 0)
    (cond ((eql (car a) 0) (return b))
          ((eql (car b) 0) (return a)))
    (setq exp (- (cadr a) (cadr b)))
    (setq man (cond ((eql exp 0)
                     (setq sticky 0)
                     (fpshift (+ (car a) (car b)) 2))
                    ((> exp 0)
                     (setq sticky (hipart (car b) (- 1 exp)))
                     (setq sticky (cond ((zerop sticky) 0)
                                        ((< (car b) 0) -1)
                                        (t 1)))
                     (+ (fpshift (car a) 2)
                        (fpshift (car b) (- 2 exp))))
                    (t
                     (setq sticky (hipart (car a) (1+ exp)))
                     (setq sticky (cond ((zerop sticky) 0)
                                        ((< (car a) 0) -1)
                                        (t 1)))
                     (+ (fpshift (car b) 2)
                        (fpshift (car a) (+ 2 exp))))))
    (setq man (+ man sticky))
    (return (cond ((eql man 0) '(0 0))
                  (t
                   (setq man (fpround man))
                   (setq exp (+ -2 *m (max (cadr a) (cadr b))))
                   (list man exp))))))

;;; ----------------------------------------------------------------------------

(defprop mtimes timesbigfloat floatprog)

(defun timesbigfloat (h)
  (prog (fans r nfans)
    (setq fans (bcons (fpone)) nfans 1)
    (do ((l h (cdr l)))
        ((null l))
      (if (setq r (check-bigfloat (car l)))
          (setq fans (bcons (fptimes* (cdr r) (cdr fans))))
          (setq nfans (list '(mtimes) (car l) nfans))))
    (return (if (eql nfans 1)
                fans
                (simplifya (list '(mtimes) fans nfans) nil)))))

(defun fptimes* (a b)
  (if (or (zerop (car a)) (zerop (car b)))
      '(0 0)
      (list (fpround (* (car a) (car b)))
            (+ *m (cadr a) (cadr b) (- fpprec)))))

;;; ----------------------------------------------------------------------------

(defprop %log logbigfloat floatprog)

(defun logbigfloat (a)
  (cond ((bigfloatp (car a))
         (big-float-log ($bfloat (car a))))
        (t
         (list '(%log) (car a)))))

(defun big-float-log (x &optional y)
  (if y
      (multiple-value-bind (u v) (complex-log x y)
        (add (bcons u) (mul '$%i (bcons v))))
      (flet ((%log (x)
               (cdr
                 (let* ((extra 8)
                        (fpprec (+ fpprec extra))
                        (log-frac
                          (fplog (list (ash (car x) extra) 0)))
                        (log-exp (fptimes* (intofp (second x)) (fplog2)))
                        (result (bcons (fpplus log-frac log-exp))))
                   (let ((fpprec (- fpprec extra)))
                     (check-bigfloat result))))))
        (let ((fp-x (cdr (check-bigfloat x))))
          (if (fplessp fp-x (intofp 0))
              (add (bcons (%log (fpminus fp-x)))
                   (mul '$%i (bcons (fppi))))
              (bcons (%log fp-x)))))))

(defun complex-log (x y)
  (let* ((x (cdr (check-bigfloat x)))
         (y (cdr (check-bigfloat y)))
         (t1 (let (($float2bf t)) (floattofp 1.2)))
         (t2 (intofp 3))
         (rho (fpplus (fptimes* x x) (fptimes* y y)))
         (abs-x (fpabs x))
         (abs-y (fpabs y))
         (beta (fpmax abs-x abs-y))
         (theta (fpmin abs-x abs-y)))
    (values (if (or (fpgreaterp t1 beta)
                    (fplessp rho t2))
                (fpquotient
                  (fplog1p (fpplus (fptimes* (fpdifference beta (fpone))
                                             (fpplus beta (fpone)))
                                   (fptimes* theta theta)))
                 (intofp 2))
                (fpquotient (fplog rho) (intofp 2)))
            (fpatan2 y x))))

(defun fplog (x)
  (prog (over two ans oldans term e sum)
    (unless (> (car x) 0)
      (merror "fplog: argument must be positive; found: ~M" (car x)))
    (setq e (fpe)
          over (fpquotient (fpone) e)
          ans 0)
    (do ()
        (nil)
      (cond ((equal x e) (setq x nil) (return nil))
            ((and (fplessp x e) (fplessp over x))
             (return nil))
            ((fplessp x over)
             (setq x (fptimes* x e))
             (decf ans))
            (t
             (incf ans)
             (setq x (fpquotient x e)))))
    (when (null x) (return (intofp (1+ ans))))
    (setq x (fpdifference  x (fpone))
          ans (intofp ans))
    (setq x
          (fpexpt (setq term (fpquotient x (fpplus x (setq two (intofp 2)))))
                  2))
    (setq sum (intofp 0))
    (do ((n 1 (+ n 2)))
        ((equal sum oldans))
      (setq oldans sum)
      (setq sum (fpplus sum (fpquotient term (intofp n))))
      (setq term (fptimes* term x)))
    (return (fpplus ans (fptimes* two sum)))))

(defun fplog1p (x)
  (cond ((fpgreaterp (fpabs x) (fpone))
         (fplog (fpplus x (fpone))))
        (t
         (let* ((sum (intofp 0))
                (term (fpquotient x (fpplus x (intofp 2))))
                (f (fptimes* term term))
                (oldans nil))
           (do ((n 1 (+ n 2)))
               ((equal sum oldans))
             (setq oldans sum)
             (setq sum (fpplus sum (fpquotient term (intofp n))))
             (setq term (fptimes* term f)))
           (fptimes* sum (intofp 2))))))

;;; ----------------------------------------------------------------------------

(defprop mabs mabsbigfloat floatprog)

(defun mabsbigfloat (l)
  (prog (r)
    (setq r (check-bigfloat (car l)))
    (return (if (null r)
                (list '(mabs) (car l))
                (bcons (fpabs (cdr r)))))))

(defun fpabs (x)
  (if (>= (car x) 0)
      x
      (cons (- (car x)) (cdr x))))

;;; ----------------------------------------------------------------------------

(defun *fpexp (a)
  (fpend (let ((fpprec (+ 8 fpprec)))
           (if (bigfloatp (setq a ($bfloat a)))
               (fpexp (cdr a))
               (list '(mexpt) '$%e a)))))

(defun fpexp (x)
  (prog (r s)
    (unless (>= (car x) 0)
      (return (fpquotient (fpone) (fpexp (fpabs x)))))
    (setq r (fpintpart x))
    (return
      (cond ((< r 2)
             (fpexp1 x))
            (t
             (setq s (fpexp1 (fpdifference x (intofp r))))
             (fptimes* s
                       (cdr (check-bigfloat
                              (let ((fpprec (+ fpprec (integer-length r) -1))
                                    (r r))
                                (bcons (fpexpt (fpe) r)))))))))))

(defun fpexp1 (x)
  (prog (term ans oans)
     (setq ans (setq term (fpone)))
     (do ((n 1 (1+ n)))
         ((equal ans oans))
       (setq term (fpquotient (fptimes* x term) (intofp n)))
       (setq oans ans)
       (setq ans (fpplus ans term)))
     (return ans)))

;;; ----------------------------------------------------------------------------

(defun exptbigfloat (p n)
  (declare (special $numer $float $keepfloat $ratprint))
  (cond ((eql n 1) p)
        ((eql n 0) ($bfloat 1))
        ((not (bigfloatp p)) (list '(mexpt) p n))
        ((eql (cadr p) 0) ($bfloat 0))
        ((and (< (cadr p) 0) (ratnump n))
         (mul (let ($numer $float $keepfloat $ratprint)
                 (power -1 n))
               (exptbigfloat (bcons (fpminus (cdr p))) n)))
        ((and (< (cadr p) 0) (not (integerp n)))
         (cond ((or (eql n 0.5) (equal n bfhalf))
                (exptbigfloat p '((rat simp) 1 2)))
               ((or (eql n -0.5) (equal n bfmhalf))
                (exptbigfloat p '((rat simp) -1 2)))
               ((bigfloatp (setq n ($bfloat n)))
                (cond ((equal n ($bfloat (fpentier n)))
                       (exptbigfloat p (fpentier n)))
                      (t
                       (setq p (exptbigfloat (bcons (fpminus (cdr p))) n)
                             n ($bfloat `((mtimes) $%pi ,n)))
                       (add2 ($bfloat `((mtimes) ,p ,(*fpsin n nil)))
                             `((mtimes simp)
                               ,($bfloat `((mtimes) ,p ,(*fpsin n t)))
                               $%i)))))
               (t (list '(mexpt) p n))))
        ((and (ratnump n) (< (caddr n) 10))
         (bcons (fpexpt (fproot p (caddr n)) (cadr n))))
        ((not (integerp n))
         (setq n ($bfloat n))
         (cond ((not (bigfloatp n)) (list '(mexpt) p n))
               (t
                (let ((extrabits (max 1 (+ (caddr n)
                                           (integer-length (caddr p))))))
                  (setq p
                        (let ((fpprec (+ extrabits fpprec)))
                          (fpexp
                            (fptimes* (cdr (check-bigfloat n))
                                      (fplog (cdr (check-bigfloat p)))))))
                  (setq p
                        (list (fpround (car p))
                              (+ (- extrabits) *m (cadr p))))
                  (bcons p)))))
        ((< n 0) (invertbigfloat (exptbigfloat p (- n))))
        (t (bcons (fpexpt (cdr p) n)))))

(defun fpexpt (p nn)
  (cond ((zerop nn) (fpone))
        ((eql nn 1) p)
        ((< nn 0) (fpquotient (fpone) (fpexpt p (- nn))))
        (t
         (prog (u)
           (if (oddp nn)
               (setq u p)
               (setq u (fpone)))
           (do ((ii (truncate nn 2) (truncate ii 2)))
               ((zerop ii))
             (setq p (fptimes* p p))
             (when (oddp ii) (setq u (fptimes* u p))))
           (return u)))))

;;; ----------------------------------------------------------------------------

(defun big-float-sqrt (x &optional y)
  (if y
      (multiple-value-bind (u v) (complex-sqrt x y)
        (add (bcons u) (mul '$%i (bcons v))))
      (let ((fp-x (cdr (check-bigfloat x))))
        (if (fplessp fp-x (intofp 0))
            (mul '$%i (bcons (fproot (bcons (fpminus fp-x)) 2)))
            (bcons (fproot x 2))))))

(defun complex-sqrt (xx yy)
  (let* ((x (cdr (check-bigfloat xx)))
         (y (cdr (check-bigfloat yy)))
         (rho (fpplus (fptimes* x x)
                      (fptimes* y y))))
    (setf rho (fpplus (fpabs x) (fproot (bcons rho) 2)))
    (setf rho (fpplus rho rho))
    (setf rho (fpquotient (fproot (bcons rho) 2) (intofp 2)))
    (let ((eta rho)
          (nu y))
      (when (fpgreaterp rho (intofp 0))
        (setf nu (fpquotient (fpquotient nu rho) (intofp 2)))
        (when (fplessp x (intofp 0))
          (setf eta (fpabs nu))
          (setf nu (if (minusp (car y))
                       (fpminus rho)
                       rho))))
      (values eta nu))))

(defun fproot (a n)
  (if (eq (cadr a) 0)
      '(0 0)
      (progn
        (let* ((ofprec fpprec)
               (fpprec (+ fpprec 2))
               (bk (fpexpt
                     (intofp 2)
                     (1+ (truncate
                           (cadr (setq a (cdr (check-bigfloat a)))) n)))))
          (do ((x bk (fpdifference
                       x
                       (setq bk
                             (fpquotient
                               (fpdifference x (fpquotient a (fpexpt x n1)))
                               n))))
               (n1 (1- n))
               (n (intofp n)))
              ((or (equal bk '(0 0))
                   (> (- (cadr x) (cadr bk)) ofprec))
               (setq a x))))
        (list (fpround (car a)) (+ -2 *m (cadr a))))))

;;; ----------------------------------------------------------------------------

(defun fpentier (f)
  (let ((fpprec (caddar f)))
    (fpintpart (cdr f))))

;;; ----------------------------------------------------------------------------

(defun fpquotient (a b)
  (cond ((equal (car b) 0)
         (merror "pquotient: attempted quotient by zero."))
        ((eql (car a) 0) '(0 0))
        (t
         (list (fpround (truncate (fpshift (car a) (+ 3 fpprec)) (car b)))
               (+ -3 (- (cadr a) (cadr b)) *m)))))

;;; ----------------------------------------------------------------------------

(defun fpone ()
  (cond (*decfp (intofp 1))
        ((= fpprec (caddar bigfloatone)) (cdr bigfloatone))
        (t (intofp 1))))

;;; ----------------------------------------------------------------------------

(defun fpdifference (a b)
  (fpplus a (fpminus b)))

;;; ----------------------------------------------------------------------------

(defun fpminus (x)
  (if (eql (car x) 0)
      x
      (list (- (car x)) (cadr x))))

;;; ----------------------------------------------------------------------------

(defun fpgreaterp (a b)
  (fpposp (fpdifference a b)))

(defun fplessp (a b)
  (fpposp (fpdifference b a)))

(defun fpposp (x)
  (> (car x) 0))

;;; ----------------------------------------------------------------------------

(defun fpmin (arg1 &rest args)
  (let ((min arg1))
    (mapc #'(lambda (u) (if (fplessp u min) (setq min u))) args)
    min))

(defun fpmax (arg1 &rest args)
  (let ((max arg1))
    (mapc #'(lambda (u) (if (fpgreaterp u max) (setq max u))) args)
    max))

;;; ----------------------------------------------------------------------------

(let ((table (make-hash-table)))
  (defun fpe ()
    (let ((value (gethash fpprec table)))
      (if value
          value
          (setf (gethash fpprec table) (cdr (fpe1))))))
  
  (defun fpe-table ()
    table)
  
  (defun clear_fpe_table ()
    (clrhash table)))

(defun fpe1 nil
  (bcons (list (fpround (compe (+ fpprec 12))) (+ -12 *m))))

(defun compe (prec)
  (let (s h (n 1) d (k (isqrt prec))) 
     (setq h (ash 1 prec))
     (setq s h)
     (do ((i k (+ i k)))
         ((zerop h))
       (setq d (do ((j 1 (1+ j)) (p i))
                   ((> j (1- k)) (* p n))
                 (setq p (* p (- i j)))) )
       (setq n (do ((j (- k 2) (1- j)) (p 1))
                   ((< j 0) p)
                 (setq p (1+ (* p (- i j))))))
       (setq h (truncate (* h n) d))
       (setq s (+ s h)))
     s))

;;; ----------------------------------------------------------------------------

(let ((table (make-hash-table)))
  (defun fppi ()
    (let ((value (gethash fpprec table)))
      (if value
          value
          (setf (gethash fpprec table) (cdr (fppi1))))))
  
  (defun fppi-table ()
    table)
  
  (defun clear_fppi_table ()
    (clrhash table)))

(defun fppi1 nil
  (bcons
    (fpquotient
      (fprt18231_)
      (list (fpround (comppi (+ fpprec 12))) (+ -12 *m)) )))

(defun fprt18231_ ()
  (let ((a 1823176476672000))
    (setq a (ash a (* 2 fpprec)))
    (destructuring-bind (mantissa exp)
        (intofp (isqrt a))
      (list mantissa (- exp fpprec)))))

(defun comppi (prec)
  (let (s h n d)
     (setq s (ash 13591409 prec))
     (setq h (neg (truncate (ash 67047785160 prec) 262537412640768000)))
     (setq s (+ s h))
     (do ((i 2 (1+ i)))
         ((zerop h))
       (setq n (* 12 
                  (- (* 6 i) 5)
                  (- (* 6 i) 4)
                  (- (* 2 i) 1)
                  (- (* 6 i) 1)
                  (+ (* i 545140134) 13591409)))
       (setq d (* (- (* 3 i) 2)
                  (expt i 3)
                  (- (* i 545140134) 531548725)
                  262537412640768000))
       (setq h (neg (truncate (* h n) d)))
       (setq s (+ s h)))
     s ))

;;; ----------------------------------------------------------------------------

(let ((table (make-hash-table)))
  (defun fpgamma ()
    (let ((value (gethash fpprec table)))
      (if value
          value
          (setf (gethash fpprec table) (cdr (fpgamma1))))))
  
  (defun fpgamma-table ()
    table)
  
  (defun clear_fpgamma_table ()
    (clrhash table)))

(defun fpgamma1 ()
  (bcons (list (fpround (first (comp-bf%gamma (+ fpprec 8)))) 0)))

(defun comp-bf%gamma (prec)
  (let* ((fpprec prec)
         (big-n (floor (* 1/4 prec (log 2.0))))
         (big-n-sq (intofp (* big-n big-n)))
         (beta 3.591121476668622136649223)
         (limit (floor (* beta big-n)))
         (one (fpone))
         (term (intofp 1))
         (harmonic (intofp 0))
         (a-sum (intofp 0))
         (b-sum (intofp 1)))
    (do ((n 1 (1+ n)))
        ((> n limit))
      (let ((bf-n (intofp n)))
        (setf term (fpquotient (fptimes* term big-n-sq)
                               (fptimes* bf-n bf-n)))
        (setf harmonic (fpplus harmonic (fpquotient one bf-n)))
        (setf a-sum (fpplus a-sum (fptimes* term harmonic)))
        (setf b-sum (fpplus b-sum term))))
    (fpplus (fpquotient a-sum b-sum)
            (fpminus (fplog (intofp big-n))))))

;;; ----------------------------------------------------------------------------

(let ((table (make-hash-table)))
  (defun fplog2 ()
    (let ((value (gethash fpprec table)))
      (if value
	  value
	  (setf (gethash fpprec table) (comp-log2)))))
  (defun fplog2-table ()
    table)
  (defun clear_fplog2_table ()
    (clrhash table)))

(defun comp-log2 ()
  (flet ((fast-atanh (k)
           (let* ((term (fpquotient (intofp 1) (intofp k)))
                  (fact (fptimes* term term))
                  (oldsum (intofp 0))
                  (sum term))
             (loop for m from 3 by 2
                   until (equal oldsum sum)
                   do
                   (setf oldsum sum)
                   (setf term (fptimes* term fact))
                   (setf sum (fpplus sum (fpquotient term (intofp m)))))
             sum)))
    (let ((result
            (let* ((fpprec (+ fpprec 8)))
              (fpplus (fpdifference (fptimes* (intofp 18) (fast-atanh 26))
                                    (fptimes* (intofp 2) (fast-atanh 4801)))
                      (fptimes* (intofp 8) (fast-atanh 8749))))))
      (list (fpround (car result))
            (+ -8 *m)))))

;;; ----------------------------------------------------------------------------

(defprop %sin sinbigfloat floatprog)

(defun sinbigfloat (x)
  (*fpsin (car x) t))

(defun *fpsin (a fl)
  (fpend (let ((fpprec (+ 8 fpprec)))
           (cond ((bigfloatp a) (fpsin (cdr ($bfloat a)) fl))
                 (fl (list '(%sin) a))
                 (t (list '(%cos) a))))))

(defun fpsin (x fl)
  (prog (piby2 r sign res k *cancelled)
    (setq sign (cond (fl (> (car x) 0))
                     (t))
          x (fpabs x))
    (when (eql (car x) 0)
      (return (if fl (intofp 0) (intofp 1))))
    (return
      (cdr
        (check-bigfloat
          (let ((fpprec (max fpprec (+ fpprec (cadr x))))
                (xt (bcons x))
                (*cancelled 0)
                (oldprec fpprec))
            (prog (x)
            loop
              (setq x (cdr (check-bigfloat xt)))
              (setq piby2 (fpquotient (fppi) (intofp 2)))
              (setq r (fpintpart (fpquotient x piby2)))
              (setq x (fpplus x (fptimes* (intofp (- r)) piby2)))
              (setq k *cancelled)
              (fpplus x (fpminus piby2))
              (setq *cancelled (max k *cancelled))
              (cond ((not (> oldprec (- fpprec *cancelled)))
                     (setq r (rem r 4))
                     (setq res
                           (cond (fl
                                  (cond ((= r 0) (fpsin1 x))
                                        ((= r 1) (fpcos1 x))
                                        ((= r 2) (fpminus (fpsin1 x)))
                                        ((= r 3) (fpminus (fpcos1 x)))))
                                 (t
                                  (cond ((= r 0) (fpcos1 x))
                                        ((= r 1) (fpminus (fpsin1 x)))
                                        ((= r 2) (fpminus (fpcos1 x)))
                                        ((= r 3) (fpsin1 x))))))
                     (return (bcons (if sign res (fpminus res)))))
                    (t
                     (incf fpprec *cancelled)
                     (go loop))))))))))

(defun fpcos1 (x)
  (fpsincos1 x nil))

(defun fpsin1(x)
  (fpsincos1 x t))

(defun fpsincos1 (x fl)
  (prog (ans term oans x2)
     (setq ans (if fl x (intofp 1))
           x2 (fpminus(fptimes* x x)))
     (setq term ans)
     (do ((n (if fl 3 2) (+ n 2)))
         ((equal ans oans))
       (setq term (fptimes* term (fpquotient x2 (intofp (* n (1- n))))))
       (setq oans ans
             ans (fpplus ans term)))
     (return ans)))

;;; ----------------------------------------------------------------------------

(defprop %cos cosbigfloat floatprog)

(defun cosbigfloat (x)
  (*fpsin (car x) nil))

;;; ----------------------------------------------------------------------------

(defprop %tan tanbigfloat floatprog)

(defun tanbigfloat (a)
  (setq a (car a))
  (fpend (let ((fpprec (+ 8 fpprec)))
           (cond ((bigfloatp a)
                  (setq a (cdr ($bfloat a)))
                  (fpquotient (fpsin a t) (fpsin a nil)))
                 (t (list '(%tan) a))))))

;;; ----------------------------------------------------------------------------

(defun big-float-asin (x &optional y)
  (if y
      (multiple-value-bind (u v)
          (complex-asin x y)
        (add u (mul '$%i v)))
      (fpasin x)))

(defun complex-asin (x y)
  (let ((x (cdr (check-bigfloat x)))
        (y (cdr (check-bigfloat y))))
    (multiple-value-bind (re-sqrt-1-z im-sqrt-1-z)
        (complex-sqrt (bcons (fpdifference (intofp 1) x))
                      (bcons (fpminus y)))
      (multiple-value-bind (re-sqrt-1+z im-sqrt-1+z)
          (complex-sqrt (bcons (fpplus (intofp 1) x))
                        (bcons y))
        (values (bcons
                  (let ((d (fpdifference (fptimes* re-sqrt-1-z
                                                   re-sqrt-1+z)
                                         (fptimes* im-sqrt-1-z
                                                   im-sqrt-1+z))))
                    (cond ((equal d '(0 0))
                           (if (fplessp x '(0 0))
                               (fpminus (fpquotient (fppi) (intofp 2)))
                               (fpquotient (fppi) (intofp 2))))
                          (t
                           (fpatan (fpquotient x d))))))
                (fpasinh
                  (bcons (fpdifference (fptimes* re-sqrt-1-z
                                                 im-sqrt-1+z)
                                       (fptimes* im-sqrt-1-z
                                                 re-sqrt-1+z)))))))))

(defun fpasin (x)
  ($bfloat (fpasin-core x)))

(defun fpasin-core (x)
  (let ((fp-x (cdr (check-bigfloat x))))
    (cond ((minusp (car fp-x))
           (mul -1 (fpasin (bcons (fpminus fp-x)))))
          ((fplessp fp-x (cdr bfhalf))
           (bcons
             (fpatan
               (fpquotient fp-x
                           (fproot
                             (bcons (fptimes* (fpdifference (fpone) fp-x)
                                              (fpplus (fpone) fp-x)))
                             2)))))
          ((fpgreaterp fp-x (fpone))
           (let ((arg (fpplus
                        fp-x
                        (fproot
                          (bcons (fptimes* (fpdifference fp-x (fpone))
                                           (fpplus fp-x (fpone))))
                          2))))
             (add (div '$%pi 2)
                  (mul -1 '$%i (bcons (fplog arg))))))
          (t
           (add (div '$%pi 2)
                (mul -1
                     (bcons
                       (fpatan
                         (fpquotient
                           (fproot
                             (bcons
                               (fptimes* (fpdifference (fpone) fp-x)
                                         (fpplus (fpone) fp-x)))
                             2)
                           fp-x)))))))))

;;; ----------------------------------------------------------------------------

(defun big-float-acos (x &optional y)
  (if y
      (multiple-value-bind (u v)
          (complex-acos x y)
        (add u (mul '$%i v)))
      (fpacos x)))

(defun complex-acos (x y)
  (let ((x (cdr (check-bigfloat x)))
        (y (cdr (check-bigfloat y))))
    (multiple-value-bind (re-sqrt-1-z im-sqrt-1-z)
        (complex-sqrt (bcons (fpdifference (intofp 1) x))
                      (bcons (fpminus y)))
      (multiple-value-bind (re-sqrt-1+z im-sqrt-1+z)
          (complex-sqrt (bcons (fpplus (intofp 1) x))
                        (bcons y))
        (values (bcons
                  (fptimes* (intofp 2)
                            (fpatan (fpquotient re-sqrt-1-z re-sqrt-1+z))))
                (fpasinh (bcons
                           (fpdifference
                             (fptimes* re-sqrt-1+z im-sqrt-1-z)
                             (fptimes* im-sqrt-1+z re-sqrt-1-z)))))))))

(defun fpacos (x)
  ($bfloat (add (div '$%pi 2) (mul -1 (fpasin-core x)))))

;;; ----------------------------------------------------------------------------

(defprop %atan atanbigfloat floatprog)

(defun atanbigfloat (x)
  (*fpatan (car x) (cdr x)))

(defun *fpatan (a y)
  (fpend (let ((fpprec (+ 8. fpprec)))
           (if (null y)
               (if (bigfloatp a) (fpatan (cdr ($bfloat a)))
                   (list '(%atan) a))
               (fpatan2 (cdr ($bfloat a)) (cdr ($bfloat (car y))))))))

(defun fpatan (x)
  (prog (term x2 ans oans one two tmp)
    (setq one (intofp 1) two (intofp 2))
    (cond ((fpgreaterp (fpabs x) one)
           (setq tmp (fpquotient (fppi) two))
           (setq ans (fpdifference tmp (fpatan (fpquotient one x))))
           (return (cond ((fplessp x (intofp 0))
                          (fpdifference ans (fppi)))
                         (t ans))))
          ((fpgreaterp (fpabs x) (fpquotient one two))
           (setq tmp (fpquotient x (fpplus (fptimes* x x) one)))
           (setq x2 (fptimes* x tmp) term (setq ans one))
           (do ((n 0 (1+ n)))
               ((equal ans oans))
             (setq term
                   (fptimes* term (fptimes* x2 (fpquotient
                                                 (intofp (+ 2 (* 2 n)))
                                                 (intofp (+ (* 2 n) 3))))))
             (setq oans ans ans (fpplus term ans)))
           (setq ans (fptimes* tmp ans)))
          (t
           (setq ans x x2 (fpminus (fptimes* x x)) term x)
           (do ((n 3 (+ n 2)))
               ((equal ans oans))
             (setq term (fptimes* term x2))
             (setq oans ans
                   ans (fpplus ans (fpquotient term (intofp n)))))))
    (return ans)))

(defun fpatan2 (y x)
  (cond ((eql (car x) 0)
         (cond ((equal (car y) 0)
                (merror "atan2: atan2(0, 0) is undefined."))
               ((minusp (car y))
                (fpquotient (fppi) (intofp -2)))
               (t
                (fpquotient (fppi) (intofp 2)))))
        ((> (car x) 0)
         (fpatan (fpquotient y x)))
        ((> (car y) 0)
         (fpplus (fppi) (fpatan (fpquotient y x))))
        (t
         (fpdifference (fpatan (fpquotient y x)) (fppi)))))

;;; ----------------------------------------------------------------------------

(defun big-float-sinh (x &optional y)
  (unless y
    (fpsinh x)))

(defun fpsinh (x)
  (cond ((eql 0 (cadr x))
         (check-bigfloat x))
        ((fpposp (cdr x))
         (let ((d (fpexpm1 (cdr (check-bigfloat x)))))
           (bcons (fpquotient (fpplus d (fpquotient d (fpplus d (fpone))))
                              (intofp 2)))))
        (t
         (bcons 
           (fpminus
             (cdr (fpsinh (bcons (fpminus (cdr (check-bigfloat x)))))))))))

(defun fpexpm1 (x)
  (cond ((fpgreaterp (fpabs x) (fpone))
         (fpdifference (fpexp x) (fpone)))
        (t
         (let ((ans x)
               (oans nil)
               (term x))
           (do ((n 2 (1+ n)))
               ((equal ans oans))
             (setf term (fpquotient (fptimes* x term) (intofp n)))
             (setf oans ans)
             (setf ans (fpplus ans term)))
           ans))))

;;; ----------------------------------------------------------------------------

(defun big-float-tanh (x &optional y)
  (if y
      (multiple-value-bind (u v)
          (complex-tanh x y)
        (add u (mul '$%i v)))
      (fptanh x)))

(defun complex-tanh (x y)
  (let* ((tv (cdr (tanbigfloat (list y))))
	 (beta (fpplus (fpone) (fptimes* tv tv)))
	 (s (cdr (fpsinh x)))
	 (s^2 (fptimes* s s))
	 (rho (fproot (bcons (fpplus (fpone) s^2)) 2))
	 (den (fpplus (fpone) (fptimes* beta s^2))))
    (values (bcons (fpquotient (fptimes* beta (fptimes* rho s)) den))
	    (bcons (fpquotient tv den)))))

(defun fptanh (x)
  (let* ((two (intofp 2))
         (fp (cdr (check-bigfloat x)))
         (d (fpexpm1 (fptimes* fp two))))
    (bcons (fpquotient d (fpplus d two)))))

;;; ----------------------------------------------------------------------------

(defun big-float-asinh (x &optional y)
  (if y
      (multiple-value-bind (u v)
          (complex-asinh x y)
        (add u (mul '$%i v)))
      (fpasinh x)))

(defun complex-asinh (x y)
  (multiple-value-bind (u v)
      (complex-asin (mul -1 y) x)
    (values v (bcons (fpminus (cdr u))))))

(defun fpasinh (x)
  (let* ((fp-x (cdr (check-bigfloat x)))
         (absx (fpabs fp-x))
         (one (fpone))
         (two (intofp 2))
         (minus (minusp (car fp-x)))
         result)
    (cond ((fpgreaterp absx two)
           (setf result
                 (fplog
                   (fpplus
                     (fptimes* absx two)
                     (fpquotient
                       one
                       (fpplus absx
                               (fproot
                                 (bcons (fpplus one
                                                (fptimes* absx absx)))
                                 2)))))))
          (t
           (let ((x*x (fptimes* absx absx)))
             (setq result
                   (fplog1p
                     (fpplus
                       absx
                       (fpquotient x*x
                                   (fpplus one
                                           (fproot
                                             (bcons (fpplus one x*x))
                                             2)))))))))
    (if minus
        (bcons (fpminus result))
        (bcons result))))

;;; ----------------------------------------------------------------------------

(defun big-float-atanh (x &optional y)
  (if y
      (multiple-value-bind (u v)
          (complex-atanh x y)
        (add u (mul '$%i v)))
      (fpatanh x)))

(defun complex-atanh (x y)
  (let* ((fpx (cdr (check-bigfloat x)))
         (fpy (cdr (check-bigfloat y)))
         (beta (if (minusp (car fpx))
                   (fpminus (fpone))
                   (fpone)))
         (x-lt-minus-1 (fplessp (fpplus fpx (fpone)) '(0 0)))
         (x-gt-plus-1 (fpgreaterp fpy (fpone)))
         (y-equals-0 (equal y '((bigfloat) 0 0)))
         (x (fptimes* beta fpx))
         (y (fptimes* beta (fpminus fpy)))
         (rho (intofp 0))
         (t1 (fpplus (fpabs y) rho))
         (t1^2 (fptimes* t1 t1))
         (1-x (fpdifference (fpone) x))
         (eta (fpquotient
                (fplog1p (fpquotient (fptimes* (intofp 4) x)
                                     (fpplus (fptimes* 1-x 1-x) t1^2)))
                (intofp 4)))
         (nu (if y-equals-0
                 (fpminus
                   (if x-lt-minus-1
                       (cdr ($bfloat '((mquotient) $%pi 2)))
                       (if x-gt-plus-1
                           (cdr ($bfloat '((mminus) ((mquotient) $%pi 2))))
                           (merror "COMPLEX-ATANH: HOW DID I GET HERE?"))))
                 (fptimes*
                   (cdr bfhalf)
                   (fpatan2 (fptimes* (intofp 2) y)
                            (fpdifference (fptimes* 1-x (fpplus (fpone) x))
                                          t1^2))))))
    (values (bcons (fptimes* beta eta))
            (bcons (fpminus (fptimes* beta nu))))))

(defun fpatanh (x)
  (let* ((fp-x (cdr (check-bigfloat x))))
    (cond ((fplessp fp-x (intofp 0))
           (mul -1 (fpatanh (bcons (fpminus fp-x)))))
          ((fpgreaterp fp-x (fpone))
           (multiple-value-bind (u v)
               (complex-atanh x (bcons (intofp 0)))
             (add u (mul '$%i v))))
          ((fpgreaterp fp-x (cdr bfhalf))
           (bcons
             (fptimes* (cdr bfhalf)
                       (fplog1p (fpquotient (fptimes* (intofp 2) fp-x)
                                            (fpdifference (fpone) fp-x))))))
          (t
           (let ((2x (fptimes* (intofp 2) fp-x)))
             (bcons
               (fptimes*
                 (cdr bfhalf)
                 (fplog1p
                   (fpplus 2x
                           (fpquotient (fptimes* 2x fp-x)
                                       (fpdifference (fpone) fp-x)))))))))))

;;; ----------------------------------------------------------------------------

(eval-when (:load-toplevel :execute)
    (fpprec1 nil $fpprec))

;;; ----------------------------------------------------------------------------
