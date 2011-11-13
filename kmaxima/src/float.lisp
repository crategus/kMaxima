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
               (integer-length most-positive-fixnum))
  (defconstant +machine-mantissa-precision+ (float-digits 1.0)))

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

(defmvar $ratprint t)
(defmvar $keepfloat nil)

(defmvar $ratepsilon 2.0d-15)

;;; ----------------------------------------------------------------------------

(defvar fpprec)
(defvar *m)
(defvar *cancelled)
(defvar *decfp* nil)

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

(defun hipart (x n)
  (if (bignump n)
      (abs x)
      (haipart x n)))

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
  (if (or (not (fixnump q))
          (< q 1))
      (merror "fpprec: value must be a positive integer; found: ~M" q)
      (progn
        (setq fpprec (+ 2 (integer-length (expt 10 q)))
              bigfloatone ($bfloat 1)
              bigfloatzero ($bfloat 0)
              bfhalf (list (car bigfloatone) (cadr bigfloatone) 0)
              bfmhalf (list (car bigfloatone) (- (cadr bigfloatone)) 0))
        q)))

;;; ----------------------------------------------------------------------------

(defun bcons (x)
  `((bigfloat simp ,fpprec) . ,x))

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

(defun intofp (x)
  (cond ((floatp x) (float2fp x))
        ((eql x 0) '(0 0))
        ((eq x '$%pi) (fppi))
        ((eq x '$%e) (fpe))
        ((eq x '$%gamma) (fpgamma))
        ((eq x '$%phi)
         (cdr ($bfloat '((mtimes simp)
                         ((rat simp) 1 2)
                         ((mplus simp) 1
                          ((mexpt simp) 5 ((rat simp) 1 2)))))))
        (t (list (fpround x) (+ *m fpprec)))))

(defun fpround (x &aux (*print-base* 10) *print-radix*)
  (prog (adjust)
     (cond
       ((null *decfp*)
        (setq *m (- (integer-length x) fpprec))
        (cond ((= *m 0)
               (setq *cancelled 0)
               (return x))
              (t
               (setq adjust (fpshift 1 (1- *m)))
               (when (minusp x) (setq adjust (- adjust)))
               (incf x adjust)
               (setq *m (- (integer-length x) fpprec))
               (setq *cancelled (abs *m))
               (cond ((zerop (hipart x (- *m)))
                      (return (fpshift (fpshift x (- -1 *m)) 1)))
                     (t (return (fpshift x (- *m))))))))
       (t
        (setq *m (- (length (exploden (abs x))) fpprec))
        (setq adjust (fpshift 1 (1- *m)))
        (when (minusp x) (setq adjust (- adjust)))
        (setq adjust (* 5 adjust))
        (setq *m (- (length (exploden (abs (setq x (+ x adjust))))) fpprec))
        (return (fpshift x (- *m)))))))

(defun fpshift (x n)
  (cond ((null *decfp*)
         (cond ((and (minusp n) (minusp x))
                (- (ash (- x) n)))
               (t (ash x n))))
        ((> n 0)
         (* x (expt 10 n)))
        ((< n 0)
         (truncate x (expt 10 (- n))))
        (t x)))

(defun fpintpart (x)
  (let ((m (- fpprec (cadr x))))
     (if (> m 0)
         (truncate (car x) (expt 2 m))
         (* (car x) (expt 2 (- m))))))

(defun fpend (x)
  (cond ((eql (car x) 0) (bcons x))
        ((numberp (car x))
         (bcons (list (fpround (car x)) (+ -8 *m (cadr x)))))
        (t x)))

(defun fparcsimp (x)
  (if (and (mplusp x)
           (null (cdddr x))
           (mtimesp (caddr x))
           (null (cdddr (caddr x)))
           (bigfloatp (cadr (caddr x)))
           (eq (caddr (caddr x)) '$%i)
           (< (caddr (cadr (caddr x))) (+ (- fpprec) 2)))
      (cadr x)
      x))

;;; ----------------------------------------------------------------------------

(defun bigfloat2rat (x)
  (declare (special $ratprint))
  (setq x (check-bigfloat x))
  (let (($float2bf t)
        (rat nil)
        (y nil)
        (sign nil))
    (setq rat (cond ((minusp (cadr x))
                     (setq sign t
                           y (fpration1 (cons (car x) (fpabs (cdr x)))))
                     (rplaca y (* -1 (car y))))
                    (t (fpration1 x))))
    (when $ratprint
      (princ "`rat' replaced ")
      (when sign (princ "-"))
      (princ (coerce (fpformat (cons (car x) (fpabs (cdr x)))) 'string))
      (princ " by ")
      (princ (car rat))
      (write-char #\/)
      (princ (cdr rat))
      (princ " = ")
      (setq x ($bfloat (list '(rat simp) (car rat) (cdr rat))))
      (when sign (princ "-"))
      (princ (coerce (fpformat (cons (car x) (fpabs (cdr x)))) 'string))
      (terpri))
    rat))

(defun fpration1 (x)
  (let ((fprateps (cdr ($bfloat (if $bftorat
                                    (list '(rat simp) 1
                                          (exptrl 2 (1- fpprec)))
                                    $ratepsilon)))))
    (or (and (equal x bigfloatzero)
             (cons 0 1))
        (prog (y a)
          (return
            (do ((xx x (setq y
                             (invertbigfloat
                               (bcons (fpsub (cdr xx)
                                             (cdr ($bfloat a)))))))
                 (num (setq a (fpentier x))
                      (+ (* (setq a (fpentier y)) num) onum))
                 (den 1 (+ (* a den) oden))
                 (onum 1 num)
                 (oden 0 den))
                ((and (not (zerop den))
                      (not (fpgreaterp
                             (fpabs (fpdiv (fpsub (cdr x)
                                                  (fpdiv (cdr ($bfloat num))
                                                         (cdr ($bfloat den))))
                                 (cdr x)))
                             fprateps)))
                 (cons num den))))))))

(defun float-nan-p (x)
  (and (floatp x)
       (not (= x x))))

(defun float-inf-p (x)
  (labels ((extreme-float-values (x)
             (case (type-of x)
               (short-float
                 (values most-negative-short-float
                         most-positive-short-float))
               (single-float
                 (values most-negative-single-float
                         most-positive-single-float))
               (double-float
                 (values most-negative-double-float
                         most-positive-double-float))
               (long-float
                 (values most-negative-long-float
                         most-positive-long-float))))
           (beyond-extreme-values (x)
             (multiple-value-bind (most-negative most-positive)
                 (extreme-float-values x)
               (cond ((< x 0) (< x most-negative))
                     ((> x 0) (> x most-positive))
                     (t nil)))))
    (and (floatp x)
         (not (float-nan-p x))
         (beyond-extreme-values x))))

(defun float2fp (x)
  (when (float-nan-p x)
    (merror
      "bfloat: attempted conversion of floating point NaN (not-a-number).~%"))
  (when (float-inf-p x)
    (merror "bfloat: attempted conversion of floating-point infinity.~%"))
  (unless $float2bf
    (format t "bfloat: converting float ~S to bigfloat.~%" x))
  (if (zerop x)
      (list 0 0)
      (multiple-value-bind (frac exp sign)
          (integer-decode-float x)
        (let ((scale (- fpprec (integer-length frac))))
          (list (ash (* sign frac) scale)
                (+ fpprec (- exp scale)))))))

(defun fp2float (x)
  (let ((precision (caddar x))
        (mantissa (cadr x))
        (exponent (caddr x))
        (fpprec +machine-mantissa-precision+)
        (*m 0))
    (setq mantissa
          (/ (fpround mantissa) (expt 2.0 +machine-mantissa-precision+)))
    (let ((e (+ exponent (- precision) *m +machine-mantissa-precision+)))
      (if (>= e 1025)
          (merror "float: floating point overflow converting ~:M" x)
          (scale-float mantissa e)))))

(defun rat2fp (x)
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

(defun $bfloat (x)
  (declare (special $ratprint))
  (let (y)
    (cond ((check-bigfloat x))
          ((or (numberp x)
               (member x '($%e $%pi $%gamma $%phi) :test #'eq))
           (bcons (intofp x)))
          ((or (atom x)
               (member 'array (cdar x) :test #'eq))
           (if (eq x '$%phi)
               ($bfloat '((mtimes simp)
                          ((rat simp) 1 2)
                          ((mplus simp) 1 ((mexpt simp) 5 ((rat simp) 1 2)))))
               x))
          ((eq (caar x) 'rat)
           (bcons (rat2fp (cdr x))))
          ((eq (caar x) 'mexpt)
           (if (eq (cadr x) '$%e)
               (fpexp* ($bfloat (caddr x)))
               (exptbigfloat ($bfloat (cadr x)) (caddr x))))
          ((eq (caar x) 'mncexpt)
           (list '(mncexpt) ($bfloat (cadr x)) (caddr x)))
          ((setq y (getprop (caar x) 'floatprog))
           (funcall y (mapcar #'$bfloat (cdr x))))
          (t (recur-apply #'$bfloat x)))))

;;; ----------------------------------------------------------------------------

(defprop mplus addbigfloat floatprog)

(defun addbigfloat (args)
  (let ((fans (intofp 0))
        nfans)
    (do ((l args (cdr l)))
        ((null l)
         (cond ((null nfans) (bcons fans))
               ((zerop (car fans)) (addn nfans nil))
               (t (addn (cons (bcons fans) nfans) nil))))
      (cond ((bigfloatp (car l))
             (setq fans (fpadd (cdr (check-bigfloat (car l))) fans)))
            (t
             (setq nfans (cons (car l) nfans)))))))
    
(defun fpadd (a b)
  (prog (*m expo man sticky)
    (setq *cancelled 0)
    (cond ((eql (car a) 0) (return b))
          ((eql (car b) 0) (return a)))
    (setq expo (- (cadr a) (cadr b)))
    (setq man (cond ((eql expo 0)
                     (setq sticky 0)
                     (fpshift (+ (car a) (car b)) 2))
                    ((> expo 0)
                     (setq sticky (hipart (car b) (- 1 expo)))
                     (setq sticky (cond ((zerop sticky) 0)
                                        ((< (car b) 0) -1)
                                        (t 1)))
                     (+ (fpshift (car a) 2)
                        (fpshift (car b) (- 2 expo))))
                    (t
                     (setq sticky (hipart (car a) (1+ expo)))
                     (setq sticky (cond ((zerop sticky) 0)
                                        ((< (car a) 0) -1)
                                        (t 1)))
                     (+ (fpshift (car b) 2)
                        (fpshift (car a) (+ 2 expo))))))
    (setq man (+ man sticky))
    (return (cond ((eql man 0) '(0 0))
                  (t
                   (setq man (fpround man))
                   (setq expo (+ -2 *m (max (cadr a) (cadr b))))
                   (list man expo))))))

;;; ----------------------------------------------------------------------------

(defprop mtimes timesbigfloat floatprog)

(defun timesbigfloat (args)
  (let ((fans (fpone))
        nfans)
    (do ((l args (cdr l)))
        ((null l)
         (if (null nfans)
             (bcons fans)
             (muln (cons (bcons fans) nfans) nil)))
      (if (bigfloatp (car l))
          (setq fans (fpmul (cdr (check-bigfloat (car l))) fans))
          (setq nfans (cons (car l) nfans))))))

(defun fpmul (a b)
  (if (or (zerop (car a)) (zerop (car b)))
      (intofp 0)
      (list (fpround (* (car a) (car b)))
            (+ *m (cadr a) (cadr b) (- fpprec)))))

;;; ----------------------------------------------------------------------------

(defun fpexp* (arg)
  (fpend (let ((fpprec (+ 8 fpprec)))
           (if (bigfloatp arg)
               (fpexp (cdr (check-bigfloat arg)))
               (list '(mexpt) '$%e arg)))))

(defun fpexp (x)
  (if (< (car x) 0)
      (fpdiv (fpone) (fpexp (fpminus x)))
      (let ((n (fpintpart x)))
        (cond ((< n 2)
               (fpexp1 x))
              (t
               (fpmul (fpexp1 (fpsub x (intofp n)))
                      (cdr (check-bigfloat
                             (let ((fpprec (+ fpprec (integer-length n) -1))
                                   (n n))
                               (bcons (fpexpt (fpe) n)))))))))))

(defun fpexp1 (x)
  (do ((n 1 (1+ n))
       (ans (fpone))
       (term (fpone))
       oans)
      ((equal ans oans) ans)
    (setq term (fpdiv (fpmul x term) (intofp n)))
    (setq oans ans)
    (setq ans (fpadd ans term))))

;;; ----------------------------------------------------------------------------

(defun exptbigfloat (p n)
  (declare (special $numer $float $keepfloat $ratprint))
  (cond ((eql n 1) p)
        ((eql n 0) ($bfloat 1))
        ((not (bigfloatp p)) (list '(mexpt) p n))
        ((eql (cadr p) 0) ($bfloat 0))
        ((and (< (cadr p) 0)
              (ratnump n))
         (mul (let ($numer $float $keepfloat $ratprint)
                 (power -1 n))
              (exptbigfloat (bcons (fpminus (cdr p))) n)))
        ((and (< (cadr p) 0)
              (not (integerp n)))
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
                       (add ($bfloat `((mtimes) ,p ,(fpsin* n nil)))
                            `((mtimes simp)
                              ,($bfloat `((mtimes) ,p ,(fpsin* n t)))
                              $%i)))))
               (t (list '(mexpt) p n))))
        ((and (ratnump n)
              (< (caddr n) 10))
         (bcons (fpexpt (fproot p (caddr n)) (cadr n))))
        ((not (integerp n))
         (setq n ($bfloat n))
         (cond ((not (bigfloatp n)) (list '(mexpt) p n))
               (t
                (let ((extrabits (max 1 (+ (caddr n)
                                           (integer-length (caddr p))))))
                  (setq p
                        (let ((fpprec (+ extrabits fpprec)))
                          (fpexp (fpmul (cdr (check-bigfloat n))
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
        ((< nn 0) (fpdiv (fpone) (fpexpt p (- nn))))
        (t
         (prog (u)
           (if (oddp nn)
               (setq u p)
               (setq u (fpone)))
           (do ((ii (truncate nn 2) (truncate ii 2)))
               ((zerop ii))
             (setq p (fpmul p p))
             (when (oddp ii) (setq u (fpmul u p))))
           (return u)))))

;;; ----------------------------------------------------------------------------

(defun fpsub (a b)
  (fpadd a (fpminus b)))

;;; ----------------------------------------------------------------------------

(defun fpminus (x)
  (if (eql (car x) 0)
      x
      (list (- (car x)) (cadr x))))

;;; ----------------------------------------------------------------------------

(defun invertbigfloat (a)
  (if (bigfloatp a)
      (bcons (fpdiv (fpone) (cdr (check-bigfloat a))))
      (simplifya (list '(mexpt) a -1) nil)))

(defun fpdiv (a b)
  (cond ((eql (car b) 0)
         (merror "pquotient: attempted quotient by zero."))
        ((eql (car a) 0) (intofp 0))
        (t
         (list (fpround (truncate (fpshift (car a) (+ 3 fpprec)) (car b)))
               (+ -3 (- (cadr a) (cadr b)) *m)))))

;;; ----------------------------------------------------------------------------

(defun fpgreaterp (a b)
  (fpposp (fpsub a b)))

(defun fplessp (a b)
  (fpposp (fpsub b a)))

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

(defun fpone ()
  (cond (*decfp* (intofp 1))
        ((= fpprec (caddar bigfloatone)) (cdr bigfloatone))
        (t (intofp 1))))

;;; ----------------------------------------------------------------------------

(let ((table (make-hash-table)))
  (defun fpe ()
    (labels ((compe (prec)
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
             (fpe1 ()
               (bcons (list (fpround (compe (+ fpprec 12))) (+ -12 *m)))))
      (let ((value (gethash fpprec table)))
        (if value
            value
            (setf (gethash fpprec table) (cdr (fpe1))))))))

;;; ----------------------------------------------------------------------------

(let ((table (make-hash-table)))
  (defun fppi ()
    (labels ((fprt18231 ()
               (let ((a 1823176476672000))
                 (setq a (ash a (* 2 fpprec)))
                 (destructuring-bind (mantissa expo)
                     (intofp (isqrt a))
                   (list mantissa (- expo fpprec)))))
             (comppi (prec)
               (let (s h n d)
                 (setq s (ash 13591409 prec))
                 (setq h (neg (truncate (ash 67047785160 prec)
                                        262537412640768000)))
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
                 s))
             (fppi1 ()
               (bcons (fpdiv (fprt18231)
                             (list (fpround (comppi (+ fpprec 12)))
                                   (+ -12 *m))))))
      (let ((value (gethash fpprec table)))
        (if value
            value
            (setf (gethash fpprec table) (cdr (fppi1))))))))

;;; ----------------------------------------------------------------------------

(let ((table (make-hash-table)))
  (defun fpgamma ()
    (labels ((compgamma (prec)
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
                     (setf term (fpdiv (fpmul term big-n-sq)
                                       (fpmul bf-n bf-n)))
                     (setf harmonic (fpadd harmonic (fpdiv one bf-n)))
                     (setf a-sum (fpadd a-sum (fpmul term harmonic)))
                     (setf b-sum (fpadd b-sum term))))
                 (fpadd (fpdiv a-sum b-sum)
                        (fpminus (fplog (intofp big-n))))))
             (fpgamma1 ()
               (bcons (list (fpround (first (compgamma (+ fpprec 8)))) 0))))
      (let ((value (gethash fpprec table)))
        (if value
            value
            (setf (gethash fpprec table) (cdr (fpgamma1))))))))

;;; ----------------------------------------------------------------------------

(let ((table (make-hash-table)))
  (defun fplog2 ()
    (labels ((fast-atanh (k)
               (let* ((term (fpdiv (intofp 1) (intofp k)))
                      (fact (fpmul term term))
                      (oldsum (intofp 0))
                      (sum term))
                 (loop for m from 3 by 2
                       until (equal oldsum sum)
                       do
                       (setf oldsum sum)
                       (setf term (fpmul term fact))
                       (setf sum (fpadd sum (fpdiv term (intofp m)))))
                 sum))
             (comp-log2 ()
               (let ((result
                       (let ((fpprec (+ fpprec 8)))
                         (fpadd (fpsub (fpmul (intofp 18) (fast-atanh 26))
                                       (fpmul (intofp 2) (fast-atanh 4801)))
                                (fpmul (intofp 8) (fast-atanh 8749))))))
                 (list (fpround (car result))
                       (+ -8 *m)))))
      (let ((value (gethash fpprec table)))
        (if value
	    value
	    (setf (gethash fpprec table) (comp-log2)))))))

;;; ----------------------------------------------------------------------------

(defun fpentier (x)
  (let ((fpprec (caddar x)))
    (fpintpart (cdr x))))

;;; ----------------------------------------------------------------------------

(defprop mabs mabsbigfloat floatprog)

(defun mabsbigfloat (arg)
  (if (bigfloatp (setq arg (car arg)))
      (bcons (fpabs (cdr (check-bigfloat arg))))
      (list '(mabs) arg)))

(defun fpabs (x)
  (if (>= (car x) 0)
      x
      (cons (- (car x)) (cdr x))))

;;; ----------------------------------------------------------------------------

(defun big-float-sqrt (x &optional y)
  (if y
      (multiple-value-bind (u v)
          (complex-sqrt x y)
        (add (bcons u) (mul '$%i (bcons v))))
      (let ((fp-x (cdr (check-bigfloat x))))
        (if (fplessp fp-x (intofp 0))
            (mul '$%i (bcons (fproot (bcons (fpminus fp-x)) 2)))
            (bcons (fproot x 2))))))

(defun complex-sqrt (xx yy)
  (let* ((x (cdr (check-bigfloat xx)))
         (y (cdr (check-bigfloat yy)))
         (rho (fpadd (fpmul x x)
                      (fpmul y y))))
    (setf rho (fpadd (fpabs x) (fproot (bcons rho) 2)))
    (setf rho (fpadd rho rho))
    (setf rho (fpdiv (fproot (bcons rho) 2) (intofp 2)))
    (let ((eta rho)
          (nu y))
      (when (fpgreaterp rho (intofp 0))
        (setf nu (fpdiv (fpdiv nu rho) (intofp 2)))
        (when (fplessp x (intofp 0))
          (setf eta (fpabs nu))
          (setf nu (if (minusp (car y))
                       (fpminus rho)
                       rho))))
      (values eta nu))))

(defun fproot (a n)
  (if (eq (cadr a) 0)
      (intofp 0)
      (progn
        (let* ((ofprec fpprec)
               (fpprec (+ fpprec 2))
               (bk (fpexpt
                     (intofp 2)
                     (1+ (truncate
                           (cadr (setq a (cdr (check-bigfloat a)))) n)))))
          (do ((x bk (fpsub
                       x
                       (setq bk
                             (fpdiv
                               (fpsub x (fpdiv a (fpexpt x n1)))
                               n))))
               (n1 (1- n))
               (n (intofp n)))
              ((or (equal bk '(0 0))
                   (> (- (cadr x) (cadr bk)) ofprec))
               (setq a x))))
        (list (fpround (car a)) (+ -2 *m (cadr a))))))

;;; ----------------------------------------------------------------------------

(defun big-float-log (x &optional y)
  (if y
      (multiple-value-bind (u v)
          (complex-log x y)
        (add (bcons u) (mul '$%i (bcons v))))
      (flet ((%log (x)
               (cdr
                 (let* ((extra 8)
                        (fpprec (+ fpprec extra))
                        (log-frac
                          (fplog (list (ash (car x) extra) 0)))
                        (log-exp (fpmul (intofp (second x)) (fplog2)))
                        (result (bcons (fpadd log-frac log-exp))))
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
         (t1 (let (($float2bf t)) (float2fp 1.2)))
         (t2 (intofp 3))
         (rho (fpadd (fpmul x x) (fpmul y y)))
         (abs-x (fpabs x))
         (abs-y (fpabs y))
         (beta (fpmax abs-x abs-y))
         (theta (fpmin abs-x abs-y)))
    (values (if (or (fpgreaterp t1 beta)
                    (fplessp rho t2))
                (fpdiv
                  (fplog1p (fpadd (fpmul (fpsub beta (fpone))
                                             (fpadd beta (fpone)))
                                   (fpmul theta theta)))
                 (intofp 2))
                (fpdiv (fplog rho) (intofp 2)))
            (fpatan2 y x))))

(defprop %log logbigfloat floatprog)

(defun logbigfloat (a)
  (cond ((bigfloatp (car a))
         (big-float-log ($bfloat (car a))))
        (t
         (list '(%log) (car a)))))

(defun fplog (x)
  (prog (over two ans oldans term e sum)
    (unless (> (car x) 0)
      (merror "fplog: argument must be positive; found: ~M" (car x)))
    (setq e (fpe)
          over (fpdiv (fpone) e)
          ans 0)
    (do ()
        (nil)
      (cond ((equal x e) (setq x nil) (return nil))
            ((and (fplessp x e) (fplessp over x))
             (return nil))
            ((fplessp x over)
             (setq x (fpmul x e))
             (decf ans))
            (t
             (incf ans)
             (setq x (fpdiv x e)))))
    (when (null x) (return (intofp (1+ ans))))
    (setq x (fpsub  x (fpone))
          ans (intofp ans))
    (setq x
          (fpexpt (setq term (fpdiv x (fpadd x (setq two (intofp 2)))))
                  2))
    (setq sum (intofp 0))
    (do ((n 1 (+ n 2)))
        ((equal sum oldans))
      (setq oldans sum)
      (setq sum (fpadd sum (fpdiv term (intofp n))))
      (setq term (fpmul term x)))
    (return (fpadd ans (fpmul two sum)))))

(defun fplog1p (x)
  (cond ((fpgreaterp (fpabs x) (fpone))
         (fplog (fpadd x (fpone))))
        (t
         (let* ((sum (intofp 0))
                (term (fpdiv x (fpadd x (intofp 2))))
                (f (fpmul term term))
                (oldans nil))
           (do ((n 1 (+ n 2)))
               ((equal sum oldans))
             (setq oldans sum)
             (setq sum (fpadd sum (fpdiv term (intofp n))))
             (setq term (fpmul term f)))
           (fpmul sum (intofp 2))))))

;;; ----------------------------------------------------------------------------

(defprop %sin sinbigfloat floatprog)

(defun sinbigfloat (x)
  (fpsin* (car x) t))

(defun fpsin* (a fl)
  (fpend (let ((fpprec (+ 8 fpprec)))
           (cond ((bigfloatp a) (fpsin (cdr (check-bigfloat a)) fl))
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
              (setq piby2 (fpdiv (fppi) (intofp 2)))
              (setq r (fpintpart (fpdiv x piby2)))
              (setq x (fpadd x (fpmul (intofp (- r)) piby2)))
              (setq k *cancelled)
              (fpadd x (fpminus piby2))
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
           x2 (fpminus(fpmul x x)))
     (setq term ans)
     (do ((n (if fl 3 2) (+ n 2)))
         ((equal ans oans))
       (setq term (fpmul term (fpdiv x2 (intofp (* n (1- n))))))
       (setq oans ans
             ans (fpadd ans term)))
     (return ans)))

;;; ----------------------------------------------------------------------------

(defprop %cos cosbigfloat floatprog)

(defun cosbigfloat (x)
  (fpsin* (car x) nil))

;;; ----------------------------------------------------------------------------

(defprop %tan tanbigfloat floatprog)

(defun tanbigfloat (a)
  (fpend (let ((fpprec (+ 8 fpprec)))
           (cond ((bigfloatp (setq a (car a)))
                  (setq a (cdr (check-bigfloat a)))
                  (fpdiv (fpsin a t) (fpsin a nil)))
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
        (complex-sqrt (bcons (fpsub (intofp 1) x))
                      (bcons (fpminus y)))
      (multiple-value-bind (re-sqrt-1+z im-sqrt-1+z)
          (complex-sqrt (bcons (fpadd (intofp 1) x))
                        (bcons y))
        (values (bcons (let ((d (fpsub (fpmul re-sqrt-1-z re-sqrt-1+z)
                                       (fpmul im-sqrt-1-z im-sqrt-1+z))))
                         (cond ((eql (car d) 0)
                                (if (fplessp x '(0 0))
                                    (fpminus (fpdiv (fppi) (intofp 2))
                                             (fpdiv (fppi) (intofp 2)))))
                               (t (fpatan (fpdiv x d))))))
                (fpasin (bcons (fpsub (fpmul re-sqrt-1-z im-sqrt-1+z)
                                      (fpmul im-sqrt-1-z re-sqrt-1+z)))))))))

(defun fpasin (x)
  ($bfloat (fpasin-core x)))

(defun fpasin-core (x)
  (let ((fp-x (cdr (check-bigfloat x))))
    (cond ((minusp (car fp-x))
           (mul -1 (fpasin (bcons (fpminus fp-x)))))
          ((fplessp fp-x (cdr bfhalf))
           (bcons (fpatan (fpdiv fp-x
                                 (fproot (bcons (fpmul (fpsub (fpone) fp-x)
                                                       (fpadd (fpone) fp-x)))
                                         2)))))
          ((fpgreaterp fp-x (fpone))
           (let ((arg (fpadd fp-x
                             (fproot (bcons (fpmul (fpsub fp-x (fpone))
                                                   (fpadd fp-x (fpone))))
                                     2))))
             (add (div '$%pi 2)
                  (mul -1 '$%i (bcons (fplog arg))))))
          (t
           (add (div '$%pi 2)
                (mul -1
                     (bcons
                       (fpatan
                         (fpdiv (fproot (bcons (fpmul (fpsub (fpone) fp-x)
                                                      (fpadd (fpone) fp-x)))
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
        (complex-sqrt (bcons (fpsub (intofp 1) x))
                      (bcons (fpminus y)))
      (multiple-value-bind (re-sqrt-1+z im-sqrt-1+z)
          (complex-sqrt (bcons (fpadd (intofp 1) x))
                        (bcons y))
        (values (bcons (fpmul (intofp 2)
                              (fpatan (fpdiv re-sqrt-1-z re-sqrt-1+z))))
                (fpasinh (bcons (fpsub (fpmul re-sqrt-1+z im-sqrt-1-z)
                                       (fpmul im-sqrt-1+z re-sqrt-1-z)))))))))

(defun fpacos (x)
  ($bfloat (add (div '$%pi 2) (mul -1 (fpasin-core x)))))

;;; ----------------------------------------------------------------------------

(defprop %atan atanbigfloat floatprog)

(defun atanbigfloat (x)
  (fpatan* (car x) (cdr x)))

(defun fpatan* (a y)
  (fpend (let ((fpprec (+ 8 fpprec)))
           (if (null y)
               (if (bigfloatp a)
                   (fpatan (cdr (check-bigfloat a)))
                   (list '(%atan) a))
               (fpatan2 (cdr (check-bigfloat a))
                        (cdr (check-bigfloat (car y))))))))

(defun fpatan (x)
  (prog (term x2 ans oans one two tmp)
    (setq one (intofp 1) two (intofp 2))
    (cond ((fpgreaterp (fpabs x) one)
           (setq tmp (fpdiv (fppi) two))
           (setq ans (fpsub tmp (fpatan (fpdiv one x))))
           (return (cond ((fplessp x (intofp 0))
                          (fpsub ans (fppi)))
                         (t ans))))
          ((fpgreaterp (fpabs x) (fpdiv one two))
           (setq tmp (fpdiv x (fpadd (fpmul x x) one)))
           (setq x2 (fpmul x tmp) term (setq ans one))
           (do ((n 0 (1+ n)))
               ((equal ans oans))
             (setq term
                   (fpmul term
                          (fpmul x2
                                 (fpdiv (intofp (+ 2 (* 2 n)))
                                        (intofp (+ (* 2 n) 3))))))
             (setq oans ans
                   ans (fpadd term ans)))
           (setq ans (fpmul tmp ans)))
          (t
           (setq ans x
                 x2 (fpminus (fpmul x x)) term x)
           (do ((n 3 (+ n 2)))
               ((equal ans oans))
             (setq term (fpmul term x2))
             (setq oans ans
                   ans (fpadd ans (fpdiv term (intofp n)))))))
    (return ans)))

(defun fpatan2 (y x)
  (cond ((eql (car x) 0)
         (cond ((equal (car y) 0)
                (merror "atan2: atan2(0, 0) is undefined."))
               ((minusp (car y))
                (fpdiv (fppi) (intofp -2)))
               (t
                (fpdiv (fppi) (intofp 2)))))
        ((> (car x) 0)
         (fpatan (fpdiv y x)))
        ((> (car y) 0)
         (fpadd (fppi) (fpatan (fpdiv y x))))
        (t
         (fpsub (fpatan (fpdiv y x)) (fppi)))))

;;; ----------------------------------------------------------------------------

(defun big-float-sinh (x &optional y)
  (unless y
    (fpsinh x)))

(defun fpsinh (x)
  (cond ((eql 0 (cadr x))
         (check-bigfloat x))
        ((fpposp (cdr x))
         (let ((d (fpexpm1 (cdr (check-bigfloat x)))))
           (bcons (fpdiv (fpadd d (fpdiv d (fpadd d (fpone))))
                         (intofp 2)))))
        (t
         (bcons 
           (fpminus
             (cdr (fpsinh (bcons (fpminus (cdr (check-bigfloat x)))))))))))

(defun fpexpm1 (x)
  (cond ((fpgreaterp (fpabs x) (fpone))
         (fpsub (fpexp x) (fpone)))
        (t
         (let ((ans x)
               (oans nil)
               (term x))
           (do ((n 2 (1+ n)))
               ((equal ans oans))
             (setf term (fpdiv (fpmul x term) (intofp n)))
             (setf oans ans)
             (setf ans (fpadd ans term)))
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
	 (beta (fpadd (fpone) (fpmul tv tv)))
	 (s (cdr (fpsinh x)))
	 (s^2 (fpmul s s))
	 (rho (fproot (bcons (fpadd (fpone) s^2)) 2))
	 (den (fpadd (fpone) (fpmul beta s^2))))
    (values (bcons (fpdiv (fpmul beta (fpmul rho s)) den))
	    (bcons (fpdiv tv den)))))

(defun fptanh (x)
  (let* ((two (intofp 2))
         (fp (cdr (check-bigfloat x)))
         (d (fpexpm1 (fpmul fp two))))
    (bcons (fpdiv d (fpadd d two)))))

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
                   (fpadd (fpmul absx two)
                          (fpdiv one
                                 (fpadd absx
                                        (fproot (bcons (fpadd one
                                                              (fpmul absx
                                                                     absx)))
                                                2)))))))
          (t
           (let ((x*x (fpmul absx absx)))
             (setq result
                   (fplog1p
                     (fpadd absx
                            (fpdiv x*x
                                   (fpadd one
                                          (fproot (bcons (fpadd one x*x))
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
         (x-lt-minus-1 (fplessp (fpadd fpx (fpone)) '(0 0)))
         (x-gt-plus-1 (fpgreaterp fpy (fpone)))
         (y-equals-0 (equal y '((bigfloat) 0 0)))
         (x (fpmul beta fpx))
         (y (fpmul beta (fpminus fpy)))
         (rho (intofp 0))
         (t1 (fpadd (fpabs y) rho))
         (t1^2 (fpmul t1 t1))
         (1-x (fpsub (fpone) x))
         (eta (fpdiv (fplog1p (fpdiv (fpmul (intofp 4) x)
                                     (fpadd (fpmul 1-x 1-x) t1^2)))
                     (intofp 4)))
         (nu (if y-equals-0
                 (fpminus
                   (if x-lt-minus-1
                       (cdr ($bfloat '((mquotient) $%pi 2)))
                       (if x-gt-plus-1
                           (cdr ($bfloat '((mminus) ((mquotient) $%pi 2))))
                           (merror "COMPLEX-ATANH: HOW DID I GET HERE?"))))
                 (fpmul (cdr bfhalf)
                        (fpatan2 (fpmul (intofp 2) y)
                                 (fpsub (fpmul 1-x (fpadd (fpone) x))
                                        t1^2))))))
    (values (bcons (fpmul beta eta))
            (bcons (fpminus (fpmul beta nu))))))

(defun fpatanh (x)
  (let* ((fp-x (cdr (check-bigfloat x))))
    (cond ((fplessp fp-x (intofp 0))
           (mul -1 (fpatanh (bcons (fpminus fp-x)))))
          ((fpgreaterp fp-x (fpone))
           (multiple-value-bind (u v)
               (complex-atanh x (bcons (intofp 0)))
             (add u (mul '$%i v))))
          ((fpgreaterp fp-x (cdr bfhalf))
           (bcons (fpmul (cdr bfhalf)
                         (fplog1p (fpdiv (fpmul (intofp 2) fp-x)
                                         (fpsub (fpone) fp-x))))))
          (t
           (let ((2x (fpmul (intofp 2) fp-x)))
             (bcons (fpmul (cdr bfhalf)
                           (fplog1p (fpadd 2x
                                           (fpdiv (fpmul 2x fp-x)
                                                  (fpsub (fpone)
                                                         fp-x)))))))))))

;;; ----------------------------------------------------------------------------

(defprop bigfloat msize-bigfloat grind)

(defun msize-bigfloat (x l r)
  (msz (fpformat x) l r))

(defun dim-bigfloat (form result)
  (declare (special $lispdispflag))
  (let (($lispdispflag nil))
    (dimension-string (fpformat form) result)))

(defun fpformat (l)
  (if (not (member 'simp (cdar l) :test #'eq))
      (setq l (cons (cons (caar l) (cons 'simp (cdar l))) (cdr l))))
  (cond ((eql (cadr l) 0)
         (if (not (eql (caddr l) 0))
             (merror "fpformat: detected an incorrect form of 0.0b0: ~M, ~M~%"
                    (cadr l) (caddr l)))
         (list #\0 #\. #\0 #\b #\0))
        (t
         (let ((extradigs (floor (1+ (/ (integer-length (caddr l))
                                        #.(/ (log 10.0) (log 2.0))))))
               (*m 1)
               (*cancelled 0))
           (setq l
                 (let ((*decfp* t)
                       (fpprec (+ extradigs (decimalsin (- (caddar l) 2))))
                       (of (caddar l))
                       (l (cdr l))
                       (expon nil))
                   (setq expon (- (cadr l) of))
                   (setq l (if (minusp expon)
                               (fpdiv (intofp (car l))
                                      (fpintexpt 2 (- expon) of))
                               (fpmul (intofp (car l))
                                      (fpintexpt 2 expon of))))
                   (incf fpprec (- extradigs))
                   (list (fpround (car l)) (+ (- extradigs) *m (cadr l))))))
         (let ((*print-base* 10)
               *print-radix*
               (l1 nil))
           (setq l1 (if (not $bftrunc)
                        (coerce (print-invert-case (car l)) 'list)
                        (do ((l (nreverse
                                  (coerce (print-invert-case (car l)) 'list))
                                (cdr l)))
                            ((not (eql #\0 (car l))) (nreverse l)))))
           (nconc (ncons (car l1))
                  (ncons #\. )
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
                                                  ((not (eql #\0 (car l3)))
                                                   (nreverse l3))))))
                                    (setq l2 (cons (car l1) l2)
                                          l1 (cdr l1))))))
                      (ncons #\0))
                  (ncons #\b)
                  (coerce (print-invert-case (1- (cadr l))) 'list))))))

(defun decimalsin (x)
  (do ((i (truncate (* 59 x) 196) (1+ i)))
      (nil)
    (when (> (integer-length (expt 10 i)) x)
      (return (1- i)))))

(defun fpintexpt (int nn fixprec)
  (setq fixprec (truncate fixprec (1- (integer-length int))))
  (let ((bas (intofp (expt int (min nn fixprec)))))
    (if (> nn fixprec)
        (fpmul (intofp (expt int (rem nn fixprec)))
               (fpexpt bas (truncate nn fixprec)))
        bas)))

;;; ----------------------------------------------------------------------------

(eval-when (:load-toplevel :execute)
    (fpprec1 nil $fpprec))

;;; ----------------------------------------------------------------------------
