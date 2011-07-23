;;;; Created on 2011-05-08 21:55:08

(in-package :kmaxima-tests)

(define-test mfunctionp
  (assert-true (mfunctionp #'car ))
  (assert-true (mfunctionp 'car))
  (assert-false (mfunctionp 'defprop))
  (assert-true (functionp #'car ))
  (assert-false (functionp 'car)))

(define-test fixnump
  (assert-true (fixnump 1))
  (assert-false (fixnump 1.0))
  (assert-false (fixnump 'symbol)))

(define-test alphabetp
  (assert-true (alphabetp #\a ))
  (assert-false (alphabetp #\1 ))
  (assert-false (alphabetp #\* ))
  (assert-true (alphabetp #\_ ))
  (assert-true (alphabetp #\% ))
  (assert-equal (list #\_ #\%) *alphabet*))

(define-test mminusp
  (assert-true (mminusp '((mminus))))
  (assert-true (mminusp '((mminus) $a)))
  (assert-false (mminusp '(mminus $a)))
  (assert-false (mminusp '(() $a))))

(define-test mlistp
  (assert-true (mlistp '((mlist))))
  (assert-true (mlistp '((mlist) $a)))
  (assert-false (mlistp '(mlist $a)))
  (assert-false (mlistp '(() $a))))

(define-test getprop
  (defprop sym val-1 indic-1)
  (putprop 'sym 'val-2 'indic-2)
  (assert-equal 'val-1 (getprop 'sym 'indic-1))
  (assert-equal 'val-2 (getprop 'sym 'indic-2))
  (assert-false (getprop 'sym 'indic))
  (assert-equal 'val-1 (cadr (getpropl 'sym '(indic-1))))
  (assert-equal 'val-2 (cadr (getpropl 'sym '(indic-2)))))

(define-test add2lnc
  (let ((l1 nil))
    (assert-equal '((mlist) $a)       (setq l1 (add2lnc '$a (list '(mlist)))))
    (assert-equal '((mlist) $a $b)    (add2lnc '$b l1))
    (assert-equal '((mlist) $a $b $c) (add2lnc '$c l1))
    (assert-equal '((mlist) $a $b $c) (add2lnc '$b l1)))
    
  (let ((l2 nil))
    (assert-equal '(a)     (setq l2 (add2lnc 'a l2)))
    (assert-equal '(a b)   (add2lnc 'b l2))
    (assert-equal '(a b c) (add2lnc 'c l2))
    (assert-equal '(a b c) (add2lnc 'b l2)))
  
  (let ((l3 nil))
    (assert-equal '((($f) x y))            (setq l3 (add2lnc '(($f) x y) l3)))
    (assert-equal '((($f) x y) (($g) x y)) (add2lnc '(($g) x y) l3))
    (assert-equal '((($f) x y) (($g) a b)) (add2lnc '(($g) a b) l3)))
  )

(define-test exploden
  (assert-equal '(#\a #\b ) (exploden 'ab ))
  (assert-equal '(#\A #\B ) (exploden '|ab| ))
  (assert-equal '(#\A #\b ) (exploden '|Ab| ))
    
  (assert-equal '(#\1 #\2 #\3 ) (exploden 123))
  (assert-equal '(#\1 #\. #\2 #\3 ) (exploden 1.23))
  (assert-equal '(#\0 #\. #\1 #\2 #\3 ) (exploden .123)))

