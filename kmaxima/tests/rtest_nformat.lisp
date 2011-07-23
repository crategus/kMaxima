;;;; Created on 2011-06-23 21:16:10

(in-package :kmaxima-tests)

(define-test nformat
  (assert-equal '((mminus) 100) (nformat -100))
  (assert-equal '((mminus) 1.0) (nformat -1.0))
  (assert-equal t (nformat t))
  (assert-equal nil (nformat nil))
  (assert-equal 'a (nformat 'a))
  (assert-equal 10 (nformat 10))
  (assert-equal '((rat) 1 2) (nformat '((rat simp) 1 2)))
  (assert-equal '((mminus) ((rat) 1 2)) (nformat '((rat simp) -1 2)))
  (assert-equal '(a b c) (nformat '(a b c)))
  (assert-equal '((mplus) $a -10) (nformat '((mplus) $a -10)))
  (assert-equal '((mplus) $a ((mminus) 10)) (nformat '((mplus simp) $a -10)))
  (assert-equal '((%sin) -10) (nformat '((%sin) -10)))
  (assert-equal '((%sin simp) -10) (nformat '((%sin simp) -10))) )

(define-test nformat-all
  (assert-equal '((%sin) ((mminus) 10)) (nformat-all '((%sin) -10)))
  (assert-equal '((%sin) ((mminus) 10)) (nformat-all '((%sin simp) -10)))
  (assert-equal '((%sin all) ((mminus) 10))
                (nformat-all '((%sin simp all) -10))) )

(define-test nformat-mplus
  (assert-equal '((mplus) $c $b $a) (nformat-mplus '((mplus simp) $a $b $c)))
  (let (($powerdisp t))
    (assert-equal '((mplus) $a $b $c) (nformat-mplus '((mplus simp) $a $b $c))))
  (assert-equal '((mplus) $a ((mminus) $b))
                  (nformat-mplus '((mplus simp) $a ((mtimes simp) -1 $b))))
  (let (($negsumdispflag nil))
    (assert-equal '((mplus) ((mminus) $b) $a)
                  (nformat-mplus '((mplus simp) $a ((mtimes simp) -1 $b))))) )

(define-test nformat-mtimes
  (assert-equal '((mquotient) ((mtimes) 2 $x) 3)
                (nformat-mtimes '((mtimes simp) ((rat simp) 2 3) $x)))
  (assert-equal '((mminus) ((mquotient) ((mtimes) 2 $x) 3))
                (nformat-mtimes '((mtimes simp) ((rat simp) -2 3) $x)))
  (let (($pfeformat t))
    (assert-equal '((mtimes) ((rat) 2 3) $x)
                  (nformat-mtimes '((mtimes simp) ((rat simp) 2 3) $x)))
    (assert-equal '((mminus) ((mtimes) ((rat) 2 3) $x))
                  (nformat-mtimes '((mtimes simp) ((rat simp) -2 3) $x)))) )

(define-test nformat-mexpt
  (assert-equal '((%sqrt) $a) (nformat-mexpt '((mexpt) $a ((rat) 1 2))))
  (let (($sqrtdispflag nil))
    (assert-equal '((mexpt) $a ((rat) 1 2))
                  (nformat-mexpt '((mexpt) $a ((rat) 1 2)))))
  (assert-equal '((mexpt) $%e -2) (nformat-mexpt '((mexpt) $%e -2)))
  (let (($%edispflag t))
    (assert-equal '((mquotient) 1 ((mexpt) $%e 2))
                  (nformat-mexpt '((mexpt) $%e -2))))
  (assert-equal '((mquotient) 1 ((mexpt) $a 2))
                (nformat-mexpt '((mexpt) $a -2)))
  (let (($exptdispflag nil))
    (assert-equal '((mexpt) $a -2)
                  (nformat-mexpt '((mexpt) $a -2)))) )
