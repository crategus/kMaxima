;;;; Created on 2011-06-23 21:16:10

(load "lisp/lisp-unit.lisp")

(defpackage :kmaxima-tests
  (:use :common-lisp :lisp-unit :kmaxima))

(in-package :kmaxima-tests)

(define-test form-mexpt
  (setq $sqrtdispflag nil)
  (assert-equal '((mexpt) $a ((rat) 1 2)) (form-mexpt '((mexpt) $a ((rat) 1 2))))
  (setq $sqrtdispflag t)
  (assert-equal '((%sqrt) $a) (form-mexpt '((mexpt) $a ((rat) 1 2))))
  (setq $%edispflag nil)
  (assert-equal '((mexpt) $%e -2) (form-mexpt '((mexpt) $%e -2)))
  (setq $%edispflag t)
  (assert-equal '((mquotient) 1 ((mexpt) $%e 2)) (form-mexpt '((mexpt) $%e -2)))
  (setq $exptdispflag nil)
  (assert-equal '((mexpt) $a -2) (form-mexpt '((mexpt) $a -2)))
  (setq $exptdispflag t)
  (assert-equal '((mquotient) 1 ((mexpt) $a 2)) (form-mexpt '((mexpt) $a -2))))


