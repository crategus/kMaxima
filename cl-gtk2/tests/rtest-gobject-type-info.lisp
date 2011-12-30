;;;; Created on 2011-12-26 22:51:39

(asdf:operate 'asdf:load-op :cl-gtk2-gtk)

(defpackage :gobject-tests
  (:use :common-lisp :lisp-unit))

(in-package :gobject-tests)

(define-test g-type-make-fundamental
  (assert-eql (ash 1 2) (g:g-type-make-fundamental 1))
  (assert-eql (ash 2 2) (g:g-type-make-fundamental 2))
  (assert-eql (ash 3 2) (g:g-type-make-fundamental 3)))

(define-test g-type-is-value-type
  (assert-true (g:g-type-is-value-type g:+g-type-double+))
  (assert-true (g:g-type-is-value-type "gdouble"))
  (assert-true (g:g-type-is-value-type (g::gtype-from-name "gdouble")))
  (assert-true (g:g-type-is-value-type (g::gtype-from-id g:+g-type-double+)))
  
  (assert-false (g:g-type-is-value-type g:+g-type-interface+))
  (assert-false (g:g-type-is-value-type "GInterface"))
  (assert-false (g:g-type-is-value-type (g::gtype-from-name "GInterface")))
  (assert-false (g:g-type-is-value-type (g::gtype-from-id g:+g-type-interface+))))

(define-test g-type-flags
  (assert-equal '(:abstract)
                (cffi:foreign-bitfield-symbols 'g:g-type-flags (ash 1 4)))
  (assert-equal '(:value-abstract)
                (cffi:foreign-bitfield-symbols 'g:g-type-flags (ash 1 5)))
  (assert-equal '(:abstract :value-abstract)
                (cffi:foreign-bitfield-symbols 'g:g-type-flags
                                               (+ (ash 1 4) (ash 1 5))))
  (assert-eql (ash 1 4)
              (cffi:foreign-bitfield-value 'g:g-type-flags '(:abstract)))
  (assert-eql (ash 1 5)
              (cffi:foreign-bitfield-value 'g:g-type-flags '(:value-abstract)))
  (assert-eql (+ (ash 1 4) (ash 1 5))
              (cffi:foreign-bitfield-value 'g:g-type-flags
                                           '(:abstract :value-abstract))))

(define-test gtype-id+name
  (assert-equal 60 (g:gtype-id (g::gtype-from-id g:+g-type-double+)))
  (assert-equal "gdouble" (g:gtype-name (g::gtype-from-name "gdouble"))))

