;;;; Created on 2011-07-23 14:03:16

(load "lisp/lisp-unit.lisp")

(defpackage :kmaxima-tests
  (:use :common-lisp :lisp-unit :kmaxima))

(in-package :kmaxima-tests)

(load "tests/rtest_mutils.lisp")
(load "tests/rtest_parser.lisp")
(load "tests/rtest_nformat.lisp")
(load "tests/rtest_display.lisp")
(load "tests/rtest_float.lisp")

(run-all-tests :kmaxima-tests)
