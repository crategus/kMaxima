;;;; Created on 2011-07-23 14:03:16

(asdf:operate 'asdf:load-op :cl-gtk2-gtk)

(load "lisp-unit.lisp")

(load "rtest-glib.lisp")
(load "rtest-gobject-type-info.lisp")

(in-package :glib-tests)
(run-all-tests :glib-tests)

(in-package :gobject-tests)
(run-all-tests :gobject-tests)
