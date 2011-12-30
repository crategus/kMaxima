(defpackage :cl-gtk2-init
  (:use :cl :glib))

(in-package :cl-gtk2-init)

(at-init ()
 (eval-when (:compile-toplevel :load-toplevel :execute)
   (cffi:define-foreign-library gobject
     ((:and :unix (:not :darwin)) (:or "libgobject-2.0.so.0" "libgobject-2.0.so"))
     (:darwin (:or "libgobject-2.0.0.dylib" "libgobject-2.0.dylib"))
     (:windows "libgobject-2.0-0.dll")
     (t "libgobject-2.0")))

 (cffi:use-foreign-library gobject))

(in-package :gobject)

(defvar *lisp-name-package* nil
  "For internal use (used by class definitions generator).
  Specifies the package in which symbols are interned.")
(defvar *generated-types* nil)

(defvar *gobject-debug* nil)
(defvar *debug-gc* nil)
(defvar *debug-subclass* nil)
(defvar *debug-stream* t)

(defmacro log-for (categories control-string &rest args)
  (let ((vars (iter (for sym in (if (listp categories) categories (list categories)))
                    (collect (intern (format nil "*DEBUG-~A*" (symbol-name sym)) (find-package :gobject))))))
    `(progn
       (when (or ,@vars)
         (format *debug-stream* ,control-string ,@args))
       nil)))

(defmacro ev-case (keyform &body clauses)
  "Macro that is an analogue of CASE except that it evaluates keyforms"
  (let ((value (gensym)))
    `(let ((,value ,keyform))
       (cond
         ,@(loop
              for (key . forms) in clauses
              collect
                (if (eq key t)
                    `(t ,@forms)
                    `((equalp ,key ,value) ,@forms)))))))
