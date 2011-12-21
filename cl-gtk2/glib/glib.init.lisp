;;; ----------------------------------------------------------------------------
;;; glib.init.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2 from
;;; http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GLib 2.30.2 Reference Manual
;;; See http://www.gtk.org.
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

(in-package :glib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defvar *initializers-table* (make-hash-table :test 'equalp))
  (defvar *initializers* nil)
  
  (defun register-initializer (key fn)
    (unless (gethash key *initializers-table*)
      (setf (gethash key *initializers-table*) t
            *initializers* (nconc *initializers* (list fn)))))
  
  (defvar *finalizers-table* (make-hash-table :test 'equalp))
  (defvar *finalizers* nil)
  
  (defun register-finalizer (key fn)
    (unless (gethash key *finalizers-table*)
      (setf (gethash key *finalizers-table*) t
            *finalizers* (nconc *finalizers* (list fn))))))

(defun run-initializers ()
  (iter (for fn in *initializers*)
        (funcall fn)))

(defun run-finalizers ()
  (iter (for fn in *finalizers*)
        (funcall fn)))

#+sbcl
(pushnew 'run-initializers sb-ext:*init-hooks*)
#+openmcl
(pushnew 'run-initializers ccl:*restore-lisp-functions*)

#+sbcl
(pushnew 'run-finalizers sb-ext:*save-hooks*)
#+openmcl
(pushnew 'run-finalizers ccl:*save-exit-functions*)

;;; ----------------------------------------------------------------------------
;;; at-init(keys body)
;;;
;;; Runs the code normally but also schedules the code to be run at image load
;;; time. It is used to reinitialize the libraries when the dumped image is
;;; loaded. (Works only on SBCL for now).
;;;
;;; At-init form may be called multiple times. The same code from should not be
;;; run multiple times at initialization time (in best case, this will only
;;; slow down initialization, in worst case, the code may crash). To ensure
;;; this, every at-init expression is added to hash-table with the body and
;;; keys as a composite key. This ensures that the same code is only executed
;;; once (once on the same set of parameters).
;;;
;;; Example:
;;; 
;;; (defmethod initialize-instance :after ((class gobject-class)
;;;                                        &key &allow-other-keys)
;;;   (register-object-type (gobject-class-g-type-name class)
;;;                         (class-name class))
;;;   (at-init (class) (initialize-gobject-class-g-type class)))
;;;
;;; In this example, for every class, (initialize-gobject-class-g-type class)
;;; will be called only once.
;;; ----------------------------------------------------------------------------

(defmacro at-init ((&rest keys) &body body)
  `(progn (register-initializer (list ,@keys ',body) (lambda () ,@body))
          ,@body))

(defmacro at-finalize ((&rest keys) &body body)
  `(register-finalizer (list ,@keys ',body) (lambda () ,@body)))

(at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library glib
      ((:and :unix (:not :darwin))
       (:or "libglib-2.0.so.0" "libglib-2.0.so"))
      (:darwin (:or "libglib-2.0.0.dylib" "libglib-2.0.dylib"))
      (:windows "libglib-2.0-0.dll")
      (t (:default "libglib-2.0"))))
  
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library gthread
      ((:and :unix (:not :darwin))
       (:or "libgthread-2.0.so.0"  "libgthread-2.0.so"))
      (:darwin (:or "libgthread-2.0.0.dylib"  "libgthread-2.0.dylib"))
      (:windows "libgthread-2.0-0.dll")
      (t "libgthread-2.0")))

  (use-foreign-library glib)
  (use-foreign-library gthread))

;;; --- End of file glib.init.lisp --------------------------------------------- 
