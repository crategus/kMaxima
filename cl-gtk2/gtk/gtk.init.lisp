;;; ----------------------------------------------------------------------------
;;; gtk.init.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2 from
;;; http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 2.2.2 Reference Manual
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

(in-package :gtk)

(at-init () (gtk-init))

#+thread-support
(progn
  (defvar *main-thread* nil)
  (defvar *main-thread-level* nil)
  (defvar *main-thread-lock* (bt:make-lock "*main-thread* lock"))

  (at-finalize ()
    (when (and *main-thread* (bt:thread-alive-p *main-thread*))
      (bt:destroy-thread *main-thread*)
      (setf *main-thread* nil)))

  (defun ensure-gtk-main ()
    (bt:with-lock-held (*main-thread-lock*)
      (when (and *main-thread* (not (bt:thread-alive-p *main-thread*)))
        (setf *main-thread* nil))
      (unless *main-thread*
        (setf *main-thread* (bt:make-thread (lambda () (gtk-main)) :name "cl-gtk2 main thread")
              *main-thread-level* 0))
      (incf *main-thread-level*))
    (values))

  (defun join-gtk-main ()
    (when *main-thread*
      (bt:join-thread *main-thread*)))

  (defun leave-gtk-main ()
    (bt:with-lock-held (*main-thread-lock*)
      (decf *main-thread-level*)
      (when (zerop *main-thread-level*)
        (main-quit)))))

#-thread-support
(progn
  (defun ensure-gtk-main ()
    (gtk-main)
    (values))

  (defun leave-gtk-main ()
    (main-quit))
  
  (defun join-gtk-main ()))

(export 'ensure-gtk-main)

(export 'leave-gtk-main)

(export 'join-gtk-main)

;;; --- End of file gtk.init.lisp ----------------------------------------------
