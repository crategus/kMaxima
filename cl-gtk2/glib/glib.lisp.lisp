;;; ----------------------------------------------------------------------------
;;; glib.lisp.lisp
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

;;; ----------------------------------------------------------------------------

;;; Check Version information

(defmacro push-library-version-features (library-name
                                         major-version-var
                                         minor-version-var
                                         &body versions)
  `(eval-when (:load-toplevel :execute)
     ,@(iter (for (major minor) on versions by #'cddr)
             (collect
                 `(when (or (and (= ,major-version-var ,major)
                                 (>= ,minor-version-var ,minor))
                            (> ,major-version-var ,major))
                    (pushnew ,(intern (format nil "~A-~A.~A"
                                              (string library-name)
                                              major minor)
                                      (find-package :keyword))
                             *features*))))))

(define-condition foreign-library-minimum-version-mismatch (error)
  ((library :initarg :library :reader .library)
   (minimum-version :initarg :minimum-version :reader .minimum-version)
   (actual-version :initarg :actual-version :reader .actual-version))
  (:report (lambda (c s)
             (format s "Library ~A has too old version: it is ~A but required ~
                       to be at least ~A"
                     (.library c)
                     (.actual-version c)
                     (.minimum-version c)))))

(defun require-library-version (library min-major-version
                                        min-minor-version
                                        major-version 
                                        minor-version)
  (unless (or (> major-version min-major-version)
              (and (= major-version min-major-version)
                   (>= minor-version min-minor-version)))
    (restart-case
        (error 'foreign-library-minimum-version-mismatch
               :library library
               :minimum-version (format nil "~A.~A"
                                        min-major-version min-minor-version)
               :actual-version (format nil "~A.~A"
                                       major-version minor-version))
      (ignore () :report "Ignore version requirement" nil))))

(push-library-version-features glib *glib-major-version* *glib-micro-version*
  2 2
  2 4
  2 6
  2 8
  2 10
  2 12
  2 14
  2 16
  2 18
  2 20
  2 22)

(require-library-version "Glib" 2 20 *glib-major-version* *glib-minor-version*)

;;; ----------------------------------------------------------------------------

;;; Lisp support for GError

(define-condition g-error-condition (error)
  ((domain :initarg :domain :initform nil :reader g-error-condition-domain)
   (code :initarg :code :initform nil :reader g-error-condition-code)
   (message :initarg :message :initform nil :reader g-error-condition-message))
  (:report (lambda (e stream)
             (format stream "GError was raised. Domain: ~S, code: ~S, ~
                             message: ~A"
                     (g-error-condition-domain e)
                     (g-error-condition-code e)
                     (g-error-condition-message e)))))

(defun mayber-raise-g-error-condition (err)
  (unless (null-pointer-p err)
    (error 'g-error-condition
           :domain (foreign-slot-value err 'gerror :domain)
           :code (foreign-slot-value err 'gerror :code)
           :message (foreign-slot-value err 'gerror :message))))

(defmacro with-g-error ((err) &body body)
  `(with-foreign-object (,err :pointer)
     (setf (mem-ref ,err :pointer) (null-pointer))
     (unwind-protect
          (progn ,@body)
       (mayber-raise-g-error-condition (mem-ref ,err :pointer))
       (g-clear-error ,err))))

(defmacro with-catching-to-g-error ((err) &body body)
  `(handler-case
       (progn ,@body)
     (g-error-condition (e)
       (g-set-error-literal ,err
                            (g-error-condition-domain e)
                            (g-error-condition-code e)
                            (g-error-condition-message e)))))

;;; Initialize threads

(at-init ()
  (unless (g-thread-get-initialized)
    (g-thread-init (null-pointer))))
