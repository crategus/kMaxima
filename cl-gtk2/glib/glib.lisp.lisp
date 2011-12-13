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
             (format s "Library ~A has too old version: it is ~A but required to be at least ~A"
                     (.library c)
                     (.actual-version c)
                     (.minimum-version c)))))

(defun require-library-version (library min-major-version min-minor-version major-version minor-version)
  (unless (or (> major-version min-major-version)
              (and (= major-version min-major-version)
                   (>= minor-version min-minor-version)))
    (restart-case
        (error 'foreign-library-minimum-version-mismatch
               :library library
               :minimum-version (format nil "~A.~A" min-major-version min-minor-version)
               :actual-version (format nil "~A.~A" major-version minor-version))
      (ignore () :report "Ignore version requirement" nil))))

(push-library-version-features glib *major-version* *micro-version*
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

(require-library-version "Glib" 2 20 *major-version* *minor-version*)

;;; ----------------------------------------------------------------------------

;;; Lisp support for GError

(define-condition g-error-condition (error)
  ((domain :initarg :domain :initform nil :reader g-error-condition-domain)
   (code :initarg :code :initform nil :reader g-error-condition-code)
   (message :initarg :message :initform nil :reader g-error-condition-message))
  (:report (lambda (e stream)
             (format stream "GError was raised. Domain: ~S, code: ~S, message: ~A"
                     (g-error-condition-domain e)
                     (g-error-condition-code e)
                     (g-error-condition-message e)))))

(defun mayber-raise-g-error-condition (err)
  (unless (null-pointer-p err)
    (error 'g-error-condition
           :domain (foreign-slot-value err 'g-error :domain)
           :code (foreign-slot-value err 'g-error :code)
           :message (foreign-slot-value err 'g-error :message))))

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

;;; ----------------------------------------------------------------------------

;;; Lisp support for String Utility Functions

(define-foreign-type gstrv-type ()
  ((free-from-foreign :initarg :free-from-foreign :initform t :reader gstrv-type-fff)
   (free-to-foreign :initarg :free-to-foreign :initform t :reader gstrv-type-ftf))
  (:actual-type :pointer))

(define-parse-method gstrv (&key (free-from-foreign t) (free-to-foreign t))
  (make-instance 'gstrv-type :free-from-foreign free-from-foreign :free-to-foreign free-to-foreign))

(defmethod translate-from-foreign (value (type gstrv-type))
  (unless (null-pointer-p value)
    (prog1
        (iter (for i from 0)
              (for str-ptr = (mem-aref value :pointer i))
              (until (null-pointer-p str-ptr))
              (collect (convert-from-foreign str-ptr '(:string :free-from-foreign nil)))
              (when (gstrv-type-fff type)
                (g-free str-ptr)))
      (when (gstrv-type-fff type)
        (g-free value)))))

(defmethod translate-to-foreign (str-list (type gstrv-type))
  (let* ((n (length str-list))
         (result (g-malloc (* (1+ n) (foreign-type-size :pointer)))))
    (iter (for i from 0)
          (for str in str-list)
          (setf (mem-aref result :pointer i) (g-strdup str)))
    (setf (mem-aref result :pointer n) (null-pointer))
    result))

;;; ----------------------------------------------------------------------------

;;; Initialize threads

(at-init ()
  (unless (g-thread-get-initialized)
    (g-thread-init (null-pointer))))

;;; ----------------------------------------------------------------------------

;;; Support for String Utility Functions

;; A type that it almost like :string but uses g_malloc and g_free

(define-foreign-type g-string-type ()
  ((free-from-foreign :initarg :fff :reader g-string-type-fff :initform nil)
   (free-to-foreign :initarg :ftf :reader g-string-type-ftf :initform t))
  (:actual-type :pointer))

(define-parse-method g-string (&key (free-from-foreign nil) (free-to-foreign t))
  (make-instance 'g-string-type :fff free-from-foreign :ftf free-to-foreign))

(defmethod translate-to-foreign (value (type g-string-type))
  (g-strdup value))

(defmethod translate-from-foreign (value (type g-string-type))
  (prog1
      (convert-from-foreign value '(:string :free-from-foreign nil))
    (when (g-string-type-fff type)
      (g-free value))))

;;; ----------------------------------------------------------------------------
