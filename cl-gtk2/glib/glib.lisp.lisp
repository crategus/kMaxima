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

;;; Lisp support for GList

(define-foreign-type glist-type ()
  ((type :reader glist-type-type :initarg :type :initform :pointer)
   (free-from-foreign :reader glist-type-free-from-foreign :initarg :free-from-foreign :initform t)
   (free-to-foreign :reader glist-type-free-to-foreign :initarg :free-to-foreign :initform t))
  (:actual-type :pointer))

(define-parse-method glist (type &key (free-from-foreign t) (free-to-foreign t))
  (make-instance 'glist-type
                 :type type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defmethod translate-from-foreign (pointer (type glist-type))
  (prog1
      (iter (for c initially pointer then (g-list-next c))
            (until (null-pointer-p c))
            (collect (convert-from-foreign (foreign-slot-value c 'g-list 'data) (glist-type-type type))))
    (when (glist-type-free-from-foreign type)
      (g-list-free pointer))))

;;; ----------------------------------------------------------------------------

;;; Lisp support for GSList

(define-foreign-type gslist-type ()
  ((type :reader gslist-type-type :initarg :type :initform :pointer)
   (free-from-foreign :reader gslist-type-free-from-foreign :initarg :free-from-foreign :initform t)
   (free-to-foreign :reader gslist-type-free-to-foreign :initarg :free-to-foreign :initform t))
  (:actual-type :pointer))

(define-parse-method gslist (type &key (free-from-foreign t) (free-to-foreign t))
  (make-instance 'gslist-type
                 :type type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defmethod translate-from-foreign (pointer (type gslist-type))
  (prog1
      (iter (for c initially pointer then (g-slist-next c))
            (until (null-pointer-p c))
            (collect (convert-from-foreign (foreign-slot-value c 'g-slist 'data) (gslist-type-type type))))
    (when (gslist-type-free-from-foreign type)
      (g-slist-free pointer))))

(defmethod translate-to-foreign (list (type gslist-type))
  (let ((result (null-pointer)) last)
    (iter (for item in list)
          (for n = (g-slist-alloc))
          (for ptr = (convert-to-foreign item (gslist-type-type type)))
          (setf (foreign-slot-value n 'g-slist 'data) ptr)
          (setf (foreign-slot-value n 'g-slist 'next) (null-pointer))
          (when last
            (setf (foreign-slot-value last 'g-slist 'next) n))
          (setf last n)
          (when (first-iteration-p)
            (setf result n)))
    result))

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

;;; Lisp support for threads


;;; Initialize threads

(at-init ()
  (unless (g-thread-get-initialized)
    (g-thread-init (null-pointer))))

;;; ----------------------------------------------------------------------------
