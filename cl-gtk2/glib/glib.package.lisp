;;; ----------------------------------------------------------------------------
;;; glib.package.lisp
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

(defpackage :glib
  (:use :cl :cffi :iter)
  (:export #:at-init
           
           #:g-free
           #:glist
           #:gstrv
           #:g-malloc
           #:g-strdup
           #:g-string
           #:gslist
           #:g-quark
           #:+g-priority-high+
           #:+g-priority-default+
           #:+g-priority-high-idle+
           #:+g-priority-default-idle+
           #:+g-priority-low+
           #:g-idle-add-full
           #:g-idle-add
           #:g-timeout-add-full
           #:g-source-remove
           #:at-finalize
           #:with-g-error
           #:with-catching-to-g-error
           #:g-error-condition
           #:g-error-condition-domain
           #:g-error-condition-code
           #:g-error-condition-message
           #:g-spawn-flags
           #:push-library-version-features
           #:foreign-library-minimum-version-mismatch
           #:require-library-version
           #:get-user-cache-dir
           #:get-user-cache-dir
           #:get-user-config-dir
           #:build-filename
           #:random-double-range
           #:random-double
           #:random-int-range
           #:random-int
           #:random-boolean
           #:random-set-seed
    ))
