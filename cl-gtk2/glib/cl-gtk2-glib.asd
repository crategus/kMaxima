;;; ----------------------------------------------------------------------------
;;; cl-gtk2-glib.asd
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

(defsystem :cl-gtk2-glib
  :name :cl-gtk2-glib
  :version "0.1.1"
  :author "Kalyanov Dmitry <Kalyanov.Dmitry@gmail.com>"
  :license "LLGPL"
  :serial t
  :components ((:file "glib.package")
               (:file "glib.version")
               (:file "glib.basic-types")
               
               (:file "glib.init")
               (:file "glib.mem")
               (:file "glib.date")
               (:file "glib.threads")
               (:file "glib.main-loop")
               (:file "glib.spawn")
               (:file "glib.glist")
               (:file "glib.gslist")
               (:file "glib.string")
               (:file "glib.quark")
               (:file "glib.error")
               (:file "glib.utils")
               (:file "glib.rand")
               (:file "glib.lisp"))
  :depends-on (:cffi
               :trivial-garbage
               :iterate
               :bordeaux-threads
               :iterate
               :closer-mop))
