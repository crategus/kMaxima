;;; ----------------------------------------------------------------------------
;;; gtk.cl-gtk2-gtk.lisp
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

(defpackage #:cl-gtk2-gtk-system
  (:use #:cl #:asdf))

(in-package #:cl-gtk2-gtk-system)

(defclass plain-file (static-file)
  ((type :initarg :type :reader plain-file-type :initform nil)))

(defmethod source-file-type ((c plain-file) (s module))
  (plain-file-type c))

(defsystem :cl-gtk2-gtk
  :name :cl-gtk2-gtk
  :version "0.1.1"
  :author "Kalyanov Dmitry <Kalyanov.Dmitry@gmail.com>"
  :license "LLGPL"
  :serial t
  :components ((:file "gtk.package")
               (:file "gtk.misc")
               
               (:file "gtk.generated-classes")
               
               (:file "gtk.main-loop")
               
               (:file "gtk.object")
               (:file "gtk.objects")
               
               (:file "gtk.hsv")
               

               (:file "gtk.functions")
               (:file "gtk.base-classes")
               (:file "gtk.dialog")
               (:file "gtk.about-dialog")
               (:file "gtk.window")
               (:file "gtk.window-group")
               (:file "gtk.icon-factory")
               (:file "gtk.image")
               (:file "gtk.label")
               (:file "gtk.progress-bar")
               (:file "gtk.status-bar")
               (:file "gtk.status-icon")
               (:file "gtk.scale-button")
               (:file "gtk.entry")
               (:file "gtk.spin-button")
               (:file "gtk.selections")
               (:file "gtk.dnd")
               (:file "gtk.text")
               
               (:file "gtk.tree-model")
               (:file "gtk.tree-model-sort")
               (:file "gtk.tree-view-column")
               (:file "gtk.tree-selection")
               (:file "gtk.tree-view")
               
               (:file "gtk.icon-view")
               (:file "gtk.cell-layout")
               (:file "gtk.cell-renderer")
               (:file "gtk.combo-box")
               (:file "gtk.menu")
               (:file "gtk.ui-manager")
               (:file "gtk.selectors")
               (:file "gtk.layout-containers")
               (:file "gtk.scrolling")
               (:file "gtk.calendar")
               (:file "gtk.size-group")
               (:file "gtk.tooltip")
               (:file "gtk.box")
               (:file "gtk.container")
               (:file "gtk.paned")
               (:file "gtk.child-properties")
               (:file "gtk.widget")
               (:file "gtk.tree-view-dnd")
               (:file "gtk.builder")
               (:file "gtk.assistant")
               (:file "gtk.link-button")
               (:file "gtk.list-store")
               (:file "gtk.tree-store")
               (:file "gtk.tree-model-filter")
               (:file "gtk.clipboard")
               
               (:file "gtk.generated-child-properties")
               
               (:file "gtk.high-level")

               (:file "ui-markup")
               (:file "gtk.timer")
               (:file "gtk.finalize-classes")
               
               (:file "gtk.init")
               
               (:file "gtk.demo")
               (:file "gtk.dialog.example")
               
               (:module "demo-files"
                        :pathname "demo/"
                        :components ((:plain-file "demo1" :type "ui")
                                     (:plain-file "text-editor" :type "ui"))))
  :depends-on (:cl-gtk2-glib
               :cl-gtk2-gobject
               :cl-gtk2-gdk
               :cl-gtk2-pango
               :cffi
               :bordeaux-threads
               :iterate))

;;; --- End of file cl-gtk2-gtk.asd --------------------------------------------
