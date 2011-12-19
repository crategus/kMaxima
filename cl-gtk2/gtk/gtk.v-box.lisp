;;; ----------------------------------------------------------------------------
;;; gtk.v-box.lisp
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
;;;
;;; GtkVBox
;;; 
;;; GtkVBox — A vertical container box
;;; 	
;;; Synopsis
;;; 
;;; struct       GtkVBox;
;;;
;;; GtkWidget *  gtk_vbox_new  (gboolean homogeneous, gint spacing);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBox
;;;                            +----GtkVBox
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkVBox implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; Description
;;; 
;;; A GtkVBox is a container that organizes child widgets into a single column.
;;; 
;;; Use the GtkBox packing interface to determine the arrangement, spacing,
;;; height, and alignment of GtkVBox children.
;;; 
;;; All children are allocated the same width.
;;; 
;;; GtkVBox has been deprecated. You can use GtkBox instead, which is a very
;;; quick and easy change. If you have derived your own classes from GtkVBox,
;;; you can simply change the inheritance to derive directly from GtkBox, and
;;; set the "orientation" property to GTK_ORIENTATION_VERTICAL in your instance
;;; init function, with a call like:
;;; 
;;;  1 gtk_orientable_set_orientation (GTK_ORIENTABLE (object),
;;;  2                                 GTK_ORIENTATION_VERTICAL);
;;; 
;;; If you want your code to be future-proof, the recommendation is to switch to
;;; GtkGrid, since GtkBox is going to be deprecated in favor of the more
;;; flexible grid widget eventually. For more information about migrating to
;;; GtkGrid, see Migrating from other containers to GtkGrid.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkVBox
;;; 
;;; struct GtkVBox;
;;; 
;;; Warning
;;; 
;;; GtkVBox is deprecated and should not be used in newly-written code.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkVBox" v-box
                       (:superclass box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_vbox_get_type")
                       nil)

;;; ----------------------------------------------------------------------------
;;; gtk_vbox_new ()
;;; 
;;; GtkWidget * gtk_vbox_new (gboolean homogeneous, gint spacing)
;;; 
;;; Warning
;;; 
;;; gtk_vbox_new has been deprecated since version 3.2 and should not be used in
;;; newly-written code. You can use gtk_box_new() with GTK_ORIENTATION_VERTICAL
;;; instead, wich is a very quick and easy change. But the recommendation is to
;;; switch to GtkGrid, since GtkBox is going to go away eventually. See
;;; Migrating from other containers to GtkGrid.
;;; 
;;; Creates a new GtkVBox.
;;; 
;;; homogeneous :
;;; 	TRUE if all children are to be given equal space allotments.
;;; 
;;; spacing :
;;; 	the number of pixels to place by default between children.
;;; 
;;; Returns :
;;; 	a new GtkVBox.
;;; ----------------------------------------------------------------------------

(defun v-box-new (homogeneous spacing)
  (make-instance 'v-box :homogeneous homogeneous :spacing spacing))

(export 'v-box-new)

;;; ----------------------------------------------------------------------------

