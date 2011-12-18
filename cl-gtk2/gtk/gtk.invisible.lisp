;;; ----------------------------------------------------------------------------
;;; gtk.invisible.lisp
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
;;; GtkInvisible
;;; 
;;; A widget which is not displayed
;;; 	
;;; Synopsis
;;; 
;;; struct              GtkInvisible;
;;; GtkWidget *         gtk_invisible_new                   (void);
;;; GtkWidget *         gtk_invisible_new_for_screen        (GdkScreen *screen);
;;; void                gtk_invisible_set_screen            (GtkInvisible *invisible,
;;;                                                          GdkScreen *screen);
;;; GdkScreen *         gtk_invisible_get_screen            (GtkInvisible *invisible);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkInvisible
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkInvisible implements AtkImplementorIface and GtkBuildable.
;;; Properties
;;; 
;;;   "screen"                   GdkScreen*            : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkInvisible widget is used internally in GTK+, and is probably not very useful for application developers.
;;; 
;;; It is used for reliable pointer grabs and selection handling in the code for drag-and-drop.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkInvisible
;;; 
;;; struct GtkInvisible;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkInvisible" invisible
                       (:superclass widget :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_invisible_get_type")
                       ((screen invisible-screen "screen" "GdkScreen" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_invisible_new ()
;;; 
;;; GtkWidget *         gtk_invisible_new                   (void);
;;; 
;;; Creates a new GtkInvisible.
;;; 
;;; Returns :
;;; 	a new GtkInvisible.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_invisible_new_for_screen ()
;;; 
;;; GtkWidget *         gtk_invisible_new_for_screen        (GdkScreen *screen);
;;; 
;;; Creates a new GtkInvisible object for a specified screen
;;; 
;;; screen :
;;; 	a GdkScreen which identifies on which the new GtkInvisible will be created.
;;; 
;;; Returns :
;;; 	a newly created GtkInvisible object
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_invisible_set_screen ()
;;; 
;;; void                gtk_invisible_set_screen            (GtkInvisible *invisible,
;;;                                                          GdkScreen *screen);
;;; 
;;; Sets the GdkScreen where the GtkInvisible object will be displayed.
;;; 
;;; invisible :
;;; 	a GtkInvisible.
;;; 
;;; screen :
;;; 	a GdkScreen.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_invisible_get_screen ()
;;; 
;;; GdkScreen *         gtk_invisible_get_screen            (GtkInvisible *invisible);
;;; 
;;; Returns the GdkScreen object associated with invisible
;;; 
;;; invisible :
;;; 	a GtkInvisible.
;;; 
;;; Returns :
;;; 	the associated GdkScreen. [transfer none]
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Property Details
;;; The "screen" property
;;; 
;;;   "screen"                   GdkScreen*            : Read / Write
;;; 
;;; The screen where this window will be displayed.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.invisible.lisp -----------------------------------------
