;;; ----------------------------------------------------------------------------
;;; gtk.accel-label.lisp
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
;;; GtkAccelLabel
;;; 
;;; A label which displays an accelerator key on the right of the text
;;; 	
;;; Synopsis
;;; 
;;; struct      GtkAccelLabel;
;;;
;;; GtkWidget * gtk_accel_label_new               (const gchar *string);
;;; void        gtk_accel_label_set_accel_closure (GtkAccelLabel *accel_label,
;;;                                                GClosure *accel_closure);
;;; GtkWidget * gtk_accel_label_get_accel_widget  (GtkAccelLabel *accel_label);
;;; void        gtk_accel_label_set_accel_widget  (GtkAccelLabel *accel_label,
;;;                                                GtkWidget *accel_widget);
;;; guint       gtk_accel_label_get_accel_width   (GtkAccelLabel *accel_label);
;;; gboolean    gtk_accel_label_refetch           (GtkAccelLabel *accel_label);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkMisc
;;;                      +----GtkLabel
;;;                            +----GtkAccelLabel
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkAccelLabel implements AtkImplementorIface and GtkBuildable.
;;; Properties
;;; 
;;;   "accel-closure"            GClosure*             : Read / Write
;;;   "accel-widget"             GtkWidget*            : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkAccelLabel widget is a subclass of GtkLabel that also displays an
;;; accelerator key on the right of the label text, e.g. 'Ctl+S'. It is commonly
;;; used in menus to show the keyboard short-cuts for commands.
;;; 
;;; The accelerator key to display is not set explicitly. Instead, the
;;; GtkAccelLabel displays the accelerators which have been added to a
;;; particular widget. This widget is set by calling
;;; gtk_accel_label_set_accel_widget().
;;; 
;;; For example, a GtkMenuItem widget may have an accelerator added to emit the
;;; "activate" signal when the 'Ctl+S' key combination is pressed. A
;;; GtkAccelLabel is created and added to the GtkMenuItem, and
;;; gtk_accel_label_set_accel_widget() is called with the GtkMenuItem as the
;;; second argument. The GtkAccelLabel will now display 'Ctl+S' after its label.
;;; 
;;; Note that creating a GtkMenuItem with gtk_menu_item_new_with_label() (or
;;; one of the similar functions for GtkCheckMenuItem and GtkRadioMenuItem)
;;; automatically adds a GtkAccelLabel to the GtkMenuItem and calls
;;; gtk_accel_label_set_accel_widget() to set it up for you.
;;; 
;;; A GtkAccelLabel will only display accelerators which have GTK_ACCEL_VISIBLE
;;; set (see GtkAccelFlags). A GtkAccelLabel can display multiple accelerators
;;; and even signal names, though it is almost always used to display just one
;;; accelerator key.
;;; 
;;; Example 49. Creating a simple menu item with an accelerator key.
;;; 
;;;  1 GtkWidget *save_item;
;;;  2 GtkAccelGroup *accel_group;
;;;  3 
;;;  4 /* Create a GtkAccelGroup and add it to the window. */
;;;  5 accel_group = gtk_accel_group_new ();
;;;  6 gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);
;;;  7 
;;;  8 /* Create the menu item using the convenience function. */
;;;  9 save_item = gtk_menu_item_new_with_label ("Save");
;;; 10 gtk_widget_show (save_item);
;;; 11 gtk_container_add (GTK_CONTAINER (menu), save_item);
;;; 12
;;; 13 /* Now add the accelerator to the GtkMenuItem. Note that since we called
;;; 14    gtk_menu_item_new_with_label() to create the GtkMenuItem the
;;; 15    GtkAccelLabel is automatically set up to display the GtkMenuItem
;;; 16    accelerators. We just need to make sure we use GTK_ACCEL_VISIBLE here. */
;;; 17 gtk_widget_add_accelerator (save_item, "activate", accel_group,
;;; 18                             GDK_KEY_s, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE); 
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAccelLabel
;;; 
;;; struct GtkAccelLabel;
;;; 
;;; The GtkAccelLabel struct contains private data only, and should be accessed
;;; using the functions below.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAccelLabel" accel-label
                       (:superclass label :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_accel_label_get_type")
                       ((accel-closure accel-label-accel-closure
                         "accel-closure" "GClosure" t t)
                        (accel-widget accel-label-accel-widget "accel-widget"
                         "GtkWidget" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_new ()
;;; 
;;; GtkWidget * gtk_accel_label_new (const gchar *string)
;;; 
;;; Creates a new GtkAccelLabel.
;;; 
;;; string :
;;; 	the label string. Must be non-NULL.
;;; 
;;; Returns :
;;; 	a new GtkAccelLabel.
;;; ----------------------------------------------------------------------------

(defun accel-label-new (str)
  (make-instance 'accel-label :label str))

(export 'accel-label-new)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_set_accel_closure ()
;;; 
;;; void gtk_accel_label_set_accel_closure (GtkAccelLabel *accel_label,
;;;                                         GClosure *accel_closure)
;;; 
;;; Sets the closure to be monitored by this accelerator label. The closure
;;; must be connected to an accelerator group; see gtk_accel_group_connect().
;;; 
;;; accel_label :
;;; 	a GtkAccelLabel
;;; 
;;; accel_closure :
;;; 	the closure to monitor for accelerator changes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_get_accel_widget ()
;;; 
;;; GtkWidget * gtk_accel_label_get_accel_widget (GtkAccelLabel *accel_label)
;;; 
;;; Fetches the widget monitored by this accelerator label.
;;; See gtk_accel_label_set_accel_widget().
;;; 
;;; accel_label :
;;; 	a GtkAccelLabel
;;; 
;;; Returns :
;;; 	the object monitored by the accelerator label, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_set_accel_widget ()
;;; 
;;; void gtk_accel_label_set_accel_widget (GtkAccelLabel *accel_label,
;;;                                        GtkWidget *accel_widget)
;;; 
;;; Sets the widget to be monitored by this accelerator label.
;;; 
;;; accel_label :
;;; 	a GtkAccelLabel
;;; 
;;; accel_widget :
;;; 	the widget to be monitored.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_get_accel_width ()
;;; 
;;; guint gtk_accel_label_get_accel_width (GtkAccelLabel *accel_label)
;;; 
;;; Returns the width needed to display the accelerator key(s). This is used by
;;; menus to align all of the GtkMenuItem widgets, and shouldn't be needed by
;;; applications.
;;; 
;;; accel_label :
;;; 	a GtkAccelLabel.
;;; 
;;; Returns :
;;; 	the width needed to display the accelerator key(s).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_refetch ()
;;; 
;;; gboolean gtk_accel_label_refetch (GtkAccelLabel *accel_label)
;;; 
;;; Recreates the string representing the accelerator keys. This should not be
;;; needed since the string is automatically updated whenever accelerators are
;;; added or removed from the associated widget.
;;; 
;;; accel_label :
;;; 	a GtkAccelLabel.
;;; 
;;; Returns :
;;; 	always returns FALSE.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "accel-closure" property
;;; 
;;;   "accel-closure"            GClosure*             : Read / Write
;;; 
;;; The closure to be monitored for accelerator changes.
;;; The "accel-widget" property
;;; 
;;;   "accel-widget"             GtkWidget*            : Read / Write
;;; 
;;; The widget to be monitored for accelerator changes.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.accel-label.lisp ---------------------------------------
