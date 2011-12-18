;;; ----------------------------------------------------------------------------
;;; gtk.check-button.lisp
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
;;; GtkCheckButton
;;; 
;;; Create widgets with a discrete toggle button
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;; struct              GtkCheckButton;
;;; GtkWidget *         gtk_check_button_new                (void);
;;; GtkWidget *         gtk_check_button_new_with_label     (const gchar *label);
;;; GtkWidget *         gtk_check_button_new_with_mnemonic  (const gchar *label);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkButton
;;;                                  +----GtkToggleButton
;;;                                        +----GtkCheckButton
;;;                                              +----GtkRadioButton
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkCheckButton implements AtkImplementorIface, GtkBuildable and GtkActivatable.
;;; Style Properties
;;; 
;;;   "indicator-size"           gint                  : Read
;;;   "indicator-spacing"        gint                  : Read
;;; 
;;; Description
;;; 
;;; A GtkCheckButton places a discrete GtkToggleButton next to a widget, (usually a GtkLabel). See the section on GtkToggleButton widgets for more information about toggle/check buttons.
;;; 
;;; The important signal ( "toggled" ) is also inherited from GtkToggleButton.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCheckButton
;;; 
;;; struct GtkCheckButton;
;;; 
;;; gtk_check_button_new ()
;;; 
;;; GtkWidget *         gtk_check_button_new                (void);
;;; 
;;; Creates a new GtkCheckButton.
;;; 
;;; Returns :
;;; 	a GtkWidget.
;;; gtk_check_button_new_with_label ()
;;; 
;;; GtkWidget *         gtk_check_button_new_with_label     (const gchar *label);
;;; 
;;; Creates a new GtkCheckButton with a GtkLabel to the right of it.
;;; 
;;; label :
;;; 	the text for the check button.
;;; 
;;; Returns :
;;; 	a GtkWidget.
;;; gtk_check_button_new_with_mnemonic ()
;;; 
;;; GtkWidget *         gtk_check_button_new_with_mnemonic  (const gchar *label);
;;; 
;;; Creates a new GtkCheckButton containing a label. The label will be created using gtk_label_new_with_mnemonic(), so underscores in label indicate the mnemonic for the check button.
;;; 
;;; label :
;;; 	The text of the button, with an underscore in front of the mnemonic character
;;; 
;;; Returns :
;;; 	a new GtkCheckButton
;;; Style Property Details
;;; The "indicator-size" style property
;;; 
;;;   "indicator-size"           gint                  : Read
;;; 
;;; Size of check or radio indicator.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 16
;;; The "indicator-spacing" style property
;;; 
;;;   "indicator-spacing"        gint                  : Read
;;; 
;;; Spacing around check or radio indicator.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 2
;;; See Also
;;; GtkCheckMenuItem, GtkButton, GtkToggleButton, GtkRadioButton 
;;; 
