;;; ----------------------------------------------------------------------------
;;; gtk.volume-button.lisp
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
;;; GtkVolumeButton
;;; 
;;; A button which pops up a volume control
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;; struct              GtkVolumeButton;
;;; GtkWidget *         gtk_volume_button_new               (void);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkButton
;;;                                  +----GtkScaleButton
;;;                                        +----GtkVolumeButton
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkVolumeButton implements AtkImplementorIface, GtkBuildable, GtkActivatable and GtkOrientable.
;;; Properties
;;; 
;;;   "use-symbolic"             gboolean              : Read / Write
;;; 
;;; Description
;;; 
;;; GtkVolumeButton is a subclass of GtkScaleButton that has been tailored for use as a volume control widget with suitable icons, tooltips and accessible labels.
;;; Details
;;; struct GtkVolumeButton
;;; 
;;; struct GtkVolumeButton;
;;; 
;;; gtk_volume_button_new ()
;;; 
;;; GtkWidget *         gtk_volume_button_new               (void);
;;; 
;;; Creates a GtkVolumeButton, with a range between 0.0 and 1.0, with a stepping of 0.02. Volume values can be obtained and modified using the functions from GtkScaleButton.
;;; 
;;; Returns :
;;; 	a new GtkVolumeButton
;;; 
;;; Since 2.12
;;; Property Details
;;; The "use-symbolic" property
;;; 
;;;   "use-symbolic"             gboolean              : Read / Write
;;; 
;;; Whether to use symbolic icons as the icons. Note that if the symbolic icons are not available in your installed theme, then the normal (potentially colorful) icons will be used.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 3.0
;;; 
