;;; ----------------------------------------------------------------------------
;;; gtk.h-scale.lisp
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
;;; GtkHScale
;;; 
;;; GtkHScale — A horizontal slider widget for selecting a value from a range
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;; struct              GtkHScale;
;;; GtkWidget *         gtk_hscale_new                      (GtkAdjustment *adjustment);
;;; GtkWidget *         gtk_hscale_new_with_range           (gdouble min,
;;;                                                          gdouble max,
;;;                                                          gdouble step);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkRange
;;;                      +----GtkScale
;;;                            +----GtkHScale
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkHScale implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; Description
;;; 
;;; The GtkHScale widget is used to allow the user to select a value using a horizontal slider. To create one, use gtk_hscale_new_with_range().
;;; 
;;; The position to show the current value, and the number of decimal places shown can be set using the parent GtkScale class's functions.
;;; 
;;; GtkHScale has been deprecated, use GtkScale instead.
;;; Details
;;; struct GtkHScale
;;; 
;;; struct GtkHScale;
;;; 
;;; Warning
;;; 
;;; GtkHScale is deprecated and should not be used in newly-written code.
;;; gtk_hscale_new ()
;;; 
;;; GtkWidget *         gtk_hscale_new                      (GtkAdjustment *adjustment);
;;; 
;;; Warning
;;; 
;;; gtk_hscale_new has been deprecated since version 3.2 and should not be used in newly-written code. Use gtk_scale_new() with GTK_ORIENTATION_HORIZONTAL instead
;;; 
;;; Creates a new GtkHScale.
;;; 
;;; adjustment :
;;; 	the GtkAdjustment which sets the range of the scale.
;;; 
;;; Returns :
;;; 	a new GtkHScale.
;;; gtk_hscale_new_with_range ()
;;; 
;;; GtkWidget *         gtk_hscale_new_with_range           (gdouble min,
;;;                                                          gdouble max,
;;;                                                          gdouble step);
;;; 
;;; Warning
;;; 
;;; gtk_hscale_new_with_range has been deprecated since version 3.2 and should not be used in newly-written code. Use gtk_scale_new_with_range() with GTK_ORIENTATION_HORIZONTAL instead
;;; 
;;; Creates a new horizontal scale widget that lets the user input a number between min and max (including min and max) with the increment step. step must be nonzero; it's the distance the slider moves when using the arrow keys to adjust the scale value.
;;; 
;;; Note that the way in which the precision is derived works best if step is a power of ten. If the resulting precision is not suitable for your needs, use gtk_scale_set_digits() to correct it.
;;; 
;;; min :
;;; 	minimum value
;;; 
;;; max :
;;; 	maximum value
;;; 
;;; step :
;;; 	step increment (tick size) used with keyboard shortcuts
;;; 
;;; Returns :
;;; 	a new GtkHScale
;;; 
