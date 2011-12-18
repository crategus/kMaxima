;;; ----------------------------------------------------------------------------
;;; gtk.scale-button.lisp
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
;;; GtkScaleButton
;;; 
;;; GtkScaleButton — A button which pops up a scale
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;; struct              GtkScaleButton;
;;; GtkWidget *         gtk_scale_button_new                (GtkIconSize size,
;;;                                                          gdouble min,
;;;                                                          gdouble max,
;;;                                                          gdouble step,
;;;                                                          const gchar **icons);
;;; void                gtk_scale_button_set_adjustment     (GtkScaleButton *button,
;;;                                                          GtkAdjustment *adjustment);
;;; void                gtk_scale_button_set_icons          (GtkScaleButton *button,
;;;                                                          const gchar **icons);
;;; void                gtk_scale_button_set_value          (GtkScaleButton *button,
;;;                                                          gdouble value);
;;; GtkAdjustment *     gtk_scale_button_get_adjustment     (GtkScaleButton *button);
;;; gdouble             gtk_scale_button_get_value          (GtkScaleButton *button);
;;; GtkWidget *         gtk_scale_button_get_popup          (GtkScaleButton *button);
;;; GtkWidget *         gtk_scale_button_get_plus_button    (GtkScaleButton *button);
;;; GtkWidget *         gtk_scale_button_get_minus_button   (GtkScaleButton *button);
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
;;; GtkScaleButton implements AtkImplementorIface, GtkBuildable, GtkActivatable and GtkOrientable.
;;; Properties
;;; 
;;;   "adjustment"               GtkAdjustment*        : Read / Write
;;;   "icons"                    GStrv                 : Read / Write
;;;   "size"                     GtkIconSize           : Read / Write
;;;   "value"                    gdouble               : Read / Write
;;; 
;;; Signals
;;; 
;;;   "popdown"                                        : Action
;;;   "popup"                                          : Action
;;;   "value-changed"                                  : Run Last
;;; 
;;; Description
;;; 
;;; GtkScaleButton provides a button which pops up a scale widget. This kind of widget is commonly used for volume controls in multimedia applications, and GTK+ provides a GtkVolumeButton subclass that is tailored for this use case.
;;; Details
;;; struct GtkScaleButton
;;; 
;;; struct GtkScaleButton;
;;; 
;;; gtk_scale_button_new ()
;;; 
;;; GtkWidget *         gtk_scale_button_new                (GtkIconSize size,
;;;                                                          gdouble min,
;;;                                                          gdouble max,
;;;                                                          gdouble step,
;;;                                                          const gchar **icons);
;;; 
;;; Creates a GtkScaleButton, with a range between min and max, with a stepping of step.
;;; 
;;; size :
;;; 	a stock icon size. [type int]
;;; 
;;; min :
;;; 	the minimum value of the scale (usually 0)
;;; 
;;; max :
;;; 	the maximum value of the scale (usually 100)
;;; 
;;; step :
;;; 	the stepping of value when a scroll-wheel event, or up/down arrow event occurs (usually 2)
;;; 
;;; icons :
;;; 	a NULL-terminated array of icon names, or NULL if you want to set the list later with gtk_scale_button_set_icons(). [allow-none][array zero-terminated=1]
;;; 
;;; Returns :
;;; 	a new GtkScaleButton
;;; 
;;; Since 2.12
;;; gtk_scale_button_set_adjustment ()
;;; 
;;; void                gtk_scale_button_set_adjustment     (GtkScaleButton *button,
;;;                                                          GtkAdjustment *adjustment);
;;; 
;;; Sets the GtkAdjustment to be used as a model for the GtkScaleButton's scale. See gtk_range_set_adjustment() for details.
;;; 
;;; button :
;;; 	a GtkScaleButton
;;; 
;;; adjustment :
;;; 	a GtkAdjustment
;;; 
;;; Since 2.12
;;; gtk_scale_button_set_icons ()
;;; 
;;; void                gtk_scale_button_set_icons          (GtkScaleButton *button,
;;;                                                          const gchar **icons);
;;; 
;;; Sets the icons to be used by the scale button. For details, see the "icons" property.
;;; 
;;; button :
;;; 	a GtkScaleButton
;;; 
;;; icons :
;;; 	a NULL-terminated array of icon names. [array zero-terminated=1]
;;; 
;;; Since 2.12
;;; gtk_scale_button_set_value ()
;;; 
;;; void                gtk_scale_button_set_value          (GtkScaleButton *button,
;;;                                                          gdouble value);
;;; 
;;; Sets the current value of the scale; if the value is outside the minimum or maximum range values, it will be clamped to fit inside them. The scale button emits the "value-changed" signal if the value changes.
;;; 
;;; button :
;;; 	a GtkScaleButton
;;; 
;;; value :
;;; 	new value of the scale button
;;; 
;;; Since 2.12
;;; gtk_scale_button_get_adjustment ()
;;; 
;;; GtkAdjustment *     gtk_scale_button_get_adjustment     (GtkScaleButton *button);
;;; 
;;; Gets the GtkAdjustment associated with the GtkScaleButton's scale. See gtk_range_get_adjustment() for details.
;;; 
;;; button :
;;; 	a GtkScaleButton
;;; 
;;; Returns :
;;; 	the adjustment associated with the scale. [transfer none]
;;; 
;;; Since 2.12
;;; gtk_scale_button_get_value ()
;;; 
;;; gdouble             gtk_scale_button_get_value          (GtkScaleButton *button);
;;; 
;;; Gets the current value of the scale button.
;;; 
;;; button :
;;; 	a GtkScaleButton
;;; 
;;; Returns :
;;; 	current value of the scale button
;;; 
;;; Since 2.12
;;; gtk_scale_button_get_popup ()
;;; 
;;; GtkWidget *         gtk_scale_button_get_popup          (GtkScaleButton *button);
;;; 
;;; Retrieves the popup of the GtkScaleButton.
;;; 
;;; button :
;;; 	a GtkScaleButton
;;; 
;;; Returns :
;;; 	the popup of the GtkScaleButton. [transfer none]
;;; 
;;; Since 2.14
;;; gtk_scale_button_get_plus_button ()
;;; 
;;; GtkWidget *         gtk_scale_button_get_plus_button    (GtkScaleButton *button);
;;; 
;;; Retrieves the plus button of the GtkScaleButton.
;;; 
;;; button :
;;; 	a GtkScaleButton
;;; 
;;; Returns :
;;; 	the plus button of the GtkScaleButton. [transfer none]
;;; 
;;; Since 2.14
;;; gtk_scale_button_get_minus_button ()
;;; 
;;; GtkWidget *         gtk_scale_button_get_minus_button   (GtkScaleButton *button);
;;; 
;;; Retrieves the minus button of the GtkScaleButton.
;;; 
;;; button :
;;; 	a GtkScaleButton
;;; 
;;; Returns :
;;; 	the minus button of the GtkScaleButton. [transfer none]
;;; 
;;; Since 2.14
;;; Property Details
;;; The "adjustment" property
;;; 
;;;   "adjustment"               GtkAdjustment*        : Read / Write
;;; 
;;; The GtkAdjustment that contains the current value of this scale button object.
;;; The "icons" property
;;; 
;;;   "icons"                    GStrv                 : Read / Write
;;; 
;;; The names of the icons to be used by the scale button. The first item in the array will be used in the button when the current value is the lowest value, the second item for the highest value. All the subsequent icons will be used for all the other values, spread evenly over the range of values.
;;; 
;;; If there's only one icon name in the icons array, it will be used for all the values. If only two icon names are in the icons array, the first one will be used for the bottom 50% of the scale, and the second one for the top 50%.
;;; 
;;; It is recommended to use at least 3 icons so that the GtkScaleButton reflects the current value of the scale better for the users.
;;; 
;;; Since 2.12
;;; The "size" property
;;; 
;;;   "size"                     GtkIconSize           : Read / Write
;;; 
;;; The icon size.
;;; 
;;; Default value: GTK_ICON_SIZE_SMALL_TOOLBAR
;;; The "value" property
;;; 
;;;   "value"                    gdouble               : Read / Write
;;; 
;;; The value of the scale.
;;; 
;;; Default value: 0
;;; Signal Details
;;; The "popdown" signal
;;; 
;;; void                user_function                      (GtkScaleButton *button,
;;;                                                         gpointer        user_data)      : Action
;;; 
;;; The ::popdown signal is a keybinding signal which gets emitted to popdown the scale widget.
;;; 
;;; The default binding for this signal is Escape.
;;; 
;;; button :
;;; 	the object which received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Since 2.12
;;; The "popup" signal
;;; 
;;; void                user_function                      (GtkScaleButton *button,
;;;                                                         gpointer        user_data)      : Action
;;; 
;;; The ::popup signal is a keybinding signal which gets emitted to popup the scale widget.
;;; 
;;; The default bindings for this signal are Space, Enter and Return.
;;; 
;;; button :
;;; 	the object which received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Since 2.12
;;; The "value-changed" signal
;;; 
;;; void                user_function                      (GtkScaleButton *button,
;;;                                                         gdouble         value,
;;;                                                         gpointer        user_data)      : Run Last
;;; 
;;; The ::value-changed signal is emitted when the value field has changed.
;;; 
;;; button :
;;; 	the object which received the signal
;;; 
;;; value :
;;; 	the new value
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Since 2.12

(in-package :gtk)

(defcfun (scale-button-popup "gtk_scale_button_get_popup") (g-object widget)
  (scale-button (g-object scale-button)))

(defcfun (scale-button-plus-button "gtk_scale_button_get_plus_button") (g-object widget)
  (scale-button (g-object scale-button)))

(defcfun (scale-button-minus-button "gtk_scale_button_get_minus_button") (g-object widget)
  (scale-button (g-object scale-button)))

(export 'scale-button-popup)
(export 'scale-button-plus-button)
(export 'scale-button-minus-button)
