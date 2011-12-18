;;; ----------------------------------------------------------------------------
;;; gtk.scale.lisp
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
;;; GtkScale
;;; 
;;; GtkScale — A slider widget for selecting a value from a range
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;; struct              GtkScale;
;;; GtkWidget *         gtk_scale_new                       (GtkOrientation orientation,
;;;                                                          GtkAdjustment *adjustment);
;;; GtkWidget *         gtk_scale_new_with_range            (GtkOrientation orientation,
;;;                                                          gdouble min,
;;;                                                          gdouble max,
;;;                                                          gdouble step);
;;; void                gtk_scale_set_digits                (GtkScale *scale,
;;;                                                          gint digits);
;;; void                gtk_scale_set_draw_value            (GtkScale *scale,
;;;                                                          gboolean draw_value);
;;; void                gtk_scale_set_value_pos             (GtkScale *scale,
;;;                                                          GtkPositionType pos);
;;; gint                gtk_scale_get_digits                (GtkScale *scale);
;;; gboolean            gtk_scale_get_draw_value            (GtkScale *scale);
;;; GtkPositionType     gtk_scale_get_value_pos             (GtkScale *scale);
;;; PangoLayout *       gtk_scale_get_layout                (GtkScale *scale);
;;; void                gtk_scale_get_layout_offsets        (GtkScale *scale,
;;;                                                          gint *x,
;;;                                                          gint *y);
;;; void                gtk_scale_add_mark                  (GtkScale *scale,
;;;                                                          gdouble value,
;;;                                                          GtkPositionType position,
;;;                                                          const gchar *markup);
;;; void                gtk_scale_clear_marks               (GtkScale *scale);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkRange
;;;                      +----GtkScale
;;;                            +----GtkHScale
;;;                            +----GtkVScale
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkScale implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; Properties
;;; 
;;;   "digits"                   gint                  : Read / Write
;;;   "draw-value"               gboolean              : Read / Write
;;;   "value-pos"                GtkPositionType       : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "slider-length"            gint                  : Read
;;;   "value-spacing"            gint                  : Read
;;; 
;;; Signals
;;; 
;;;   "format-value"                                   : Run Last
;;; 
;;; Description
;;; 
;;; A GtkScale is a slider control used to select a numeric value. To use it, you'll probably want to investigate the methods on its base class, GtkRange, in addition to the methods for GtkScale itself. To set the value of a scale, you would normally use gtk_range_set_value(). To detect changes to the value, you would normally use the "value-changed" signal.
;;; 
;;; Note that using the same upper and lower bounds for the GtkScale (through the GtkRange methods) will hide the slider itself. This is useful for applications that want to show an undeterminate value on the scale, without changing the layout of the application (such as movie or music players).
;;; 
;;; GtkScale as GtkBuildable
;;; GtkScale supports a custom <marks> element, which can contain multiple <mark> elements. The "value" and "position" attributes have the same meaning as gtk_scale_add_mark() parameters of the same name. If the element is not empty, its content is taken as the markup to show at the mark. It can be translated with the usual "translatable and "context" attributes.
;;; 
;;; Details
;;; struct GtkScale
;;; 
;;; struct GtkScale;
;;; 
;;; gtk_scale_new ()
;;; 
;;; GtkWidget *         gtk_scale_new                       (GtkOrientation orientation,
;;;                                                          GtkAdjustment *adjustment);
;;; 
;;; Creates a new GtkScale.
;;; 
;;; orientation :
;;; 	the scale's orientation.
;;; 
;;; adjustment :
;;; 	the GtkAdjustment which sets the range of the scale, or NULL to create a new adjustment. [allow-none]
;;; 
;;; Returns :
;;; 	a new GtkScale
;;; 
;;; Since 3.0
;;; gtk_scale_new_with_range ()
;;; 
;;; GtkWidget *         gtk_scale_new_with_range            (GtkOrientation orientation,
;;;                                                          gdouble min,
;;;                                                          gdouble max,
;;;                                                          gdouble step);
;;; 
;;; Creates a new scale widget with the given orientation that lets the user input a number between min and max (including min and max) with the increment step. step must be nonzero; it's the distance the slider moves when using the arrow keys to adjust the scale value.
;;; 
;;; Note that the way in which the precision is derived works best if step is a power of ten. If the resulting precision is not suitable for your needs, use gtk_scale_set_digits() to correct it.
;;; 
;;; orientation :
;;; 	the scale's orientation.
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
;;; 	a new GtkScale
;;; 
;;; Since 3.0
;;; gtk_scale_set_digits ()
;;; 
;;; void                gtk_scale_set_digits                (GtkScale *scale,
;;;                                                          gint digits);
;;; 
;;; Sets the number of decimal places that are displayed in the value. Also causes the value of the adjustment to be rounded off to this number of digits, so the retrieved value matches the value the user saw.
;;; 
;;; scale :
;;; 	a GtkScale
;;; 
;;; digits :
;;; 	the number of decimal places to display, e.g. use 1 to display 1.0, 2 to display 1.00, etc
;;; gtk_scale_set_draw_value ()
;;; 
;;; void                gtk_scale_set_draw_value            (GtkScale *scale,
;;;                                                          gboolean draw_value);
;;; 
;;; Specifies whether the current value is displayed as a string next to the slider.
;;; 
;;; scale :
;;; 	a GtkScale
;;; 
;;; draw_value :
;;; 	TRUE to draw the value
;;; gtk_scale_set_value_pos ()
;;; 
;;; void                gtk_scale_set_value_pos             (GtkScale *scale,
;;;                                                          GtkPositionType pos);
;;; 
;;; Sets the position in which the current value is displayed.
;;; 
;;; scale :
;;; 	a GtkScale
;;; 
;;; pos :
;;; 	the position in which the current value is displayed
;;; gtk_scale_get_digits ()
;;; 
;;; gint                gtk_scale_get_digits                (GtkScale *scale);
;;; 
;;; Gets the number of decimal places that are displayed in the value.
;;; 
;;; scale :
;;; 	a GtkScale
;;; 
;;; Returns :
;;; 	the number of decimal places that are displayed
;;; gtk_scale_get_draw_value ()
;;; 
;;; gboolean            gtk_scale_get_draw_value            (GtkScale *scale);
;;; 
;;; Returns whether the current value is displayed as a string next to the slider.
;;; 
;;; scale :
;;; 	a GtkScale
;;; 
;;; Returns :
;;; 	whether the current value is displayed as a string
;;; gtk_scale_get_value_pos ()
;;; 
;;; GtkPositionType     gtk_scale_get_value_pos             (GtkScale *scale);
;;; 
;;; Gets the position in which the current value is displayed.
;;; 
;;; scale :
;;; 	a GtkScale
;;; 
;;; Returns :
;;; 	the position in which the current value is displayed
;;; gtk_scale_get_layout ()
;;; 
;;; PangoLayout *       gtk_scale_get_layout                (GtkScale *scale);
;;; 
;;; Gets the PangoLayout used to display the scale. The returned object is owned by the scale so does not need to be freed by the caller.
;;; 
;;; scale :
;;; 	A GtkScale
;;; 
;;; Returns :
;;; 	the PangoLayout for this scale, or NULL if the "draw-value" property is FALSE. [transfer none]
;;; 
;;; Since 2.4
;;; gtk_scale_get_layout_offsets ()
;;; 
;;; void                gtk_scale_get_layout_offsets        (GtkScale *scale,
;;;                                                          gint *x,
;;;                                                          gint *y);
;;; 
;;; Obtains the coordinates where the scale will draw the PangoLayout representing the text in the scale. Remember when using the PangoLayout function you need to convert to and from pixels using PANGO_PIXELS() or PANGO_SCALE.
;;; 
;;; If the "draw-value" property is FALSE, the return values are undefined.
;;; 
;;; scale :
;;; 	a GtkScale
;;; 
;;; x :
;;; 	location to store X offset of layout, or NULL. [out][allow-none]
;;; 
;;; y :
;;; 	location to store Y offset of layout, or NULL. [out][allow-none]
;;; 
;;; Since 2.4
;;; gtk_scale_add_mark ()
;;; 
;;; void                gtk_scale_add_mark                  (GtkScale *scale,
;;;                                                          gdouble value,
;;;                                                          GtkPositionType position,
;;;                                                          const gchar *markup);
;;; 
;;; Adds a mark at value.
;;; 
;;; A mark is indicated visually by drawing a tick mark next to the scale, and GTK+ makes it easy for the user to position the scale exactly at the marks value.
;;; 
;;; If markup is not NULL, text is shown next to the tick mark.
;;; 
;;; To remove marks from a scale, use gtk_scale_clear_marks().
;;; 
;;; scale :
;;; 	a GtkScale
;;; 
;;; value :
;;; 	the value at which the mark is placed, must be between the lower and upper limits of the scales' adjustment
;;; 
;;; position :
;;; 	where to draw the mark. For a horizontal scale, GTK_POS_TOP and GTK_POS_LEFT are drawn above the scale, anything else below. For a vertical scale, GTK_POS_LEFT and GTK_POS_TOP are drawn to the left of the scale, anything else to the right.
;;; 
;;; markup :
;;; 	Text to be shown at the mark, using Pango markup, or NULL. [allow-none]
;;; 
;;; Since 2.16
;;; gtk_scale_clear_marks ()
;;; 
;;; void                gtk_scale_clear_marks               (GtkScale *scale);
;;; 
;;; Removes any marks that have been added with gtk_scale_add_mark().
;;; 
;;; scale :
;;; 	a GtkScale
;;; 
;;; Since 2.16
;;; Property Details
;;; The "digits" property
;;; 
;;;   "digits"                   gint                  : Read / Write
;;; 
;;; The number of decimal places that are displayed in the value.
;;; 
;;; Allowed values: [G_MAXULONG,64]
;;; 
;;; Default value: 1
;;; The "draw-value" property
;;; 
;;;   "draw-value"               gboolean              : Read / Write
;;; 
;;; Whether the current value is displayed as a string next to the slider.
;;; 
;;; Default value: TRUE
;;; The "value-pos" property
;;; 
;;;   "value-pos"                GtkPositionType       : Read / Write
;;; 
;;; The position in which the current value is displayed.
;;; 
;;; Default value: GTK_POS_TOP
;;; Style Property Details
;;; The "slider-length" style property
;;; 
;;;   "slider-length"            gint                  : Read
;;; 
;;; Length of scale's slider.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 31
;;; The "value-spacing" style property
;;; 
;;;   "value-spacing"            gint                  : Read
;;; 
;;; Space between value text and the slider/trough area.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 2
;;; Signal Details
;;; The "format-value" signal
;;; 
;;; gchar*              user_function                      (GtkScale *scale,
;;;                                                         gdouble   value,
;;;                                                         gpointer  user_data)      : Run Last
;;; 
;;; Signal which allows you to change how the scale value is displayed. Connect a signal handler which returns an allocated string representing value. That string will then be used to display the scale's value.
;;; 
;;; Here's an example signal handler which displays a value 1.0 as with "-->1.0<--".
;;; 
;;; 1
;;; 2
;;; 3
;;; 4
;;; 5
;;; 6
;;; 7
;;; 
;;; 	
;;; 
;;; static gchar*
;;; format_value_callback (GtkScale *scale,
;;;                        gdouble   value)
;;; {
;;;   return g_strdup_printf ("-->%0.*g<--",
;;;                           gtk_scale_get_digits (scale), value);
;;;  }
;;; 
;;; scale :
;;; 	the object which received the signal
;;; 
;;; value :
;;; 	the value to format
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Returns :
;;; 	allocated string representing value
;;; 
