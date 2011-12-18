;;; ----------------------------------------------------------------------------
;;; gtk.spin-button.lisp
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
;;; GtkSpinButton
;;; 
;;; GtkSpinButton — Retrieve an integer or floating-point number from the user
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;; struct              GtkSpinButton;
;;; enum                GtkSpinButtonUpdatePolicy;
;;; enum                GtkSpinType;
;;; void                gtk_spin_button_configure           (GtkSpinButton *spin_button,
;;;                                                          GtkAdjustment *adjustment,
;;;                                                          gdouble climb_rate,
;;;                                                          guint digits);
;;; GtkWidget *         gtk_spin_button_new                 (GtkAdjustment *adjustment,
;;;                                                          gdouble climb_rate,
;;;                                                          guint digits);
;;; GtkWidget *         gtk_spin_button_new_with_range      (gdouble min,
;;;                                                          gdouble max,
;;;                                                          gdouble step);
;;; void                gtk_spin_button_set_adjustment      (GtkSpinButton *spin_button,
;;;                                                          GtkAdjustment *adjustment);
;;; GtkAdjustment *     gtk_spin_button_get_adjustment      (GtkSpinButton *spin_button);
;;; void                gtk_spin_button_set_digits          (GtkSpinButton *spin_button,
;;;                                                          guint digits);
;;; void                gtk_spin_button_set_increments      (GtkSpinButton *spin_button,
;;;                                                          gdouble step,
;;;                                                          gdouble page);
;;; void                gtk_spin_button_set_range           (GtkSpinButton *spin_button,
;;;                                                          gdouble min,
;;;                                                          gdouble max);
;;; gint                gtk_spin_button_get_value_as_int    (GtkSpinButton *spin_button);
;;; void                gtk_spin_button_set_value           (GtkSpinButton *spin_button,
;;;                                                          gdouble value);
;;; void                gtk_spin_button_set_update_policy   (GtkSpinButton *spin_button,
;;;                                                          GtkSpinButtonUpdatePolicy policy);
;;; void                gtk_spin_button_set_numeric         (GtkSpinButton *spin_button,
;;;                                                          gboolean numeric);
;;; void                gtk_spin_button_spin                (GtkSpinButton *spin_button,
;;;                                                          GtkSpinType direction,
;;;                                                          gdouble increment);
;;; void                gtk_spin_button_set_wrap            (GtkSpinButton *spin_button,
;;;                                                          gboolean wrap);
;;; void                gtk_spin_button_set_snap_to_ticks   (GtkSpinButton *spin_button,
;;;                                                          gboolean snap_to_ticks);
;;; void                gtk_spin_button_update              (GtkSpinButton *spin_button);
;;; guint               gtk_spin_button_get_digits          (GtkSpinButton *spin_button);
;;; void                gtk_spin_button_get_increments      (GtkSpinButton *spin_button,
;;;                                                          gdouble *step,
;;;                                                          gdouble *page);
;;; gboolean            gtk_spin_button_get_numeric         (GtkSpinButton *spin_button);
;;; void                gtk_spin_button_get_range           (GtkSpinButton *spin_button,
;;;                                                          gdouble *min,
;;;                                                          gdouble *max);
;;; gboolean            gtk_spin_button_get_snap_to_ticks   (GtkSpinButton *spin_button);
;;; GtkSpinButtonUpdatePolicy gtk_spin_button_get_update_policy
;;;                                                         (GtkSpinButton *spin_button);
;;; gdouble             gtk_spin_button_get_value           (GtkSpinButton *spin_button);
;;; gboolean            gtk_spin_button_get_wrap            (GtkSpinButton *spin_button);
;;; #define             GTK_INPUT_ERROR
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkEntry
;;;                      +----GtkSpinButton
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkSpinButton implements AtkImplementorIface, GtkBuildable, GtkEditable and GtkCellEditable.
;;; Properties
;;; 
;;;   "adjustment"               GtkAdjustment*        : Read / Write
;;;   "climb-rate"               gdouble               : Read / Write
;;;   "digits"                   guint                 : Read / Write
;;;   "numeric"                  gboolean              : Read / Write
;;;   "snap-to-ticks"            gboolean              : Read / Write
;;;   "update-policy"            GtkSpinButtonUpdatePolicy  : Read / Write
;;;   "value"                    gdouble               : Read / Write
;;;   "wrap"                     gboolean              : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "shadow-type"              GtkShadowType         : Read
;;; 
;;; Signals
;;; 
;;;   "change-value"                                   : Action
;;;   "input"                                          : Run Last
;;;   "output"                                         : Run Last
;;;   "value-changed"                                  : Run Last
;;;   "wrapped"                                        : Run Last
;;; 
;;; Description
;;; 
;;; A GtkSpinButton is an ideal way to allow the user to set the value of some attribute. Rather than having to directly type a number into a GtkEntry, GtkSpinButton allows the user to click on one of two arrows to increment or decrement the displayed value. A value can still be typed in, with the bonus that it can be checked to ensure it is in a given range.
;;; 
;;; The main properties of a GtkSpinButton are through an adjustment. See the GtkAdjustment section for more details about an adjustment's properties.
;;; 
;;; Example 55. Using a GtkSpinButton to get an integer
;;; 
;;; 1
;;; 2
;;; 3
;;; 4
;;; 5
;;; 6
;;; 7
;;; 8
;;; 9
;;; 10
;;; 11
;;; 12
;;; 13
;;; 14
;;; 15
;;; 16
;;; 17
;;; 18
;;; 19
;;; 20
;;; 21
;;; 22
;;; 23
;;; 24
;;; 25
;;; 26
;;; 27
;;; 28
;;; 29
;;; 30
;;; 
;;; 	
;;; 
;;; /* Provides a function to retrieve an integer value from a
;;;  * GtkSpinButton and creates a spin button to model percentage
;;;  * values.
;;;  */
;;; 
;;; gint
;;; grab_int_value (GtkSpinButton *button,
;;;                 gpointer       user_data)
;;; {
;;;   return gtk_spin_button_get_value_as_int (button);
;;; }
;;; 
;;; void
;;; create_integer_spin_button (void)
;;; {
;;; 
;;;   GtkWidget *window, *button;
;;;   GtkAdjustment *adjustment;
;;; 
;;;   adjustment = gtk_adjustment_new (50.0, 0.0, 100.0, 1.0, 5.0, 0.0);
;;; 
;;;   window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;;;   gtk_container_set_border_width (GTK_CONTAINER (window), 5);
;;; 
;;;   /* creates the spinbutton, with no decimal places */
;;;   button = gtk_spin_button_new (adjustment, 1.0, 0);
;;;   gtk_container_add (GTK_CONTAINER (window), button);
;;; 
;;;   gtk_widget_show_all (window);
;;; }
;;; 
;;; 
;;; Example 56. Using a GtkSpinButton to get a floating point value
;;; 
;;; 1
;;; 2
;;; 3
;;; 4
;;; 5
;;; 6
;;; 7
;;; 8
;;; 9
;;; 10
;;; 11
;;; 12
;;; 13
;;; 14
;;; 15
;;; 16
;;; 17
;;; 18
;;; 19
;;; 20
;;; 21
;;; 22
;;; 23
;;; 24
;;; 25
;;; 26
;;; 27
;;; 28
;;; 
;;; 	
;;; 
;;; /* Provides a function to retrieve a floating point value from a
;;;  * GtkSpinButton, and creates a high precision spin button.
;;;  */
;;; 
;;; gfloat
;;; grab_float_value (GtkSpinButton *button,
;;;                   gpointer       user_data)
;;; {
;;;   return gtk_spin_button_get_value (button);
;;; }
;;; 
;;; void
;;; create_floating_spin_button (void)
;;; {
;;;   GtkWidget *window, *button;
;;;   GtkAdjustment *adjustment;
;;; 
;;;   adjustment = gtk_adjustment_new (2.500, 0.0, 5.0, 0.001, 0.1, 0.0);
;;; 
;;;   window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;;;   gtk_container_set_border_width (GTK_CONTAINER (window), 5);
;;; 
;;;   /* creates the spinbutton, with three decimal places */
;;;   button = gtk_spin_button_new (adjustment, 0.001, 3);
;;;   gtk_container_add (GTK_CONTAINER (window), button);
;;; 
;;;   gtk_widget_show_all (window);
;;; }
;;; 
;;; 
;;; Details
;;; struct GtkSpinButton
;;; 
;;; struct GtkSpinButton;
;;; 
;;; The GtkSpinButton struct contains only private data and should not be directly modified.
;;; enum GtkSpinButtonUpdatePolicy
;;; 
;;; typedef enum {
;;;   GTK_UPDATE_ALWAYS,
;;;   GTK_UPDATE_IF_VALID
;;; } GtkSpinButtonUpdatePolicy;
;;; 
;;; The spin button update policy determines whether the spin button displays values even if they are outside the bounds of its adjustment. See gtk_spin_button_set_update_policy().
;;; 
;;; GTK_UPDATE_ALWAYS
;;; 	When refreshing your GtkSpinButton, the value is always displayed
;;; 
;;; GTK_UPDATE_IF_VALID
;;; 	When refreshing your GtkSpinButton, the value is only displayed if it is valid within the bounds of the spin button's adjustment
;;; enum GtkSpinType
;;; 
;;; typedef enum {
;;;   GTK_SPIN_STEP_FORWARD,
;;;   GTK_SPIN_STEP_BACKWARD,
;;;   GTK_SPIN_PAGE_FORWARD,
;;;   GTK_SPIN_PAGE_BACKWARD,
;;;   GTK_SPIN_HOME,
;;;   GTK_SPIN_END,
;;;   GTK_SPIN_USER_DEFINED
;;; } GtkSpinType;
;;; 
;;; The values of the GtkSpinType enumeration are used to specify the change to make in gtk_spin_button_spin().
;;; 
;;; GTK_SPIN_STEP_FORWARD
;;; 	Increment by the adjustments step increment.
;;; 
;;; GTK_SPIN_STEP_BACKWARD
;;; 	Decrement by the adjustments step increment.
;;; 
;;; GTK_SPIN_PAGE_FORWARD
;;; 	Increment by the adjustments page increment.
;;; 
;;; GTK_SPIN_PAGE_BACKWARD
;;; 	Decrement by the adjustments page increment.
;;; 
;;; GTK_SPIN_HOME
;;; 	Go to the adjustments lower bound.
;;; 
;;; GTK_SPIN_END
;;; 	Go to the adjustments upper bound.
;;; 
;;; GTK_SPIN_USER_DEFINED
;;; 	Change by a specified amount.
;;; gtk_spin_button_configure ()
;;; 
;;; void                gtk_spin_button_configure           (GtkSpinButton *spin_button,
;;;                                                          GtkAdjustment *adjustment,
;;;                                                          gdouble climb_rate,
;;;                                                          guint digits);
;;; 
;;; Changes the properties of an existing spin button. The adjustment, climb rate, and number of decimal places are all changed accordingly, after this function call.
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; adjustment :
;;; 	a GtkAdjustment. [allow-none]
;;; 
;;; climb_rate :
;;; 	the new climb rate
;;; 
;;; digits :
;;; 	the number of decimal places to display in the spin button
;;; gtk_spin_button_new ()
;;; 
;;; GtkWidget *         gtk_spin_button_new                 (GtkAdjustment *adjustment,
;;;                                                          gdouble climb_rate,
;;;                                                          guint digits);
;;; 
;;; Creates a new GtkSpinButton.
;;; 
;;; adjustment :
;;; 	the GtkAdjustment object that this spin button should use, or NULL. [allow-none]
;;; 
;;; climb_rate :
;;; 	specifies how much the spin button changes when an arrow is clicked on
;;; 
;;; digits :
;;; 	the number of decimal places to display
;;; 
;;; Returns :
;;; 	The new spin button as a GtkWidget
;;; gtk_spin_button_new_with_range ()
;;; 
;;; GtkWidget *         gtk_spin_button_new_with_range      (gdouble min,
;;;                                                          gdouble max,
;;;                                                          gdouble step);
;;; 
;;; This is a convenience constructor that allows creation of a numeric GtkSpinButton without manually creating an adjustment. The value is initially set to the minimum value and a page increment of 10 * step is the default. The precision of the spin button is equivalent to the precision of step.
;;; 
;;; Note that the way in which the precision is derived works best if step is a power of ten. If the resulting precision is not suitable for your needs, use gtk_spin_button_set_digits() to correct it.
;;; 
;;; min :
;;; 	Minimum allowable value
;;; 
;;; max :
;;; 	Maximum allowable value
;;; 
;;; step :
;;; 	Increment added or subtracted by spinning the widget
;;; 
;;; Returns :
;;; 	The new spin button as a GtkWidget
;;; gtk_spin_button_set_adjustment ()
;;; 
;;; void                gtk_spin_button_set_adjustment      (GtkSpinButton *spin_button,
;;;                                                          GtkAdjustment *adjustment);
;;; 
;;; Replaces the GtkAdjustment associated with spin_button.
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; adjustment :
;;; 	a GtkAdjustment to replace the existing adjustment
;;; gtk_spin_button_get_adjustment ()
;;; 
;;; GtkAdjustment *     gtk_spin_button_get_adjustment      (GtkSpinButton *spin_button);
;;; 
;;; Get the adjustment associated with a GtkSpinButton
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; Returns :
;;; 	the GtkAdjustment of spin_button. [transfer none]
;;; gtk_spin_button_set_digits ()
;;; 
;;; void                gtk_spin_button_set_digits          (GtkSpinButton *spin_button,
;;;                                                          guint digits);
;;; 
;;; Set the precision to be displayed by spin_button. Up to 20 digit precision is allowed.
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; digits :
;;; 	the number of digits after the decimal point to be displayed for the spin button's value
;;; gtk_spin_button_set_increments ()
;;; 
;;; void                gtk_spin_button_set_increments      (GtkSpinButton *spin_button,
;;;                                                          gdouble step,
;;;                                                          gdouble page);
;;; 
;;; Sets the step and page increments for spin_button. This affects how quickly the value changes when the spin button's arrows are activated.
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; step :
;;; 	increment applied for a button 1 press.
;;; 
;;; page :
;;; 	increment applied for a button 2 press.
;;; gtk_spin_button_set_range ()
;;; 
;;; void                gtk_spin_button_set_range           (GtkSpinButton *spin_button,
;;;                                                          gdouble min,
;;;                                                          gdouble max);
;;; 
;;; Sets the minimum and maximum allowable values for spin_button.
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; min :
;;; 	minimum allowable value
;;; 
;;; max :
;;; 	maximum allowable value
;;; gtk_spin_button_get_value_as_int ()
;;; 
;;; gint                gtk_spin_button_get_value_as_int    (GtkSpinButton *spin_button);
;;; 
;;; Get the value spin_button represented as an integer.
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; Returns :
;;; 	the value of spin_button
;;; gtk_spin_button_set_value ()
;;; 
;;; void                gtk_spin_button_set_value           (GtkSpinButton *spin_button,
;;;                                                          gdouble value);
;;; 
;;; Sets the value of spin_button.
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; value :
;;; 	the new value
;;; gtk_spin_button_set_update_policy ()
;;; 
;;; void                gtk_spin_button_set_update_policy   (GtkSpinButton *spin_button,
;;;                                                          GtkSpinButtonUpdatePolicy policy);
;;; 
;;; Sets the update behavior of a spin button. This determines wether the spin button is always updated or only when a valid value is set.
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; policy :
;;; 	a GtkSpinButtonUpdatePolicy value
;;; gtk_spin_button_set_numeric ()
;;; 
;;; void                gtk_spin_button_set_numeric         (GtkSpinButton *spin_button,
;;;                                                          gboolean numeric);
;;; 
;;; Sets the flag that determines if non-numeric text can be typed into the spin button.
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; numeric :
;;; 	flag indicating if only numeric entry is allowed
;;; gtk_spin_button_spin ()
;;; 
;;; void                gtk_spin_button_spin                (GtkSpinButton *spin_button,
;;;                                                          GtkSpinType direction,
;;;                                                          gdouble increment);
;;; 
;;; Increment or decrement a spin button's value in a specified direction by a specified amount.
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; direction :
;;; 	a GtkSpinType indicating the direction to spin
;;; 
;;; increment :
;;; 	step increment to apply in the specified direction
;;; gtk_spin_button_set_wrap ()
;;; 
;;; void                gtk_spin_button_set_wrap            (GtkSpinButton *spin_button,
;;;                                                          gboolean wrap);
;;; 
;;; Sets the flag that determines if a spin button value wraps around to the opposite limit when the upper or lower limit of the range is exceeded.
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; wrap :
;;; 	a flag indicating if wrapping behavior is performed
;;; gtk_spin_button_set_snap_to_ticks ()
;;; 
;;; void                gtk_spin_button_set_snap_to_ticks   (GtkSpinButton *spin_button,
;;;                                                          gboolean snap_to_ticks);
;;; 
;;; Sets the policy as to whether values are corrected to the nearest step increment when a spin button is activated after providing an invalid value.
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; snap_to_ticks :
;;; 	a flag indicating if invalid values should be corrected
;;; gtk_spin_button_update ()
;;; 
;;; void                gtk_spin_button_update              (GtkSpinButton *spin_button);
;;; 
;;; Manually force an update of the spin button.
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; gtk_spin_button_get_digits ()
;;; 
;;; guint               gtk_spin_button_get_digits          (GtkSpinButton *spin_button);
;;; 
;;; Fetches the precision of spin_button. See gtk_spin_button_set_digits().
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; Returns :
;;; 	the current precision
;;; gtk_spin_button_get_increments ()
;;; 
;;; void                gtk_spin_button_get_increments      (GtkSpinButton *spin_button,
;;;                                                          gdouble *step,
;;;                                                          gdouble *page);
;;; 
;;; Gets the current step and page the increments used by spin_button. See gtk_spin_button_set_increments().
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; step :
;;; 	location to store step increment, or NULL. [out][allow-none]
;;; 
;;; page :
;;; 	location to store page increment, or NULL. [out][allow-none]
;;; gtk_spin_button_get_numeric ()
;;; 
;;; gboolean            gtk_spin_button_get_numeric         (GtkSpinButton *spin_button);
;;; 
;;; Returns whether non-numeric text can be typed into the spin button. See gtk_spin_button_set_numeric().
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; Returns :
;;; 	TRUE if only numeric text can be entered
;;; gtk_spin_button_get_range ()
;;; 
;;; void                gtk_spin_button_get_range           (GtkSpinButton *spin_button,
;;;                                                          gdouble *min,
;;;                                                          gdouble *max);
;;; 
;;; Gets the range allowed for spin_button. See gtk_spin_button_set_range().
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; min :
;;; 	location to store minimum allowed value, or NULL. [out][allow-none]
;;; 
;;; max :
;;; 	location to store maximum allowed value, or NULL. [out][allow-none]
;;; gtk_spin_button_get_snap_to_ticks ()
;;; 
;;; gboolean            gtk_spin_button_get_snap_to_ticks   (GtkSpinButton *spin_button);
;;; 
;;; Returns whether the values are corrected to the nearest step. See gtk_spin_button_set_snap_to_ticks().
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; Returns :
;;; 	TRUE if values are snapped to the nearest step
;;; gtk_spin_button_get_update_policy ()
;;; 
;;; GtkSpinButtonUpdatePolicy gtk_spin_button_get_update_policy
;;;                                                         (GtkSpinButton *spin_button);
;;; 
;;; Gets the update behavior of a spin button. See gtk_spin_button_set_update_policy().
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; Returns :
;;; 	the current update policy
;;; gtk_spin_button_get_value ()
;;; 
;;; gdouble             gtk_spin_button_get_value           (GtkSpinButton *spin_button);
;;; 
;;; Get the value in the spin_button.
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; Returns :
;;; 	the value of spin_button
;;; gtk_spin_button_get_wrap ()
;;; 
;;; gboolean            gtk_spin_button_get_wrap            (GtkSpinButton *spin_button);
;;; 
;;; Returns whether the spin button's value wraps around to the opposite limit when the upper or lower limit of the range is exceeded. See gtk_spin_button_set_wrap().
;;; 
;;; spin_button :
;;; 	a GtkSpinButton
;;; 
;;; Returns :
;;; 	TRUE if the spin button wraps around
;;; GTK_INPUT_ERROR
;;; 
;;; #define GTK_INPUT_ERROR -1
;;; 
;;; Constant to return from a signal handler for the "input" signal in case of conversion failure.
;;; Property Details
;;; The "adjustment" property
;;; 
;;;   "adjustment"               GtkAdjustment*        : Read / Write
;;; 
;;; The adjustment that holds the value of the spin button.
;;; The "climb-rate" property
;;; 
;;;   "climb-rate"               gdouble               : Read / Write
;;; 
;;; The acceleration rate when you hold down a button.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;; The "digits" property
;;; 
;;;   "digits"                   guint                 : Read / Write
;;; 
;;; The number of decimal places to display.
;;; 
;;; Allowed values: <= 20
;;; 
;;; Default value: 0
;;; The "numeric" property
;;; 
;;;   "numeric"                  gboolean              : Read / Write
;;; 
;;; Whether non-numeric characters should be ignored.
;;; 
;;; Default value: FALSE
;;; The "snap-to-ticks" property
;;; 
;;;   "snap-to-ticks"            gboolean              : Read / Write
;;; 
;;; Whether erroneous values are automatically changed to a spin button's nearest step increment.
;;; 
;;; Default value: FALSE
;;; The "update-policy" property
;;; 
;;;   "update-policy"            GtkSpinButtonUpdatePolicy  : Read / Write
;;; 
;;; Whether the spin button should update always, or only when the value is legal.
;;; 
;;; Default value: GTK_UPDATE_ALWAYS
;;; The "value" property
;;; 
;;;   "value"                    gdouble               : Read / Write
;;; 
;;; Reads the current value, or sets a new value.
;;; 
;;; Default value: 0
;;; The "wrap" property
;;; 
;;;   "wrap"                     gboolean              : Read / Write
;;; 
;;; Whether a spin button should wrap upon reaching its limits.
;;; 
;;; Default value: FALSE
;;; Style Property Details
;;; The "shadow-type" style property
;;; 
;;;   "shadow-type"              GtkShadowType         : Read
;;; 
;;; Style of bevel around the spin button.
;;; 
;;; Default value: GTK_SHADOW_IN
;;; Signal Details
;;; The "change-value" signal
;;; 
;;; void                user_function                      (GtkSpinButton *spinbutton,
;;;                                                         GtkScrollType  arg1,
;;;                                                         gpointer       user_data)       : Action
;;; 
;;; The "input" signal
;;; 
;;; gint                user_function                      (GtkSpinButton *spin_button,
;;;                                                         gpointer       new_value,
;;;                                                         gpointer       user_data)        : Run Last
;;; 
;;; The ::input signal can be used to influence the conversion of the users input into a double value. The signal handler is expected to use gtk_entry_get_text() to retrieve the text of the entry and set new_value to the new value.
;;; 
;;; The default conversion uses g_strtod().
;;; 
;;; spin_button :
;;; 	the object on which the signal was emitted
;;; 
;;; new_value :
;;; 	return location for the new value. [out][type double]
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Returns :
;;; 	TRUE for a successful conversion, FALSE if the input was not handled, and GTK_INPUT_ERROR if the conversion failed.
;;; The "output" signal
;;; 
;;; gboolean            user_function                      (GtkSpinButton *spin_button,
;;;                                                         gpointer       user_data)        : Run Last
;;; 
;;; The ::output signal can be used to change to formatting of the value that is displayed in the spin buttons entry.
;;; 
;;; 1
;;; 2
;;; 3
;;; 4
;;; 5
;;; 6
;;; 7
;;; 8
;;; 9
;;; 10
;;; 11
;;; 12
;;; 13
;;; 14
;;; 15
;;; 16
;;; 
;;; 	
;;; 
;;; /* show leading zeros */
;;; static gboolean
;;; on_output (GtkSpinButton *spin,
;;;            gpointer       data)
;;; {
;;;    GtkAdjustment *adjustment;
;;;    gchar *text;
;;;    int value;
;;;    adjustment = gtk_spin_button_get_adjustment (spin);
;;;    value = (int)gtk_adjustment_get_value (adjustment);
;;;    text = g_strdup_printf ("%02d", value);
;;;    gtk_entry_set_text (GTK_ENTRY (spin), text);
;;;    g_free (text);
;;; 
;;;    return TRUE;
;;; }
;;; 
;;; spin_button :
;;; 	the object which received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Returns :
;;; 	TRUE if the value has been displayed
;;; The "value-changed" signal
;;; 
;;; void                user_function                      (GtkSpinButton *spinbutton,
;;;                                                         gpointer       user_data)       : Run Last
;;; 
;;; The "wrapped" signal
;;; 
;;; void                user_function                      (GtkSpinButton *spinbutton,
;;;                                                         gpointer       user_data)       : Run Last
;;; 
;;; The wrapped signal is emitted right after the spinbutton wraps from its maximum to minimum value or vice-versa.
;;; 
;;; spinbutton :
;;; 	the object which received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;; See Also
;;; GtkEntry
;;; 
(in-package :gtk)

(define-g-enum "GtkSpinType" spin-type ()
  (:step-forward 0)
  (:step-backward 1) (:page-forward 2) (:page-backward 3)
  (:home 4) (:end 5) (:user-defined 6))

(defcfun (spin-button-spin "gtk_spin_button_spin") :void
  (spin-button (g-object spin-button))
  (direction spin-type)
  (increment :double))

(export 'spin-button-spin)

(defcfun (spin-button-update "gtk_spin_button_update") :void
  (spin-button (g-object spin-button)))

(export 'spin-button-update)
