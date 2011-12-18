;;; ----------------------------------------------------------------------------
;;; gtk.toggle-button.lisp
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
;;; GtkToggleButton
;;; 
;;; GtkToggleButton — Create buttons which retain their state
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;; struct              GtkToggleButton;
;;; GtkWidget *         gtk_toggle_button_new               (void);
;;; GtkWidget *         gtk_toggle_button_new_with_label    (const gchar *label);
;;; GtkWidget *         gtk_toggle_button_new_with_mnemonic (const gchar *label);
;;; void                gtk_toggle_button_set_mode          (GtkToggleButton *toggle_button,
;;;                                                          gboolean draw_indicator);
;;; gboolean            gtk_toggle_button_get_mode          (GtkToggleButton *toggle_button);
;;; void                gtk_toggle_button_toggled           (GtkToggleButton *toggle_button);
;;; gboolean            gtk_toggle_button_get_active        (GtkToggleButton *toggle_button);
;;; void                gtk_toggle_button_set_active        (GtkToggleButton *toggle_button,
;;;                                                          gboolean is_active);
;;; gboolean            gtk_toggle_button_get_inconsistent  (GtkToggleButton *toggle_button);
;;; void                gtk_toggle_button_set_inconsistent  (GtkToggleButton *toggle_button,
;;;                                                          gboolean setting);
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
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkToggleButton implements AtkImplementorIface, GtkBuildable and GtkActivatable.
;;; Properties
;;; 
;;;   "active"                   gboolean              : Read / Write
;;;   "draw-indicator"           gboolean              : Read / Write
;;;   "inconsistent"             gboolean              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "toggled"                                        : Run First
;;; 
;;; Description
;;; 
;;; A GtkToggleButton is a GtkButton which will remain 'pressed-in' when clicked. Clicking again will cause the toggle button to return to its normal state.
;;; 
;;; A toggle button is created by calling either gtk_toggle_button_new() or gtk_toggle_button_new_with_label(). If using the former, it is advisable to pack a widget, (such as a GtkLabel and/or a GtkPixmap), into the toggle button's container. (See GtkButton for more information).
;;; 
;;; The state of a GtkToggleButton can be set specifically using gtk_toggle_button_set_active(), and retrieved using gtk_toggle_button_get_active().
;;; 
;;; To simply switch the state of a toggle button, use gtk_toggle_button_toggled().
;;; 
;;; Example 54. Creating two GtkToggleButton widgets.
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
;;; 
;;; 	
;;; 
;;; void make_toggles (void) {
;;;    GtkWidget *dialog, *toggle1, *toggle2;
;;; 
;;;    dialog = gtk_dialog_new ();
;;;    toggle1 = gtk_toggle_button_new_with_label ("Hi, i'm a toggle button.");
;;; 
;;;    // Makes this toggle button invisible
;;;    gtk_toggle_button_set_mode (GTK_TOGGLE_BUTTON (toggle1), TRUE);
;;; 
;;;    g_signal_connect (toggle1, "toggled",
;;;                      G_CALLBACK (output_state), NULL);
;;;    gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->action_area),
;;;                        toggle1, FALSE, FALSE, 2);
;;; 
;;;    toggle2 = gtk_toggle_button_new_with_label ("Hi, i'm another toggle button.");
;;;    gtk_toggle_button_set_mode (GTK_TOGGLE_BUTTON (toggle2), FALSE);
;;;    g_signal_connect (toggle2, "toggled",
;;;                      G_CALLBACK (output_state), NULL);
;;;    gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->action_area),
;;;                        toggle2, FALSE, FALSE, 2);
;;; 
;;;    gtk_widget_show_all (dialog);
;;; }
;;; 
;;; 
;;; Details
;;; struct GtkToggleButton
;;; 
;;; struct GtkToggleButton;
;;; 
;;; gtk_toggle_button_new ()
;;; 
;;; GtkWidget *         gtk_toggle_button_new               (void);
;;; 
;;; Creates a new toggle button. A widget should be packed into the button, as in gtk_button_new().
;;; 
;;; Returns :
;;; 	a new toggle button.
;;; gtk_toggle_button_new_with_label ()
;;; 
;;; GtkWidget *         gtk_toggle_button_new_with_label    (const gchar *label);
;;; 
;;; Creates a new toggle button with a text label.
;;; 
;;; label :
;;; 	a string containing the message to be placed in the toggle button.
;;; 
;;; Returns :
;;; 	a new toggle button.
;;; gtk_toggle_button_new_with_mnemonic ()
;;; 
;;; GtkWidget *         gtk_toggle_button_new_with_mnemonic (const gchar *label);
;;; 
;;; Creates a new GtkToggleButton containing a label. The label will be created using gtk_label_new_with_mnemonic(), so underscores in label indicate the mnemonic for the button.
;;; 
;;; label :
;;; 	the text of the button, with an underscore in front of the mnemonic character
;;; 
;;; Returns :
;;; 	a new GtkToggleButton
;;; gtk_toggle_button_set_mode ()
;;; 
;;; void                gtk_toggle_button_set_mode          (GtkToggleButton *toggle_button,
;;;                                                          gboolean draw_indicator);
;;; 
;;; Sets whether the button is displayed as a separate indicator and label. You can call this function on a checkbutton or a radiobutton with draw_indicator = FALSE to make the button look like a normal button
;;; 
;;; This function only affects instances of classes like GtkCheckButton and GtkRadioButton that derive from GtkToggleButton, not instances of GtkToggleButton itself.
;;; 
;;; toggle_button :
;;; 	a GtkToggleButton
;;; 
;;; draw_indicator :
;;; 	if TRUE, draw the button as a separate indicator and label; if FALSE, draw the button like a normal button
;;; gtk_toggle_button_get_mode ()
;;; 
;;; gboolean            gtk_toggle_button_get_mode          (GtkToggleButton *toggle_button);
;;; 
;;; Retrieves whether the button is displayed as a separate indicator and label. See gtk_toggle_button_set_mode().
;;; 
;;; toggle_button :
;;; 	a GtkToggleButton
;;; 
;;; Returns :
;;; 	TRUE if the togglebutton is drawn as a separate indicator and label.
;;; gtk_toggle_button_toggled ()
;;; 
;;; void                gtk_toggle_button_toggled           (GtkToggleButton *toggle_button);
;;; 
;;; Emits the "toggled" signal on the GtkToggleButton. There is no good reason for an application ever to call this function.
;;; 
;;; toggle_button :
;;; 	a GtkToggleButton.
;;; gtk_toggle_button_get_active ()
;;; 
;;; gboolean            gtk_toggle_button_get_active        (GtkToggleButton *toggle_button);
;;; 
;;; Queries a GtkToggleButton and returns its current state. Returns TRUE if the toggle button is pressed in and FALSE if it is raised.
;;; 
;;; toggle_button :
;;; 	a GtkToggleButton.
;;; 
;;; Returns :
;;; 	a gboolean value.
;;; gtk_toggle_button_set_active ()
;;; 
;;; void                gtk_toggle_button_set_active        (GtkToggleButton *toggle_button,
;;;                                                          gboolean is_active);
;;; 
;;; Sets the status of the toggle button. Set to TRUE if you want the GtkToggleButton to be 'pressed in', and FALSE to raise it. This action causes the toggled signal to be emitted.
;;; 
;;; toggle_button :
;;; 	a GtkToggleButton.
;;; 
;;; is_active :
;;; 	TRUE or FALSE.
;;; gtk_toggle_button_get_inconsistent ()
;;; 
;;; gboolean            gtk_toggle_button_get_inconsistent  (GtkToggleButton *toggle_button);
;;; 
;;; Gets the value set by gtk_toggle_button_set_inconsistent().
;;; 
;;; toggle_button :
;;; 	a GtkToggleButton
;;; 
;;; Returns :
;;; 	TRUE if the button is displayed as inconsistent, FALSE otherwise
;;; gtk_toggle_button_set_inconsistent ()
;;; 
;;; void                gtk_toggle_button_set_inconsistent  (GtkToggleButton *toggle_button,
;;;                                                          gboolean setting);
;;; 
;;; If the user has selected a range of elements (such as some text or spreadsheet cells) that are affected by a toggle button, and the current values in that range are inconsistent, you may want to display the toggle in an "in between" state. This function turns on "in between" display. Normally you would turn off the inconsistent state again if the user toggles the toggle button. This has to be done manually, gtk_toggle_button_set_inconsistent() only affects visual appearance, it doesn't affect the semantics of the button.
;;; 
;;; toggle_button :
;;; 	a GtkToggleButton
;;; 
;;; setting :
;;; 	TRUE if state is inconsistent
;;; Property Details
;;; The "active" property
;;; 
;;;   "active"                   gboolean              : Read / Write
;;; 
;;; If the toggle button should be pressed in.
;;; 
;;; Default value: FALSE
;;; The "draw-indicator" property
;;; 
;;;   "draw-indicator"           gboolean              : Read / Write
;;; 
;;; If the toggle part of the button is displayed.
;;; 
;;; Default value: FALSE
;;; The "inconsistent" property
;;; 
;;;   "inconsistent"             gboolean              : Read / Write
;;; 
;;; If the toggle button is in an "in between" state.
;;; 
;;; Default value: FALSE
;;; Signal Details
;;; The "toggled" signal
;;; 
;;; void                user_function                      (GtkToggleButton *togglebutton,
;;;                                                         gpointer         user_data)         : Run First
;;; 
;;; Should be connected if you wish to perform an action whenever the GtkToggleButton's state is changed.
;;; 
;;; togglebutton :
;;; 	the object which received the signal.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; See Also
;;; GtkButton, GtkCheckButton, GtkCheckMenuItem 
;;; 
