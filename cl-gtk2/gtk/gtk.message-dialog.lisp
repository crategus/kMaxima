;;; ----------------------------------------------------------------------------
;;; gtk.message-dialog.lisp
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
;;; GtkMessageDialog
;;; 
;;; A convenient message window
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;; struct              GtkMessageDialog;
;;; enum                GtkMessageType;
;;; enum                GtkButtonsType;
;;; GtkWidget *         gtk_message_dialog_new              (GtkWindow *parent,
;;;                                                          GtkDialogFlags flags,
;;;                                                          GtkMessageType type,
;;;                                                          GtkButtonsType buttons,
;;;                                                          const gchar *message_format,
;;;                                                          ...);
;;; GtkWidget *         gtk_message_dialog_new_with_markup  (GtkWindow *parent,
;;;                                                          GtkDialogFlags flags,
;;;                                                          GtkMessageType type,
;;;                                                          GtkButtonsType buttons,
;;;                                                          const gchar *message_format,
;;;                                                          ...);
;;; void                gtk_message_dialog_set_markup       (GtkMessageDialog *message_dialog,
;;;                                                          const gchar *str);
;;; void                gtk_message_dialog_set_image        (GtkMessageDialog *dialog,
;;;                                                          GtkWidget *image);
;;; GtkWidget *         gtk_message_dialog_get_image        (GtkMessageDialog *dialog);
;;; void                gtk_message_dialog_format_secondary_text
;;;                                                         (GtkMessageDialog *message_dialog,
;;;                                                          const gchar *message_format,
;;;                                                          ...);
;;; void                gtk_message_dialog_format_secondary_markup
;;;                                                         (GtkMessageDialog *message_dialog,
;;;                                                          const gchar *message_format,
;;;                                                          ...);
;;; GtkWidget *         gtk_message_dialog_get_message_area (GtkMessageDialog *message_dialog);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkWindow
;;;                                  +----GtkDialog
;;;                                        +----GtkMessageDialog
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkMessageDialog implements AtkImplementorIface and GtkBuildable.
;;; Properties
;;; 
;;;   "buttons"                  GtkButtonsType        : Write / Construct Only
;;;   "image"                    GtkWidget*            : Read / Write
;;;   "message-area"             GtkWidget*            : Read
;;;   "message-type"             GtkMessageType        : Read / Write / Construct
;;;   "secondary-text"           gchar*                : Read / Write
;;;   "secondary-use-markup"     gboolean              : Read / Write
;;;   "text"                     gchar*                : Read / Write
;;;   "use-markup"               gboolean              : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "message-border"           gint                  : Read
;;; 
;;; Description
;;; 
;;; GtkMessageDialog presents a dialog with an image representing the type of message (Error, Question, etc.) alongside some message text. It's simply a convenience widget; you could construct the equivalent of GtkMessageDialog from GtkDialog without too much effort, but GtkMessageDialog saves typing.
;;; 
;;; The easiest way to do a modal message dialog is to use gtk_dialog_run(), though you can also pass in the GTK_DIALOG_MODAL flag, gtk_dialog_run() automatically makes the dialog modal and waits for the user to respond to it. gtk_dialog_run() returns when any dialog button is clicked.
;;; 
;;; Example 46. A modal dialog.
;;; 
;;;  1 dialog = gtk_message_dialog_new (main_application_window,
;;;  2                                  GTK_DIALOG_DESTROY_WITH_PARENT,
;;;  3                                  GTK_MESSAGE_ERROR,
;;;  4                                  GTK_BUTTONS_CLOSE,
;;;  5                                  "Error loading file '%s': %s",
;;;  6                                  filename, g_strerror (errno));
;;;  7 gtk_dialog_run (GTK_DIALOG (dialog));
;;;  8 gtk_widget_destroy (dialog);
;;; 
;;; You might do a non-modal GtkMessageDialog as follows:
;;; 
;;; Example 47. A non-modal dialog.
;;; 
;;;  1 dialog = gtk_message_dialog_new (main_application_window,
;;;  2                                  GTK_DIALOG_DESTROY_WITH_PARENT,
;;;  3                                  GTK_MESSAGE_ERROR,
;;;  4                                  GTK_BUTTONS_CLOSE,
;;;  5                                  "Error loading file '%s': %s",
;;;  6                                  filename, g_strerror (errno));
;;;  7
;;;  8 /* Destroy the dialog when the user responds to it (e.g. clicks a button) */
;;;  9 g_signal_connect_swapped (dialog, "response",
;;; 10                           G_CALLBACK (gtk_widget_destroy),
;;; 11                           dialog);
;;; 
;;; 
;;; GtkMessageDialog as GtkBuildable
;;; 
;;; The GtkMessageDialog implementation of the GtkBuildable interface exposes
;;; the message area as an internal child with the name "message_area".
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMessageDialog
;;; 
;;; struct GtkMessageDialog;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMessageDialog" message-dialog
                       (:superclass dialog :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_message_dialog_get_type")
                       ((buttons message-dialog-buttons "buttons"
                         "GtkButtonsType" nil nil)
                        (image message-dialog-image "image" "GtkWidget" t t)
                        (message-type message-dialog-message-type
                         "message-type" "GtkMessageType" t t)
                        (secondary-text message-dialog-secondary-text
                         "secondary-text" "gchararray" t t)
                        (secondary-use-markup
                         message-dialog-secondary-use-markup
                         "secondary-use-markup" "gboolean" t t)
                        (text message-dialog-text "text" "gchararray" t t)
                        (use-markup message-dialog-use-markup "use-markup"
                         "gboolean" t t)))

;;; ---------------------------------------------------------------------------- 
;;; enum GtkMessageType
;;; 
;;; typedef enum {
;;;   GTK_MESSAGE_INFO,
;;;   GTK_MESSAGE_WARNING,
;;;   GTK_MESSAGE_QUESTION,
;;;   GTK_MESSAGE_ERROR,
;;;   GTK_MESSAGE_OTHER
;;; } GtkMessageType;
;;; 
;;; The type of message being displayed in the dialog.
;;; 
;;; GTK_MESSAGE_INFO
;;; 	Informational message
;;; 
;;; GTK_MESSAGE_WARNING
;;; 	Nonfatal warning message
;;; 
;;; GTK_MESSAGE_QUESTION
;;; 	Question requiring a choice
;;; 
;;; GTK_MESSAGE_ERROR
;;; 	Fatal error message
;;; 
;;; GTK_MESSAGE_OTHER
;;; 	None of the above, doesn't get an icon
;;; enum GtkButtonsType
;;; 
;;; typedef enum {
;;;   GTK_BUTTONS_NONE,
;;;   GTK_BUTTONS_OK,
;;;   GTK_BUTTONS_CLOSE,
;;;   GTK_BUTTONS_CANCEL,
;;;   GTK_BUTTONS_YES_NO,
;;;   GTK_BUTTONS_OK_CANCEL
;;; } GtkButtonsType;
;;; 
;;; Prebuilt sets of buttons for the dialog. If none of these choices are
;;; appropriate, simply use GTK_BUTTONS_NONE then call gtk_dialog_add_buttons().
;;;
;;; Note
;;;
;;; Please note that GTK_BUTTONS_OK, GTK_BUTTONS_YES_NO and
;;; GTK_BUTTONS_OK_CANCEL are discouraged by the GNOME HIG.
;;; 
;;; GTK_BUTTONS_NONE
;;; 	no buttons at all
;;; 
;;; GTK_BUTTONS_OK
;;; 	an OK button
;;; 
;;; GTK_BUTTONS_CLOSE
;;; 	a Close button
;;; 
;;; GTK_BUTTONS_CANCEL
;;; 	a Cancel button
;;; 
;;; GTK_BUTTONS_YES_NO
;;; 	Yes and No buttons
;;; 
;;; GTK_BUTTONS_OK_CANCEL
;;; 	OK and Cancel buttons
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_new ()
;;; 
;;; GtkWidget * gtk_message_dialog_new (GtkWindow *parent,
;;;                                     GtkDialogFlags flags,
;;;                                     GtkMessageType type,
;;;                                     GtkButtonsType buttons,
;;;                                     const gchar *message_format,
;;;                                     ...);
;;; 
;;; Creates a new message dialog, which is a simple dialog with an icon
;;; indicating the dialog type (error, warning, etc.) and some text the user
;;; may want to see. When the user clicks a button a "response" signal is
;;; emitted with response IDs from GtkResponseType. See GtkDialog for more
;;; details.
;;; 
;;; parent :
;;; 	transient parent, or NULL for none. [allow-none]
;;; 
;;; flags :
;;; 	flags
;;; 
;;; type :
;;; 	type of message
;;; 
;;; buttons :
;;; 	set of buttons to use
;;; 
;;; message_format :
;;; 	printf()-style format string, or NULL. [allow-none]
;;; 
;;; ... :
;;; 	arguments for message_format
;;; 
;;; Returns :
;;; 	a new GtkMessageDialog. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_new_with_markup ()
;;; 
;;; GtkWidget * gtk_message_dialog_new_with_markup (GtkWindow *parent,
;;;                                                 GtkDialogFlags flags,
;;;                                                 GtkMessageType type,
;;;                                                 GtkButtonsType buttons,
;;;                                                 const gchar *message_format,
;;;                                                 ...);
;;; 
;;; Creates a new message dialog, which is a simple dialog with an icon
;;; indicating the dialog type (error, warning, etc.) and some text which is
;;; marked up with the Pango text markup language. When the user clicks a button
;;; a "response" signal is emitted with response IDs from GtkResponseType. See
;;; GtkDialog for more details.
;;; 
;;; Special XML characters in the printf() arguments passed to this function
;;; will automatically be escaped as necessary. (See g_markup_printf_escaped()
;;; for how this is implemented.) Usually this is what you want, but if you have
;;; an existing Pango markup string that you want to use literally as the label,
;;; then you need to use gtk_message_dialog_set_markup() instead, since you
;;; can't pass the markup string either as the format (it might contain '%'
;;; characters) or as a string argument.
;;; 
;;;  1 GtkWidget *dialog;
;;;  2 dialog = gtk_message_dialog_new (main_application_window,
;;;  3                                  GTK_DIALOG_DESTROY_WITH_PARENT,
;;;  4                                  GTK_MESSAGE_ERROR,
;;;  5                                  GTK_BUTTONS_CLOSE,
;;;  6                                  NULL);
;;;  7 gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (dialog),
;;;  8                                markup);
;;; 
;;; parent :
;;; 	transient parent, or NULL for none. [allow-none]
;;; 
;;; flags :
;;; 	flags
;;; 
;;; type :
;;; 	type of message
;;; 
;;; buttons :
;;; 	set of buttons to use
;;; 
;;; message_format :
;;; 	printf()-style format string, or NULL. [allow-none]
;;; 
;;; ... :
;;; 	arguments for message_format
;;; 
;;; Returns :
;;; 	a new GtkMessageDialog
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_set_markup ()
;;; 
;;; void gtk_message_dialog_set_markup (GtkMessageDialog *message_dialog,
;;;                                     const gchar *str);
;;; 
;;; Sets the text of the message dialog to be str, which is marked up with the
;;; Pango text markup language.
;;; 
;;; message_dialog :
;;; 	a GtkMessageDialog
;;; 
;;; str :
;;; 	markup string (see Pango markup format)
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_set_image ()
;;; 
;;; void gtk_message_dialog_set_image (GtkMessageDialog *dialog,
;;;                                    GtkWidget *image);
;;; 
;;; Sets the dialog's image to image.
;;; 
;;; dialog :
;;; 	a GtkMessageDialog
;;; 
;;; image :
;;; 	the image
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_get_image ()
;;; 
;;; GtkWidget * gtk_message_dialog_get_image (GtkMessageDialog *dialog);
;;; 
;;; Gets the dialog's image.
;;; 
;;; dialog :
;;; 	a GtkMessageDialog
;;; 
;;; Returns :
;;; 	the dialog's image. [transfer none]
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_format_secondary_text ()
;;; 
;;; void gtk_message_dialog_format_secondary_text
;;;                                           (GtkMessageDialog *message_dialog,
;;;                                            const gchar *message_format,
;;;                                            ...);
;;; 
;;; Sets the secondary text of the message dialog to be message_format (with
;;; printf()-style).
;;; 
;;; Note that setting a secondary text makes the primary text become bold,
;;; unless you have provided explicit markup.
;;; 
;;; message_dialog :
;;; 	a GtkMessageDialog
;;; 
;;; message_format :
;;; 	printf()-style format string, or NULL. [allow-none]
;;; 
;;; ... :
;;; 	arguments for message_format
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_format_secondary_markup ()
;;; 
;;; void gtk_message_dialog_format_secondary_markup
;;;                                           (GtkMessageDialog *message_dialog,
;;;                                            const gchar *message_format,
;;;                                            ...);
;;; 
;;; Sets the secondary text of the message dialog to be message_format (with
;;; printf()-style), which is marked up with the Pango text markup language.
;;; 
;;; Note that setting a secondary text makes the primary text become bold,
;;; unless you have provided explicit markup.
;;; 
;;; Due to an oversight, this function does not escape special XML characters
;;; like gtk_message_dialog_new_with_markup() does. Thus, if the arguments may
;;; contain special XML characters, you should use g_markup_printf_escaped() to
;;; escape it.
;;; 
;;;  1 gchar *msg;
;;;  2 
;;;  3 msg = g_markup_printf_escaped (message_format, ...);
;;;  4 gtk_message_dialog_format_secondary_markup (message_dialog, "%s", msg);
;;;  5 g_free (msg);
;;; 
;;; message_dialog :
;;; 	a GtkMessageDialog
;;; 
;;; message_format :
;;; 	printf()-style markup string (see Pango markup format), or NULL
;;; 
;;; ... :
;;; 	arguments for message_format
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_get_message_area ()
;;; 
;;; GtkWidget * gtk_message_dialog_get_message_area
;;;                                           (GtkMessageDialog *message_dialog)
;;; 
;;; Returns the message area of the dialog. This is the box where the dialog's
;;; primary and secondary labels are packed. You can add your own extra content
;;; to that box and it will appear below those labels, on the right side of the
;;; dialog's image (or on the left for right-to-left languages). See
;;; gtk_dialog_get_content_area() for the corresponding function in the parent
;;; GtkDialog.
;;; 
;;; message_dialog :
;;; 	a GtkMessageDialog
;;; 
;;; Returns :
;;; 	A GtkVBox corresponding to the "message area" in the message_dialog.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "buttons" property
;;; 
;;;   "buttons" GtkButtonsType                          : Write / Construct Only
;;; 
;;; The buttons shown in the message dialog.
;;; 
;;; Default value: GTK_BUTTONS_NONE
;;;
;;; ----------------------------------------------------------------------------
;;; The "image" property
;;; 
;;;   "image"                    GtkWidget*             : Read / Write
;;; 
;;; The image for this dialog.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "message-area" property
;;; 
;;;   "message-area"             GtkWidget*            : Read
;;; 
;;; The GtkVBox that corresponds to the message area of this dialog.
;;; See gtk_message_dialog_get_message_area() for a detailed description of
;;; this area.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "message-type" property
;;; 
;;;   "message-type"             GtkMessageType       : Read / Write / Construct
;;; 
;;; The type of the message. The type is used to determine the image that is
;;; shown in the dialog, unless the image is explicitly set by the ::image
;;; property.
;;; 
;;; Default value: GTK_MESSAGE_INFO
;;;
;;; ----------------------------------------------------------------------------
;;; The "secondary-text" property
;;; 
;;;   "secondary-text"           gchar*                : Read / Write
;;; 
;;; The secondary text of the message dialog.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.10
;;; The "secondary-use-markup" property
;;; 
;;;   "secondary-use-markup"     gboolean              : Read / Write
;;; 
;;; TRUE if the secondary text of the dialog includes Pango markup.
;;; See pango_parse_markup().
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "text" property
;;; 
;;;   "text"                     gchar*                : Read / Write
;;; 
;;; The primary text of the message dialog. If the dialog has a secondary text,
;;; this will appear as the title.
;;; 
;;; Default value: ""
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-markup" property
;;; 
;;;   "use-markup"               gboolean              : Read / Write
;;; 
;;; TRUE if the primary text of the dialog includes Pango markup.
;;; See pango_parse_markup().
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; Style Property Details
;;;
;;; The "message-border" style property
;;; 
;;;   "message-border"           gint                  : Read
;;; 
;;; Width of border around the label and image in the message dialog.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 12
;;; See Also
;;; GtkDialog
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.message-dialog.lisp ------------------------------------
