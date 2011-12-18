;;; ----------------------------------------------------------------------------
;;; gtk.link-button.lisp
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
;;; GtkLinkButton
;;; 
;;; GtkLinkButton — Create buttons bound to a URL
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;; struct              GtkLinkButton;
;;; GtkWidget *         gtk_link_button_new                 (const gchar *uri);
;;; GtkWidget *         gtk_link_button_new_with_label      (const gchar *uri,
;;;                                                          const gchar *label);
;;; const gchar *       gtk_link_button_get_uri             (GtkLinkButton *link_button);
;;; void                gtk_link_button_set_uri             (GtkLinkButton *link_button,
;;;                                                          const gchar *uri);
;;; gboolean            gtk_link_button_get_visited         (GtkLinkButton *link_button);
;;; void                gtk_link_button_set_visited         (GtkLinkButton *link_button,
;;;                                                          gboolean visited);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkButton
;;;                                  +----GtkLinkButton
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkLinkButton implements AtkImplementorIface, GtkBuildable and GtkActivatable.
;;; Properties
;;; 
;;;   "uri"                      gchar*                : Read / Write
;;;   "visited"                  gboolean              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "activate-link"                                  : Run Last
;;; 
;;; Description
;;; 
;;; A GtkLinkButton is a GtkButton with a hyperlink, similar to the one used by web browsers, which triggers an action when clicked. It is useful to show quick links to resources.
;;; 
;;; A link button is created by calling either gtk_link_button_new() or gtk_link_button_new_with_label(). If using the former, the URI you pass to the constructor is used as a label for the widget.
;;; 
;;; The URI bound to a GtkLinkButton can be set specifically using gtk_link_button_set_uri(), and retrieved using gtk_link_button_get_uri().
;;; 
;;; By default, GtkLinkButton calls gtk_show_uri() when the button is clicked. This behaviour can be overridden by connecting to the "activate-link" signal and returning TRUE from the signal handler.
;;; Details
;;; struct GtkLinkButton
;;; 
;;; struct GtkLinkButton;
;;; 
;;; The GtkLinkButton structure contains only private data and should be accessed using the provided API.
;;; gtk_link_button_new ()
;;; 
;;; GtkWidget *         gtk_link_button_new                 (const gchar *uri);
;;; 
;;; Creates a new GtkLinkButton with the URI as its text.
;;; 
;;; uri :
;;; 	a valid URI
;;; 
;;; Returns :
;;; 	a new link button widget.
;;; 
;;; Since 2.10
;;; gtk_link_button_new_with_label ()
;;; 
;;; GtkWidget *         gtk_link_button_new_with_label      (const gchar *uri,
;;;                                                          const gchar *label);
;;; 
;;; Creates a new GtkLinkButton containing a label.
;;; 
;;; uri :
;;; 	a valid URI
;;; 
;;; label :
;;; 	the text of the button. [allow-none]
;;; 
;;; Returns :
;;; 	a new link button widget. [transfer none]
;;; 
;;; Since 2.10
;;; gtk_link_button_get_uri ()
;;; 
;;; const gchar *       gtk_link_button_get_uri             (GtkLinkButton *link_button);
;;; 
;;; Retrieves the URI set using gtk_link_button_set_uri().
;;; 
;;; link_button :
;;; 	a GtkLinkButton
;;; 
;;; Returns :
;;; 	a valid URI. The returned string is owned by the link button and should not be modified or freed.
;;; 
;;; Since 2.10
;;; gtk_link_button_set_uri ()
;;; 
;;; void                gtk_link_button_set_uri             (GtkLinkButton *link_button,
;;;                                                          const gchar *uri);
;;; 
;;; Sets uri as the URI where the GtkLinkButton points. As a side-effect this unsets the 'visited' state of the button.
;;; 
;;; link_button :
;;; 	a GtkLinkButton
;;; 
;;; uri :
;;; 	a valid URI
;;; 
;;; Since 2.10
;;; gtk_link_button_get_visited ()
;;; 
;;; gboolean            gtk_link_button_get_visited         (GtkLinkButton *link_button);
;;; 
;;; Retrieves the 'visited' state of the URI where the GtkLinkButton points. The button becomes visited when it is clicked. If the URI is changed on the button, the 'visited' state is unset again.
;;; 
;;; The state may also be changed using gtk_link_button_set_visited().
;;; 
;;; link_button :
;;; 	a GtkLinkButton
;;; 
;;; Returns :
;;; 	TRUE if the link has been visited, FALSE otherwise
;;; 
;;; Since 2.14
;;; gtk_link_button_set_visited ()
;;; 
;;; void                gtk_link_button_set_visited         (GtkLinkButton *link_button,
;;;                                                          gboolean visited);
;;; 
;;; Sets the 'visited' state of the URI where the GtkLinkButton points. See gtk_link_button_get_visited() for more details.
;;; 
;;; link_button :
;;; 	a GtkLinkButton
;;; 
;;; visited :
;;; 	the new 'visited' state
;;; 
;;; Since 2.14
;;; Property Details
;;; The "uri" property
;;; 
;;;   "uri"                      gchar*                : Read / Write
;;; 
;;; The URI bound to this button.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.10
;;; The "visited" property
;;; 
;;;   "visited"                  gboolean              : Read / Write
;;; 
;;; The 'visited' state of this button. A visited link is drawn in a different color.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.14
;;; Signal Details
;;; The "activate-link" signal
;;; 
;;; gboolean            user_function                      (GtkLinkButton *button,
;;;                                                         gpointer       user_data)      : Run Last
;;; 
;;; The ::activate-link signal is emitted each time the GtkLinkButton has been clicked.
;;; 
;;; The default handler will call gtk_show_uri() with the URI stored inside the "uri" property.
;;; 
;;; To override the default behavior, you can connect to the ::activate-link signal and stop the propagation of the signal by returning TRUE from your handler.
;;; 
;;; button :
;;; 	the GtkLinkButton that emitted the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; See Also
;;; GtkButton
;;; 

(in-package :gtk)

(defvar *link-button-uri-func* nil)

(defcallback link-button-uri-func-cb :void
    ((button (g-object link-button)) (link (:string :free-from-foreign nil)) (user-data :pointer))
  (declare (ignore user-data))
  (funcall *link-button-uri-func* button link))

(defcallback link-button-uri-func-destroy-cb :void
    ((data :pointer))
  (declare (ignore data))
  (setf *link-button-uri-func* nil))

(defcfun gtk-link-button-set-uri-hook :void
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun (setf link-button-global-uri-hook) (new-value)
  (if new-value
      (gtk-link-button-set-uri-hook (callback link-button-uri-func-cb)
                                    (null-pointer)
                                    (callback link-button-uri-func-destroy-cb))
      (gtk-link-button-set-uri-hook (null-pointer)
                                    (null-pointer)
                                    (null-pointer)))
  (setf *link-button-uri-func* new-value))

(export 'link-button-global-uri-hook)
