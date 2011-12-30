;;; ----------------------------------------------------------------------------
;;; gtk.window-group.lisp
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
;;; GtkWindowGroup
;;; 
;;; Limit the effect of grabs
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;; struct              GtkWindowGroup;
;;; GtkWindowGroup *    gtk_window_group_new                (void);
;;; void                gtk_window_group_add_window         (GtkWindowGroup *window_group,
;;;                                                          GtkWindow *window);
;;; void                gtk_window_group_remove_window      (GtkWindowGroup *window_group,
;;;                                                          GtkWindow *window);
;;; GList *             gtk_window_group_list_windows       (GtkWindowGroup *window_group);
;;; GtkWidget *         gtk_window_group_get_current_grab   (GtkWindowGroup *window_group);
;;; GtkWidget *         gtk_window_group_get_current_device_grab
;;;                                                         (GtkWindowGroup *window_group,
;;;                                                          GdkDevice *device);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkWindowGroup
;;; 
;;; Description
;;; 
;;; GtkWindowGroup objects are referenced by each window in the group, so once
;;; you have added all windows to a GtkWindowGroup, you can drop the initial
;;; reference to the window group with g_object_unref(). If the windows in the
;;; window group are subsequently destroyed, then they will be removed from the
;;; window group and drop their references on the window group; when all window
;;; have been removed, the window group will be freed.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkWindowGroup
;;; 
;;; struct GtkWindowGroup;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_new ()
;;; 
;;; GtkWindowGroup * gtk_window_group_new (void)
;;; 
;;; Creates a new GtkWindowGroup object. Grabs added with gtk_grab_add() only
;;; affect windows within the same GtkWindowGroup.
;;; 
;;; Returns :
;;; 	a new GtkWindowGroup.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_add_window ()
;;; 
;;; void gtk_window_group_add_window (GtkWindowGroup *window_group,
;;;                                   GtkWindow *window)
;;; 
;;; Adds a window to a GtkWindowGroup.
;;; 
;;; window_group :
;;; 	a GtkWindowGroup
;;; 
;;; window :
;;; 	the GtkWindow to add
;;; ----------------------------------------------------------------------------

(defcfun (gtk-window-group-add-window "gtk_window_group_add_window") :void
  (window-group (g-object window-group))
  (window (g-object gtk-window)))

(export 'gtk-window-group-add-window)

(defcfun (window-group-add-window "gtk_window_group_add_window") :void
  (window-group (g-object window-group))
  (window (g-object gtk-window)))

(export 'window-group-add-window)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_remove_window ()
;;; 
;;; void gtk_window_group_remove_window (GtkWindowGroup *window_group,
;;;                                      GtkWindow *window)
;;; 
;;; Removes a window from a GtkWindowGroup.
;;; 
;;; window_group :
;;; 	a GtkWindowGroup
;;; 
;;; window :
;;; 	the GtkWindow to remove
;;; ----------------------------------------------------------------------------

(defcfun (window-group-remove-window "gtk_window_group_remove_window") :void
  (window-group (g-object window-group))
  (window (g-object gtk-window)))

(export 'window-group-remove-window)

(defcfun (gtk-window-group-remove-window "gtk_window_group_remove_window") :void
  (window-group (g-object window-group))
  (window (g-object gtk-window)))

(export 'gtk-window-group-remove-window)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_list_windows ()
;;; 
;;; GList * gtk_window_group_list_windows (GtkWindowGroup *window_group)
;;; 
;;; Returns a list of the GtkWindows that belong to window_group.
;;; 
;;; window_group :
;;; 	a GtkWindowGroup
;;; 
;;; Returns :
;;; 	A newly-allocated list of windows inside the group.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun (gtk-window-group-list-windows "gtk_window_group_list_windows")
    (g-list gtk-window)
  (window-group (g-object window-group)))

(export 'gtk-window-group-list-windows)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_get_current_grab ()
;;; 
;;; GtkWidget* gtk_window_group_get_current_grab (GtkWindowGroup *window_group)
;;; 
;;; Gets the current grab widget of the given group, see gtk_grab_add().
;;; 
;;; window_group :
;;; 	a GtkWindowGroup
;;; 
;;; Returns :
;;; 	the current grab widget of the group. [transfer none]
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_get_current_device_grab ()
;;; 
;;; GtkWidget * gtk_window_group_get_current_device_grab
;;;                                               (GtkWindowGroup *window_group,
;;;                                                GdkDevice *device)
;;; 
;;; Returns the current grab widget for device, or NULL if none.
;;; 
;;; window_group :
;;; 	a GtkWindowGroup
;;; 
;;; device :
;;; 	a GdkDevice
;;; 
;;; Returns :
;;; 	The grab widget, or NULL. [transfer none]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------
