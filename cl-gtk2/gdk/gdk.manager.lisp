;;; ----------------------------------------------------------------------------
;;; gdk.manager.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2 from
;;; http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 2 Reference Manual
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
;;; GdkDisplayManager
;;;
;;; GdkDisplayManager — Maintains a list of all open GdkDisplays
;;;	
;;; Synopsis
;;;
;;;                    GdkDisplayManager
;;; GdkDisplayManager* gdk_display_manager_get (void)
;;; GdkDisplay *       gdk_display_manager_get_default_display
;;;                                         (GdkDisplayManager *display_manager)
;;; void               gdk_display_manager_set_default_display
;;;                                         (GdkDisplayManager *display_manager,
;;;                                          GdkDisplay *display)
;;; GSList *           gdk_display_manager_list_displays
;;;                                         (GdkDisplayManager *display_manager)
;;; GdkDevice *        gdk_display_get_core_pointer (GdkDisplay *display)
;;; ----------------------------------------------------------------------------
;;;
;;; Object Hierarchy
;;;
;;;  GObject
;;;   +----GdkDisplayManager
;;; ----------------------------------------------------------------------------
;;;
;;; Description
;;;
;;; The purpose of the GdkDisplayManager singleton object is to offer
;;; notification when displays appear or disappear or the default display
;;; changes.
;;; ----------------------------------------------------------------------------
;;;
;;; Properties
;;;
;;; The "default-display" property
;;;
;;;  "default-display"          GdkDisplay*           : Read / Write
;;;
;;; The default display for GDK.
;;; ----------------------------------------------------------------------------
;;;
;;; Signals
;;;
;;; The "display-opened" signal
;;;
;;; void user_function (GdkDisplayManager *display_manager,
;;;                     GdkDisplay        *display,
;;;                     gpointer           user_data)            : Run Last
;;;
;;; The ::display_opened signal is emitted when a display is opened.
;;;
;;; display_manager : the object on which the signal is emitted
;;; display         : the opened display
;;; user_data       : user data set when the signal handler was connected.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkDisplayManager
;;;
;;; typedef struct _GdkDisplayManager GdkDisplayManager;
;;;
;;; The GdkDisplayManager struct has no interesting fields.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_get ()
;;;
;;; GdkDisplayManager * gdk_display_manager_get (void)
;;;
;;; Gets the singleton GdkDisplayManager object.
;;;
;;; Returns : The global GdkDisplayManager singleton; gdk_parse_pargs(),
;;;           gdk_init(), or gdk_init_check() must have been called first.
;;;           [transfer none]
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun (display-manager-get "gdk_display_manager_get")
         (g-object display-manager))

(export 'display-manager-get)

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_get_default_display ()
;;;
;;; GdkDisplay * gdk_display_manager_get_default_display
;;;                                         (GdkDisplayManager *display_manager)
;;;
;;; Gets the default GdkDisplay.
;;;
;;; display_manager : a GdkDisplayManager
;;; Returns         : a GdkDisplay, or NULL if there is no default display.
;;;                   [transfer none]
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_set_default_display ()
;;;
;;; void gdk_display_manager_set_default_display
;;;                                         (GdkDisplayManager *display_manager,
;;;                                          GdkDisplay *display)
;;;
;;; Sets display as the default display.
;;;
;;; display_manager : a GdkDisplayManager
;;; display         : a GdkDisplay
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_list_displays ()
;;;
;;; GSList * gdk_display_manager_list_displays
;;;                                         (GdkDisplayManager *display_manager)
;;;
;;; List all currently open displays.
;;;
;;; display_manager : a GdkDisplayManager
;;; Returns         : a newly allocated GSList of GdkDisplay objects. Free this
;;;                   list with g_slist_free() when you are done with it.
;;;                   [transfer container][element-type GdkDisplay]
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_core_pointer ()
;;;
;;; GdkDevice * gdk_display_get_core_pointer (GdkDisplay *display)
;;;
;;; Returns the core pointer device for the given display
;;;
;;; display : a GdkDisplay
;;; Returns : the core pointer device; this is owned by the display and should
;;;           not be freed.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***
