;;; ----------------------------------------------------------------------------
;;; gobject.events.lisp
;;;
;;; Copyright (C) 2009, 2011 Kalyanov Dmitry
;;; Copyright (C) 2011, 2012 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; See http://www.gtk.org
;;; ----------------------------------------------------------------------------
;;;
;;; License
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
;;; Events
;;; 
;;; Functions for handling events from the window system
;;; 	
;;; Synopsis
;;; 
;;; enum                GdkEventType; --> gdk.event-structures.lisp
;;; enum                GdkEventMask;
;;;
;;; #define             GDK_CURRENT_TIME
;;; #define             GDK_PRIORITY_EVENTS
;;; #define             GDK_PRIORITY_REDRAW
;;; 
;;; gboolean            gdk_events_pending                  (void);
;;; GdkEvent *          gdk_event_peek                      (void);
;;; GdkEvent *          gdk_event_get                       (void);
;;; GdkEvent *          gdk_event_get_graphics_expose       (GdkWindow *window);
;;; void                gdk_event_put                       (const GdkEvent *event);
;;; GdkEvent *          gdk_event_new                       (GdkEventType type);
;;; GdkEvent *          gdk_event_copy                      (const GdkEvent *event);
;;; void                gdk_event_free                      (GdkEvent *event);
;;; guint32             gdk_event_get_time                  (const GdkEvent *event);
;;; gboolean            gdk_event_get_state                 (const GdkEvent *event,
;;;                                                          GdkModifierType *state);
;;; gboolean            gdk_event_get_axis                  (const GdkEvent *event,
;;;                                                          GdkAxisUse axis_use,
;;;                                                          gdouble *value);
;;; gboolean            gdk_event_get_coords                (const GdkEvent *event,
;;;                                                          gdouble *x_win,
;;;                                                          gdouble *y_win);
;;; gboolean            gdk_event_get_root_coords           (const GdkEvent *event,
;;;                                                          gdouble *x_root,
;;;                                                          gdouble *y_root);
;;; void                gdk_event_request_motions           (const GdkEventMotion *event);
;;; 
;;; void                gdk_event_handler_set               (GdkEventFunc func,
;;;                                                          gpointer data,
;;;                                                          GDestroyNotify notify);
;;; void                (*GdkEventFunc)                     (GdkEvent *event,
;;;                                                          gpointer data);
;;; 
;;; gboolean            gdk_event_send_client_message       (GdkEvent *event,
;;;                                                          GdkNativeWindow winid);
;;; gboolean            gdk_event_send_client_message_for_display
;;;                                                         (GdkDisplay *display,
;;;                                                          GdkEvent *event,
;;;                                                          GdkNativeWindow winid);
;;; void                gdk_event_send_clientmessage_toall  (GdkEvent *event);
;;; void                gdk_add_client_message_filter       (GdkAtom message_type,
;;;                                                          GdkFilterFunc func,
;;;                                                          gpointer data);
;;; 
;;; gboolean            gdk_get_show_events                 (void);
;;; void                gdk_set_show_events                 (gboolean show_events);
;;; void                gdk_event_set_screen                (GdkEvent *event,
;;;                                                          GdkScreen *screen);
;;; GdkScreen *         gdk_event_get_screen                (const GdkEvent *event);
;;; 
;;; gboolean            gdk_setting_get                     (const gchar *name,
;;;                                                          GValue *value);
;;; 
;;; Description
;;; 
;;; This section describes functions dealing with events from the window system.
;;; 
;;; In GTK+ applications the events are handled automatically in
;;; gtk_main_do_event() and passed on to the appropriate widgets, so these
;;; functions are rarely needed. Though some of the fields in the Event
;;; Structures are useful.
;;; ----------------------------------------------------------------------------

(in-package :gdk)




;;; ----------------------------------------------------------------------------
;;; enum GdkEventMask
;;; 
;;; typedef enum
;;; {
;;;   GDK_EXPOSURE_MASK		        = 1 << 1,
;;;   GDK_POINTER_MOTION_MASK           = 1 << 2,
;;;   GDK_POINTER_MOTION_HINT_MASK      = 1 << 3,
;;;   GDK_BUTTON_MOTION_MASK            = 1 << 4,
;;;   GDK_BUTTON1_MOTION_MASK           = 1 << 5,
;;;   GDK_BUTTON2_MOTION_MASK           = 1 << 6,
;;;   GDK_BUTTON3_MOTION_MASK           = 1 << 7,
;;;   GDK_BUTTON_PRESS_MASK		= 1 << 8,
;;;   GDK_BUTTON_RELEASE_MASK           = 1 << 9,
;;;   GDK_KEY_PRESS_MASK		= 1 << 10,
;;;   GDK_KEY_RELEASE_MASK		= 1 << 11,
;;;   GDK_ENTER_NOTIFY_MASK		= 1 << 12,
;;;   GDK_LEAVE_NOTIFY_MASK		= 1 << 13,
;;;   GDK_FOCUS_CHANGE_MASK		= 1 << 14,
;;;   GDK_STRUCTURE_MASK		= 1 << 15,
;;;   GDK_PROPERTY_CHANGE_MASK          = 1 << 16,
;;;   GDK_VISIBILITY_NOTIFY_MASK        = 1 << 17,
;;;   GDK_PROXIMITY_IN_MASK		= 1 << 18,
;;;   GDK_PROXIMITY_OUT_MASK            = 1 << 19,
;;;   GDK_SUBSTRUCTURE_MASK		= 1 << 20,
;;;   GDK_SCROLL_MASK                   = 1 << 21,
;;;   GDK_ALL_EVENTS_MASK		= 0x3FFFFE
;;; } GdkEventMask;
;;; 
;;; A set of bit-flags to indicate which events a window is to receive. Most
;;; of these masks map onto one or more of the GdkEventType event types above.
;;; 
;;; GDK_POINTER_MOTION_HINT_MASK is a special mask which is used to reduce the
;;; number of GDK_MOTION_NOTIFY events received. Normally a GDK_MOTION_NOTIFY
;;; event is received each time the mouse moves. However, if the application
;;; spends a lot of time processing the event (updating the display, for
;;; example), it can lag behind the position of the mouse. When using
;;; GDK_POINTER_MOTION_HINT_MASK, fewer GDK_MOTION_NOTIFY events will be sent,
;;; some of which are marked as a hint (the is_hint member is TRUE). To receive
;;; more motion events after a motion hint event, the application needs to asks
;;; for more, by calling gdk_event_request_motions().
;;; 
;;; GDK_EXPOSURE_MASK
;;; 	receive expose events
;;; 
;;; GDK_POINTER_MOTION_MASK
;;; 	receive all pointer motion events
;;; 
;;; GDK_POINTER_MOTION_HINT_MASK
;;; 	see the explanation above
;;; 
;;; GDK_BUTTON_MOTION_MASK
;;; 	receive pointer motion events while any button is pressed
;;; 
;;; GDK_BUTTON1_MOTION_MASK
;;; 	receive pointer motion events while 1 button is pressed
;;; 
;;; GDK_BUTTON2_MOTION_MASK
;;; 	receive pointer motion events while 2 button is pressed
;;; 
;;; GDK_BUTTON3_MOTION_MASK
;;; 	receive pointer motion events while 3 button is pressed
;;; 
;;; GDK_BUTTON_PRESS_MASK
;;; 	receive button press events
;;; 
;;; GDK_BUTTON_RELEASE_MASK
;;; 	receive button release events
;;; 
;;; GDK_KEY_PRESS_MASK
;;; 	receive key press events
;;; 
;;; GDK_KEY_RELEASE_MASK
;;; 	receive key release events
;;; 
;;; GDK_ENTER_NOTIFY_MASK
;;; 	receive window enter events
;;; 
;;; GDK_LEAVE_NOTIFY_MASK
;;; 	receive window leave events
;;; 
;;; GDK_FOCUS_CHANGE_MASK
;;; 	receive focus change events
;;; 
;;; GDK_STRUCTURE_MASK
;;; 	receive events about window configuration change
;;; 
;;; GDK_PROPERTY_CHANGE_MASK
;;; 	receive property change events
;;; 
;;; GDK_VISIBILITY_NOTIFY_MASK
;;; 	receive visibility change events
;;; 
;;; GDK_PROXIMITY_IN_MASK
;;; 	receive proximity in events
;;; 
;;; GDK_PROXIMITY_OUT_MASK
;;; 	receive proximity out events
;;; 
;;; GDK_SUBSTRUCTURE_MASK
;;; 	receive events about window configuration changes of child windows
;;; 
;;; GDK_SCROLL_MASK
;;; 	receive scroll events
;;; 
;;; GDK_ALL_EVENTS_MASK
;;; 	the combination of all the above event masks.
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkEventMask" event-mask ()
  (:exposure-mask 2)
  (:pointer-motion-mask 4)
  (:pointer-motion-hint-mask 8)
  (:button-motion-mask 16)
  (:button1-motion-mask 32)
  (:button2-motion-mask 64)
  (:button3-motion-mask 128)
  (:button-press-mask 256)
  (:button-release-mask 512)
  (:key-press-mask 1024)
  (:key-release-mask 2048)
  (:enter-notify-mask 4096)
  (:leave-notify-mask 8192)
  (:focus-change-mask 16384)
  (:structure-mask 32768)
  (:property-change-mask 65536)
  (:visibility-notify-mask 131072)
  (:proximity-in-mask 262144)
  (:proximity-out-mask 524288)
  (:substructure-mask 1048576)
  (:scroll-mask 2097152)
  (:all-events-mask 4194302))

;;; ----------------------------------------------------------------------------
;;; GDK_CURRENT_TIME
;;;
;;; #define GDK_CURRENT_TIME     0L
;;; 
;;; Represents the current time, and can be used anywhere a time is expected.
;;; ----------------------------------------------------------------------------

(defconstant +gdk-current-time+ 0)

(export '+gdk-current-time+)

;;; ----------------------------------------------------------------------------
;;; GDK_PRIORITY_EVENTS
;;; 
;;; #define             GDK_PRIORITY_EVENTS
;;; 
;;; This is the priority that events from the X server are given in the GLib Main Loop.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_PRIORITY_REDRAW
;;; 
;;; #define GDK_PRIORITY_REDRAW     (G_PRIORITY_HIGH_IDLE + 20)
;;; 
;;; This is the priority that the idle handler processing window updates is given in the GLib Main Loop.
;;; ----------------------------------------------------------------------------

(defconstant +gdk-priority-redraw+ (+ glib:+g-priority-high-idle+ 20))

(export '+gdk-priority-redraw+)

;;; ----------------------------------------------------------------------------
;;; gdk_events_pending ()
;;; 
;;; gboolean gdk_events_pending (void);
;;; 
;;; Checks if any events are ready to be processed for any display.
;;; 
;;; Returns :
;;; 	TRUE if any events are pending.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_events_pending" events-pending) :boolean)

(export 'events-pending)

;;; ----------------------------------------------------------------------------
;;; gdk_event_peek ()
;;; 
;;; GdkEvent * gdk_event_peek (void);
;;; 
;;; If there is an event waiting in the event queue of some open display,
;;; returns a copy of it. See gdk_display_peek_event().
;;; 
;;; Returns :
;;; 	a copy of the first GdkEvent on some event queue, or NULL if no events
;;;     are in any queues. The returned GdkEvent should be freed with
;;;     gdk_event_free().
;;; ----------------------------------------------------------------------------

(defcfun (event-peek "gdk_event_peek") (g-boxed-foreign event :return))

(export 'event-peek)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get ()
;;; 
;;; GdkEvent *          gdk_event_get                       (void);
;;; 
;;; Checks all open displays for a GdkEvent to process,to be processed on, fetching events from the windowing system if necessary. See gdk_display_get_event().
;;; 
;;; Returns :
;;; 	the next GdkEvent to be processed, or NULL if no events are pending. The returned GdkEvent should be freed with gdk_event_free().
;;; ----------------------------------------------------------------------------

(defcfun (event-get "gdk_event_get") (g-boxed-foreign event :return))

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_graphics_expose ()
;;; 
;;; GdkEvent *          gdk_event_get_graphics_expose       (GdkWindow *window);
;;; 
;;; Warning
;;; 
;;; gdk_event_get_graphics_expose has been deprecated since version 2.18 and should not be used in newly-written code.
;;; 
;;; Waits for a GraphicsExpose or NoExpose event from the X server. This is used in the GtkText and GtkCList widgets in GTK+ to make sure any GraphicsExpose events are handled before the widget is scrolled.
;;; 
;;; window :
;;; 	the GdkWindow to wait for the events for.
;;; 
;;; Returns :
;;; 	a GdkEventExpose if a GraphicsExpose was received, or NULL if a NoExpose event was received.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_put ()
;;; 
;;; void                gdk_event_put                       (const GdkEvent *event);
;;; 
;;; Appends a copy of the given event onto the front of the event queue for event->any.window's display, or the default event queue if event->any.window is NULL. See gdk_display_put_event().
;;; 
;;; event :
;;; 	a GdkEvent.
;;; ----------------------------------------------------------------------------

(defcfun (event-put "gdk_event_put") :void
  (event (g-boxed-foreign event)))

(export 'event-put)

;;; ----------------------------------------------------------------------------
;;; gdk_event_new ()
;;; 
;;; GdkEvent *          gdk_event_new                       (GdkEventType type);
;;; 
;;; Creates a new event of the given type. All fields are set to 0.
;;; 
;;; type :
;;; 	a GdkEventType
;;; 
;;; Returns :
;;; 	a newly-allocated GdkEvent. The returned GdkEvent should be freed with gdk_event_free().
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_copy ()
;;; 
;;; GdkEvent *          gdk_event_copy                      (const GdkEvent *event);
;;; 
;;; Copies a GdkEvent, copying or incrementing the reference count of the resources associated with it (e.g. GdkWindow's and strings).
;;; 
;;; event :
;;; 	a GdkEvent
;;; 
;;; Returns :
;;; 	a copy of event. The returned GdkEvent should be freed with gdk_event_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_free ()
;;; 
;;; void                gdk_event_free                      (GdkEvent *event);
;;; 
;;; Frees a GdkEvent, freeing or decrementing any resources associated with it. Note that this function should only be called with events returned from functions such as gdk_event_peek(), gdk_event_get(), gdk_event_get_graphics_expose() and gdk_event_copy() and gdk_event_new().
;;; 
;;; event :
;;; 	a GdkEvent.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_time ()
;;; 
;;; guint32             gdk_event_get_time                  (const GdkEvent *event);
;;; 
;;; Returns the time stamp from event, if there is one; otherwise returns GDK_CURRENT_TIME. If event is NULL, returns GDK_CURRENT_TIME.
;;; 
;;; event :
;;; 	a GdkEvent
;;; 
;;; Returns :
;;; 	time stamp field from event
;;; ----------------------------------------------------------------------------

(defcfun (event-get-time "gdk_event_get_time") :uint32
  (event (g-boxed-foreign event)))

(export 'event-get-time)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_state ()
;;; 
;;; gboolean            gdk_event_get_state                 (const GdkEvent *event,
;;;                                                          GdkModifierType *state);
;;; 
;;; If the event contains a "state" field, puts that field in state. Otherwise stores an empty state (0). Returns TRUE if there was a state field in the event. event may be NULL, in which case it's treated as if the event had no state field.
;;; 
;;; event :
;;; 	a GdkEvent or NULL
;;; 
;;; state :
;;; 	return location for state. [out]
;;; 
;;; Returns :
;;; 	TRUE if there was a state field in the event
;;; ----------------------------------------------------------------------------

(defcfun gdk_event_get_state :boolean
  (event (g-boxed-foreign event))
  (state (:pointer modifier-type)))

(defun event-get-state (event)
  (with-foreign-object (state 'modifier-type)
    (when (gdk_event_get_state event state)
      (mem-ref state 'modifier-type))))

(export 'event-get-state)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_axis ()
;;; 
;;; gboolean            gdk_event_get_axis                  (const GdkEvent *event,
;;;                                                          GdkAxisUse axis_use,
;;;                                                          gdouble *value);
;;; 
;;; Extract the axis value for a particular axis use from an event structure.
;;; 
;;; event :
;;; 	a GdkEvent
;;; 
;;; axis_use :
;;; 	the axis use to look for
;;; 
;;; value :
;;; 	location to store the value found. [out]
;;; 
;;; Returns :
;;; 	TRUE if the specified axis was found, otherwise FALSE
;;; ----------------------------------------------------------------------------

(defcfun gdk_event_get_axis :boolean
  (event (g-boxed-foreign event))
  (axis-use axis-use)
  (value (:pointer :double)))

(defun event-get-axis (event axis)
  (with-foreign-object (value :double)
    (when (gdk_event_get_axis event axis value)
      (mem-ref value :double))))

(export 'event-get-axis)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_coords ()
;;; 
;;; gboolean            gdk_event_get_coords                (const GdkEvent *event,
;;;                                                          gdouble *x_win,
;;;                                                          gdouble *y_win);
;;; 
;;; Extract the event window relative x/y coordinates from an event.
;;; 
;;; event :
;;; 	a GdkEvent
;;; 
;;; x_win :
;;; 	location to put event window x coordinate. [out]
;;; 
;;; y_win :
;;; 	location to put event window y coordinate. [out]
;;; 
;;; Returns :
;;; 	TRUE if the event delivered event window coordinates
;;; ----------------------------------------------------------------------------

(defcfun gdk_event_get_coords :boolean
  (event (g-boxed-foreign event))
  (x-win (:pointer :double))
  (y-win (:pointer :double)))

(defun event-get-coords (event)
  (with-foreign-objects ((x :double) (y :double))
    (when (gdk_event_get_coords event x y)
      (values (mem-ref x :double) (mem-ref y :double)))))

(export 'event-get-coords)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_root_coords ()
;;; 
;;; gboolean            gdk_event_get_root_coords           (const GdkEvent *event,
;;;                                                          gdouble *x_root,
;;;                                                          gdouble *y_root);
;;; 
;;; Extract the root window relative x/y coordinates from an event.
;;; 
;;; event :
;;; 	a GdkEvent
;;; 
;;; x_root :
;;; 	location to put root window x coordinate. [out]
;;; 
;;; y_root :
;;; 	location to put root window y coordinate. [out]
;;; 
;;; Returns :
;;; 	TRUE if the event delivered root window coordinates
;;; ----------------------------------------------------------------------------

(defcfun gdk_event_get_root_coords :boolean
  (event (g-boxed-foreign event))
  (x-win (:pointer :double))
  (y-win (:pointer :double)))

(defun event-get-root-coords (event)
  (with-foreign-objects ((x :double) (y :double))
    (when (gdk_event_get_root_coords event x y)
      (values (mem-ref x :double) (mem-ref y :double)))))

(export 'event-get-root-coords)

;;; ----------------------------------------------------------------------------
;;; gdk_event_request_motions ()
;;; 
;;; void                gdk_event_request_motions           (const GdkEventMotion *event);
;;; 
;;; Request more motion notifies if event is a motion notify hint event. This function should be used instead of gdk_window_get_pointer() to request further motion notifies, because it also works for extension events where motion notifies are provided for devices other than the core pointer. Coordinate extraction, processing and requesting more motion events from a GDK_MOTION_NOTIFY event usually works like this:
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
;;; { 
;;;   /* motion_event handler */
;;;   x = motion_event->x;
;;;   y = motion_event->y;
;;;   /* handle (x,y) motion */
;;;   gdk_event_request_motions (motion_event); /* handles is_hint events */
;;; }
;;; 
;;; event :
;;; 	a valid GdkEvent
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun (event-request-motions "gdk_event_request_motions") :void
  (event (g-boxed-foreign event)))

(export 'event-request-motions)

;;; ----------------------------------------------------------------------------
;;; gdk_event_handler_set ()
;;; 
;;; void                gdk_event_handler_set               (GdkEventFunc func,
;;;                                                          gpointer data,
;;;                                                          GDestroyNotify notify);
;;; 
;;; Sets the function to call to handle all events from GDK.
;;; 
;;; Note that GTK+ uses this to install its own event handler, so it is usually not useful for GTK+ applications. (Although an application can call this function then call gtk_main_do_event() to pass events to GTK+.)
;;; 
;;; func :
;;; 	the function to call to handle events from GDK.
;;; 
;;; data :
;;; 	user data to pass to the function.
;;; 
;;; notify :
;;; 	the function to call when the handler function is removed, i.e. when gdk_event_handler_set() is called with another event handler.
;;; ----------------------------------------------------------------------------

(defcallback gdk-event-func-callback :void
    ((event (g-boxed-foreign event)) (user-data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value user-data) event)
    (return-from-callback () nil)))

(defcfun gdk_event_handler_set :void
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun event-handler-set (fn)
  (gdk_event_handler_set (callback gdk-event-func-callback)
                         (allocate-stable-pointer fn)
                         (callback stable-pointer-free-destroy-notify-callback)))

(export 'event-handler-set)

;;; ----------------------------------------------------------------------------
;;; GdkEventFunc ()
;;; 
;;; void                (*GdkEventFunc)                     (GdkEvent *event,
;;;                                                          gpointer data);
;;; 
;;; Specifies the type of function passed to gdk_event_handler_set() to handle all GDK events.
;;; 
;;; event :
;;; 	the GdkEvent to process.
;;; 
;;; data :
;;; 	user data set when the event handler was installed with gdk_event_handler_set().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_send_client_message ()
;;; 
;;; gboolean            gdk_event_send_client_message       (GdkEvent *event,
;;;                                                          GdkNativeWindow winid);
;;; 
;;; Sends an X ClientMessage event to a given window (which must be on the default GdkDisplay.) This could be used for communicating between different applications, though the amount of data is limited to 20 bytes.
;;; 
;;; event :
;;; 	the GdkEvent to send, which should be a GdkEventClient.
;;; 
;;; winid :
;;; 	the window to send the X ClientMessage event to.
;;; 
;;; Returns :
;;; 	non-zero on success.
;;; ----------------------------------------------------------------------------

(defcfun (event-send-client-message "gdk_event_send_client_message") :boolean
  (event (g-boxed-foreign event))
  (window-id gdk-native-window))

(export 'event-send-client-message)

;;; ----------------------------------------------------------------------------
;;; gdk_event_send_client_message_for_display ()
;;; 
;;; gboolean gdk_event_send_client_message_for_display (GdkDisplay *display,
;;;                                                     GdkEvent *event,
;;;                                                     GdkNativeWindow winid);
;;; 
;;; On X11, sends an X ClientMessage event to a given window. On Windows, sends
;;; a message registered with the name GDK_WIN32_CLIENT_MESSAGE.
;;; 
;;; This could be used for communicating between different applications, though
;;; the amount of data is limited to 20 bytes on X11, and to just four bytes on
;;; Windows.
;;; 
;;; display :
;;; 	the GdkDisplay for the window where the message is to be sent.
;;; 
;;; event :
;;; 	the GdkEvent to send, which should be a GdkEventClient.
;;; 
;;; winid :
;;; 	the window to send the client message to.
;;; 
;;; Returns :
;;; 	non-zero on success.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_send_client_message_for_display"
          gdk-event-send-client-message-for-display)
    :boolean
  (display (g-object display))
  (event (g-boxed-foreign event))
  (winid gdk-native-window))

(export 'gdk-event-send-client-message-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_event_send_clientmessage_toall ()
;;; 
;;; void gdk_event_send_clientmessage_toall (GdkEvent *event);
;;; 
;;; Sends an X ClientMessage event to all toplevel windows on the default
;;; GdkScreen.
;;; 
;;; Toplevel windows are determined by checking for the WM_STATE property, as
;;; described in the Inter-Client Communication Conventions Manual (ICCCM). If
;;; no windows are found with the WM_STATE property set, the message is sent to
;;; all children of the root window.
;;; 
;;; event :
;;; 	the GdkEvent to send, which should be a GdkEventClient.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_send_clientmessage_toall"
          gdk-event-send-client-message-toall)
    :void
  (event (g-boxed-foreign event)))

(export 'gdk-event-send-client-message-toall)

;;; ----------------------------------------------------------------------------
;;; gdk_add_client_message_filter ()
;;; 
;;; void gdk_add_client_message_filter (GdkAtom message_type,
;;;                                     GdkFilterFunc func,
;;;                                     gpointer data);
;;; 
;;; Adds a filter to the default display to be called when X ClientMessage
;;; events are received. See gdk_display_add_client_message_filter().
;;; 
;;; message_type :
;;; 	the type of ClientMessage events to receive. This will be checked
;;;     against the message_type field of the XClientMessage event struct.
;;; 
;;; func :
;;; 	the function to call to process the event.
;;; 
;;; data :
;;; 	user data to pass to func.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_add_client_message_filter" %gdk-add-client-message-filter) :void
  (message-type gdk-atom-as-string)
  (func :pointer)
  (data :pointer))

(defcallback gdk-client-message-filter-func gdk-filter-return
    ((xevent :pointer) (event :pointer) (data :pointer))
  (multiple-value-bind (return-value translated-event)
      (funcall (stable-pointer-value data) xevent)
    (when (eq return-value :translate)
      (gobject:copy-boxed-slots-to-foreign translated-event event))
    return-value))

(defun gdk-add-client-message-filter (message-type fn)
  (%gdk-add-client-message-filter message-type
                                  (callback gdk-client-message-filter-func)
                                  (allocate-stable-pointer fn)))

(export 'gdk-add-client-message-filter)

;;; ----------------------------------------------------------------------------
;;; gdk_get_show_events ()
;;; 
;;; gboolean gdk_get_show_events (void);
;;; 
;;; Gets whether event debugging output is enabled.
;;; 
;;; Returns :
;;; 	TRUE if event debugging output is enabled.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_get_show_events" gdk-get-show-events) :boolean)

(export 'gdk-get-show-events)

;;; ----------------------------------------------------------------------------
;;; gdk_set_show_events ()
;;; 
;;; void gdk_set_show_events (gboolean show_events);
;;; 
;;; Sets whether a trace of received events is output. Note that GTK+ must be
;;; compiled with debugging (that is, configured using the --enable-debug
;;; option) to use this option.
;;; 
;;; show_events :
;;; 	TRUE to output event debugging information.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_set_show_events" gdk-set-show-events) :void
  (show-events :boolean))

(export 'gdk-set-show-events)

;;; ----------------------------------------------------------------------------
;;; gdk_event_set_screen ()
;;; 
;;; void gdk_event_set_screen (GdkEvent *event, GdkScreen *screen);
;;; 
;;; Sets the screen for event to screen. The event must have been allocated by
;;; GTK+, for instance, by gdk_event_copy().
;;; 
;;; event :
;;; 	a GdkEvent
;;; 
;;; screen :
;;; 	a GdkScreen
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_screen ()
;;; 
;;; GdkScreen * gdk_event_get_screen (const GdkEvent *event);
;;; 
;;; Returns the screen for the event. The screen is typically the screen for
;;; event->any.window, but for events such as mouse events, it is the screen
;;; where the pointer was when the event occurs - that is, the screen which has
;;; the root window to which event->motion.x_root and event->motion.y_root are
;;; relative.
;;; 
;;; event :
;;; 	a GdkEvent
;;; 
;;; Returns :
;;; 	the screen for the event
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_setting_get ()
;;; 
;;; gboolean gdk_setting_get (const gchar *name, GValue *value);
;;; 
;;; Obtains a desktop-wide setting, such as the double-click time, for the
;;; default screen. See gdk_screen_get_setting().
;;; 
;;; name :
;;; 	the name of the setting.
;;; 
;;; value :
;;; 	location to store the value of the setting.
;;; 
;;; Returns :
;;; 	TRUE if the setting existed and a value was stored in value, FALSE
;;;     otherwise.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_setting_get" %gdk-setting-get) :boolean
  (name :string)
  (value (:pointer g-value)))

(defun gdk-get-setting (name)
  (with-foreign-object (value 'g-value)
    (g-value-zero value)
    (when (%gdk-setting-get name value)
      (prog1
        (parse-g-value value)
        (g-value-unset value)))))

(export 'gdk-get-setting)

;;; --- End of file gdk.events.lisp --------------------------------------------
