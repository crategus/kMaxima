;;; ----------------------------------------------------------------------------
;;; gtk.main-loop.lisp
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
;;; Main loop and Events
;;; 
;;; Library initialization, main event loop, and events
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;; void                gtk_disable_setlocale               (void);
;;; PangoLanguage *     gtk_get_default_language            (void);
;;; gboolean            gtk_parse_args                      (int *argc,
;;;                                                          char ***argv);
;;; void                gtk_init                            (int *argc,
;;;                                                          char ***argv);
;;; gboolean            gtk_init_check                      (int *argc,
;;;                                                          char ***argv);
;;; gboolean            gtk_init_with_args                  (gint *argc,
;;;                                                          gchar ***argv,
;;;                                                          const gchar *parameter_string,
;;;                                                          const GOptionEntry *entries,
;;;                                                          const gchar *translation_domain,
;;;                                                          GError **error);
;;; GOptionGroup *      gtk_get_option_group                (gboolean open_default_display);
;;; gboolean            gtk_events_pending                  (void);
;;; void                gtk_main                            (void);
;;; guint               gtk_main_level                      (void);
;;; void                gtk_main_quit                       (void);
;;; gboolean            gtk_main_iteration                  (void);
;;; gboolean            gtk_main_iteration_do               (gboolean blocking);
;;; void                gtk_main_do_event                   (GdkEvent *event);
;;; void                (*GtkModuleInitFunc)                (gint *argc,
;;;                                                          gchar ***argv);
;;; void                (*GtkModuleDisplayInitFunc)         (GdkDisplay *display);
;;; 
;;; gboolean            gtk_true                            (void);
;;; gboolean            gtk_false                           (void);
;;; 
;;; void                gtk_grab_add                        (GtkWidget *widget);
;;; GtkWidget *         gtk_grab_get_current                (void);
;;; void                gtk_grab_remove                     (GtkWidget *widget);
;;; void                gtk_device_grab_add                 (GtkWidget *widget,
;;;                                                          GdkDevice *device,
;;;                                                          gboolean block_others);
;;; void                gtk_device_grab_remove              (GtkWidget *widget,
;;;                                                          GdkDevice *device);
;;; 
;;; #define             GTK_PRIORITY_RESIZE
;;; 
;;; guint               gtk_key_snooper_install             (GtkKeySnoopFunc snooper,
;;;                                                          gpointer func_data);
;;; gint                (*GtkKeySnoopFunc)                  (GtkWidget *grab_widget,
;;;                                                          GdkEventKey *event,
;;;                                                          gpointer func_data);
;;; void                gtk_key_snooper_remove              (guint snooper_handler_id);
;;; 
;;; GdkEvent *          gtk_get_current_event               (void);
;;; guint32             gtk_get_current_event_time          (void);
;;; gboolean            gtk_get_current_event_state         (GdkModifierType *state);
;;; GdkDevice *         gtk_get_current_event_device        (void);
;;; GtkWidget *         gtk_get_event_widget                (GdkEvent *event);
;;; void                gtk_propagate_event                 (GtkWidget *widget,
;;;                                                          GdkEvent *event);
;;; 
;;; Description
;;; 
;;; Before using GTK+, you need to initialize it; initialization connects to
;;; the window system display, and parses some standard command line arguments.
;;; The gtk_init() macro initializes GTK+. gtk_init() exits the application if
;;; errors occur; to avoid this, use gtk_init_check(). gtk_init_check() allows
;;; you to recover from a failed GTK+ initialization - you might start up your
;;; application in text mode instead.
;;; 
;;; Like all GUI toolkits, GTK+ uses an event-driven programming model. When
;;; the user is doing nothing, GTK+ sits in the main loop and waits for input.
;;; If the user performs some action - say, a mouse click - then the main loop
;;; "wakes up" and delivers an event to GTK+. GTK+ forwards the event to one or
;;; more widgets.
;;; 
;;; When widgets receive an event, they frequently emit one or more signals.
;;; Signals notify your program that "something interesting happened" by
;;; invoking functions you've connected to the signal with g_signal_connect().
;;; Functions connected to a signal are often termed callbacks.
;;; 
;;; When your callbacks are invoked, you would typically take some action - for
;;; example, when an Open button is clicked you might display a
;;; GtkFileChooserDialog. After a callback finishes, GTK+ will return to the
;;; main loop and await more user input.
;;; 
;;; Example 6. Typical main() function for a GTK+ application
;;; 
;;;  1 int
;;;  2 main (int argc, char **argv)
;;;  3 {
;;;  4   /* Initialize i18n support */
;;;  5   gtk_set_locale ();
;;;  6 
;;;  7   /* Initialize the widget set */
;;;  8   gtk_init (&argc, &argv);
;;;  9 
;;; 10   /* Create the main window */
;;; 11   mainwin = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;;; 12
;;; 13   /* Set up our GUI elements */
;;; 14   ...
;;; 15 
;;; 16   /* Show the application window */
;;; 17   gtk_widget_show_all (mainwin);
;;; 18
;;; 19   /* Enter the main event loop, and wait for user interaction */
;;; 20   gtk_main ();
;;; 21 
;;; 22   /* The user lost interest */
;;; 23   return 0;
;;; 24 }
;;; 
;;; It's OK to use the GLib main loop directly instead of gtk_main(), though it
;;; involves slightly more typing. See GMainLoop in the GLib documentation.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; gtk_disable_setlocale ()
;;; 
;;; void gtk_disable_setlocale (void)
;;; 
;;; Prevents gtk_init(), gtk_init_check(), gtk_init_with_args() and
;;; gtk_parse_args() from automatically calling setlocale (LC_ALL, ""). You
;;; would want to use this function if you wanted to set the locale for your
;;; program to something other than the user's locale, or if you wanted to set
;;; different values for different locale categories.
;;; 
;;; Most programs should not need to call this function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_get_default_language ()
;;; 
;;; PangoLanguage * gtk_get_default_language (void)
;;; 
;;; Returns the PangoLanguage for the default language currently in effect.
;;; (Note that this can change over the life of an application.) The default
;;; language is derived from the current locale. It determines, for example,
;;; whether GTK+ uses the right-to-left or left-to-right text direction.
;;; 
;;; This function is equivalent to pango_language_get_default(). See that
;;; function for details.
;;; 
;;; Returns :
;;; 	the default language as a PangoLanguage, must not be freed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_parse_args ()
;;; 
;;; gboolean gtk_parse_args (int *argc, char ***argv)
;;; 
;;; Parses command line arguments, and initializes global attributes of GTK+,
;;; but does not actually open a connection to a display. (See
;;; gdk_display_open(), gdk_get_display_arg_name())
;;; 
;;; Any arguments used by GTK+ or GDK are removed from the array and argc and
;;; argv are updated accordingly.
;;; 
;;; There is no need to call this function explicitely if you are using
;;; gtk_init(), or gtk_init_check().
;;; 
;;; argc :
;;; 	a pointer to the number of command line arguments. [inout]
;;; 
;;; argv :
;;; 	a pointer to the array of command line arguments.
;;; 
;;; Returns :
;;; 	TRUE if initialization succeeded, otherwise FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_init ()
;;; 
;;; void gtk_init (int *argc, char ***argv)
;;; 
;;; Call this function before using any other GTK+ functions in your GUI
;;; applications. It will initialize everything needed to operate the toolkit
;;; and parses some standard command line options.
;;; 
;;; Although you are expected to pass the argc, argv parameters from main() to
;;; this function, it is possible to pass NULL if argv is not available or
;;; commandline handling is not required.
;;; 
;;; argc and argv are adjusted accordingly so your own code will never see those
;;; standard arguments.
;;; 
;;; Note that there are some alternative ways to initialize GTK+: if you are
;;; calling gtk_parse_args(), gtk_init_check(), gtk_init_with_args() or
;;; g_option_context_parse() with the option group returned by
;;; gtk_get_option_group(), you don't have to call gtk_init().
;;; 
;;; Note
;;; 
;;; This function will terminate your program if it was unable to initialize
;;; the windowing system for some reason. If you want your program to fall back
;;; to a textual interface you want to call gtk_init_check() instead.
;;; 
;;; Note
;;; 
;;; Since 2.18, GTK+ calls signal (SIGPIPE, SIG_IGN) during initialization, to
;;; ignore SIGPIPE signals, since these are almost never wanted in graphical
;;; applications. If you do need to handle SIGPIPE for some reason, reset the
;;; handler after gtk_init(), but notice that other libraries (e.g. libdbus or
;;; gvfs) might do similar things.
;;; 
;;; argc :
;;; 	Address of the argc parameter of your main() function (or 0 if argv is
;;;     NULL). This will be changed if any arguments were handled. [inout]
;;; 
;;; argv :
;;; 	Address of the argv parameter of main(), or NULL. Any options understood
;;;     by GTK+ are stripped before return.
;;; ----------------------------------------------------------------------------

(defun gtk-init ()
  (gtk-init-check (foreign-alloc :int :initial-element 0)
                  (foreign-alloc :string :initial-contents '("/usr/bin/sbcl")))
  #+ (and sbcl (not win32))
  (sb-unix::enable-interrupt sb-unix:sigpipe #'sb-unix::sigpipe-handler)
  #+nil(with-foreign-objects ((argc :int)
                         (argv '(:pointer :string) 1))
    (setf (mem-ref argc :int) 0
          (mem-ref argv '(:pointer :string))
          (foreign-alloc :string :count 1 :initial-element "/usr/bin/sbcl"))
    (unwind-protect
         (unless (gtk-init-check argc argv)
           (error "Cannot initialize Gtk+"))
      (foreign-free (mem-ref argv '(:pointer :string))))))

;;; ----------------------------------------------------------------------------
;;; gtk_init_check ()
;;; 
;;; gboolean gtk_init_check (int *argc, char ***argv)
;;; 
;;; This function does the same work as gtk_init() with only a single change:
;;; It does not terminate the program if the windowing system can't be
;;; initialized. Instead it returns FALSE on failure.
;;; 
;;; This way the application can fall back to some other means of communication
;;; with the user - for example a curses or command line interface.
;;; 
;;; argc :
;;; 	Address of the argc parameter of your main() function (or 0 if argv is
;;;     NULL). This will be changed if any arguments were handled. [inout]
;;; 
;;; argv :
;;; 	Address of the argv parameter of main(), or NULL. Any options understood
;;;     by GTK+ are stripped before return.
;;; 
;;; Returns :
;;; 	TRUE if the windowing system has been successfully initialized, FALSE
;;;     otherwise
;;; ----------------------------------------------------------------------------

(defcfun gtk-init-check :boolean
  (argc (:pointer :int))
  (argv (:pointer (:pointer :string))))

;;; ----------------------------------------------------------------------------
;;; gtk_init_with_args ()
;;; 
;;; gboolean gtk_init_with_args (gint *argc,
;;;                              gchar ***argv,
;;;                              const gchar *parameter_string,
;;;                              const GOptionEntry *entries,
;;;                              const gchar *translation_domain,
;;;                              GError **error)
;;; 
;;; This function does the same work as gtk_init_check(). Additionally, it
;;; allows you to add your own commandline options, and it automatically
;;; generates nicely formatted --help output. Note that your program will be
;;; terminated after writing out the help output.
;;; 
;;; argc :
;;; 	Address of the argc parameter of your main() function (or 0 if argv is
;;;     NULL). This will be changed if any arguments were handled. [inout]
;;; 
;;; argv :
;;; 	Address of the argv parameter of main(), or NULL. Any options understood
;;;     by GTK+ are stripped before return.
;;; 
;;; parameter_string :
;;; 	a string which is displayed in the first line of --help output, after
;;;     programname [OPTION...]
;;; 
;;; entries :
;;; 	a NULL-terminated array of GOptionEntrys describing the options of your
;;;     program. [array zero-terminated=1]
;;; 
;;; translation_domain :
;;; 	a translation domain to use for translating the --help output for the
;;;     options in entries and the parameter_string with gettext(), or NULL
;;; 
;;; error :
;;; 	a return location for errors
;;; 
;;; Returns :
;;; 	TRUE if the windowing system has been successfully initialized, FALSE
;;;     otherwise
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_get_option_group ()
;;; 
;;; GOptionGroup * gtk_get_option_group (gboolean open_default_display)
;;; 
;;; Returns a GOptionGroup for the commandline arguments recognized by GTK+ and
;;; GDK.
;;; 
;;; You should add this group to your GOptionContext with
;;; g_option_context_add_group(), if you are using g_option_context_parse() to
;;; parse your commandline arguments.
;;; 
;;; open_default_display :
;;; 	whether to open the default display when parsing the commandline
;;;     arguments
;;; 
;;; Returns :
;;; 	a GOptionGroup for the commandline arguments recognized by GTK+
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_events_pending ()
;;; 
;;; gboolean gtk_events_pending (void)
;;; 
;;; Checks if any events are pending.
;;; 
;;; This can be used to update the UI and invoke timeouts etc. while doing some
;;; time intensive computation.
;;; 
;;; Example 7. Updating the UI during a long computation
;;; 
;;;  1 /* computation going on... */
;;;  2 
;;;  3 while (gtk_events_pending ())
;;;  4   gtk_main_iteration ();
;;;  5 
;;;  6 /* ...computation continued */
;;;  
;;; Returns :
;;; 	TRUE if any events are pending, FALSE otherwise
;;; ----------------------------------------------------------------------------

(defcfun gtk-events-pending :boolean)

(export 'gtk-events-pending)

;;; ----------------------------------------------------------------------------
;;; gtk_main ()
;;; 
;;; void gtk_main (void)
;;; 
;;; Runs the main loop until gtk_main_quit() is called.
;;; 
;;; You can nest calls to gtk_main(). In that case gtk_main_quit() will make
;;; the innermost invocation of the main loop return.
;;; ----------------------------------------------------------------------------

(defcfun (%gtk-main "gtk_main") :void)

(defun gtk-main ()
  (with-gdk-threads-lock (%gtk-main)))

;;; ----------------------------------------------------------------------------
;;; gtk_main_level ()
;;; 
;;; guint gtk_main_level (void)
;;; 
;;; Asks for the current nesting level of the main loop.
;;; 
;;; Returns :
;;; 	the nesting level of the current invocation of the main loop
;;; ----------------------------------------------------------------------------

(defcfun main-level :uint)

(export 'main-level)

;;; ----------------------------------------------------------------------------
;;; gtk_main_quit ()
;;; 
;;; void gtk_main_quit (void)
;;; 
;;; Makes the innermost invocation of the main loop return when it regains
;;; control.
;;; ----------------------------------------------------------------------------

(defcfun (main-quit "gtk_main_quit") :void)

(export 'main-quit)

;;; ----------------------------------------------------------------------------
;;; gtk_main_iteration ()
;;; 
;;; gboolean gtk_main_iteration (void)
;;; 
;;; Runs a single iteration of the mainloop.
;;; 
;;; If no events are waiting to be processed GTK+ will block until the next
;;; event is noticed. If you don't want to block look at gtk_main_iteration_do()
;;; or check if any events are pending with gtk_events_pending() first.
;;; 
;;; Returns :
;;; 	TRUE if gtk_main_quit() has been called for the innermost mainloop
;;; ----------------------------------------------------------------------------

(defcfun gtk-main-iteration :boolean)

(export 'gtk-main-iteration)

;;; ----------------------------------------------------------------------------
;;; gtk_main_iteration_do ()
;;; 
;;; gboolean gtk_main_iteration_do (gboolean blocking)
;;; 
;;; Runs a single iteration of the mainloop. If no events are available either
;;; return or block depending on the value of blocking.
;;; 
;;; blocking :
;;; 	TRUE if you want GTK+ to block if no events are pending
;;; 
;;; Returns :
;;; 	TRUE if gtk_main_quit() has been called for the innermost mainloop
;;; ----------------------------------------------------------------------------

(defcfun gtk-main-iteration-do :boolean
  (blocking :boolean))

(export 'gtk-main-iteration-do)

;;; ----------------------------------------------------------------------------
;;; gtk_main_do_event ()
;;; 
;;; void gtk_main_do_event (GdkEvent *event)
;;; 
;;; Processes a single GDK event.
;;; 
;;; This is public only to allow filtering of events between GDK and GTK+. You
;;; will not usually need to call this function directly.
;;; 
;;; While you should not call this function directly, you might want to know
;;; how exactly events are handled. So here is what this function does with the
;;; event:
;;; 
;;;    1. Compress enter/leave notify events. If the event passed build an
;;;       enter/leave pair together with the next event (peeked from GDK), both
;;;       events are thrown away. This is to avoid a backlog of
;;;       (de-)highlighting widgets crossed by the pointer.
;;;
;;;    2. Find the widget which got the event. If the widget can't be determined
;;;       the event is thrown away unless it belongs to a INCR transaction. In
;;;       that case it is passed to gtk_selection_incr_event().
;;;
;;;    3. Then the event is pushed onto a stack so you can query the currently
;;;       handled event with gtk_get_current_event().
;;;
;;;    4. The event is sent to a widget. If a grab is active all events for
;;;       widgets that are not in the contained in the grab widget are sent to
;;;       the latter with a few exceptions:
;;;
;;;           * Deletion and destruction events are still sent to the event
;;;             widget for obvious reasons.
;;;           * Events which directly relate to the visual representation of
;;;             the event widget.
;;;           * Leave events are delivered to the event widget if there was an
;;;             enter event delivered to it before without the paired leave
;;;             event.
;;;           * Drag events are not redirected because it is unclear what the
;;;             semantics of that would be.
;;; 
;;;       Another point of interest might be that all key events are first
;;;       passed through the key snooper functions if there are any. Read the
;;;       description of gtk_key_snooper_install() if you need this feature.
;;;
;;;    5. After finishing the delivery the event is popped from the event stack.
;;; 
;;; event :
;;; 	An event to process (normally passed by GDK)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkModuleInitFunc ()
;;; 
;;; void (*GtkModuleInitFunc) (gint *argc, gchar ***argv)
;;; 
;;; Each GTK+ module must have a function gtk_module_init() with this prototype.
;;; This function is called after loading the module.
;;; 
;;; argc :
;;; 	GTK+ always passes NULL for this argument.
;;; 
;;; argv :
;;; 	GTK+ always passes NULL for this argument.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkModuleDisplayInitFunc ()
;;; 
;;; void (*GtkModuleDisplayInitFunc) (GdkDisplay *display)
;;; 
;;; A multihead-aware GTK+ module may have a gtk_module_display_init() function
;;; with this prototype. GTK+ calls this function for each opened display.
;;; 
;;; display :
;;; 	an open GdkDisplay
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_true ()
;;; 
;;; gboolean gtk_true (void)
;;; 
;;; All this function does it to return TRUE.
;;; 
;;; This can be useful for example if you want to inhibit the deletion of a
;;; window. Of course you should not do this as the user expects a reaction
;;; from clicking the close icon of the window.
;;; 
;;; Example 8. A persistent window
;;; 
;;;  1 #include <gtk/gtk.h><
;;;  2 
;;;  3 int
;;;  4 main (int argc, char **argv)
;;;  5 {
;;;  6   GtkWidget *win, *but;
;;;  7 
;;;  8   gtk_init (&argc, &argv);
;;;  9 
;;; 10   win = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;;; 11   g_signal_connect (win, "delete-event",
;;; 12                     G_CALLBACK (gtk_true), NULL);
;;; 13   g_signal_connect (win, "destroy",
;;; 14                     G_CALLBACK (gtk_main_quit), NULL);
;;; 15 
;;; 16   but = gtk_button_new_with_label ("Close yourself. I mean it!");
;;; 17   g_signal_connect_swapped (but, "clicked",
;;; 18                             G_CALLBACK (gtk_object_destroy), win);
;;; 19   gtk_container_add (GTK_CONTAINER (win), but);
;;; 20 
;;; 21   gtk_widget_show_all (win);
;;; 22 
;;; 23   gtk_main ();
;;; 24 
;;; 25   return 0;
;;; 26 }
;;; 
;;; Returns :
;;; 	TRUE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_false ()
;;; 
;;; gboolean gtk_false (void)
;;; 
;;; Analogical to gtk_true(), this function does nothing but always returns
;;; FALSE.
;;; 
;;; Returns :
;;; 	FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_grab_add ()
;;; 
;;; void gtk_grab_add (GtkWidget *widget)
;;; 
;;; Makes widget the current grabbed widget.
;;; 
;;; This means that interaction with other widgets in the same application is
;;; blocked and mouse as well as keyboard events are delivered to this widget.
;;; 
;;; If widget is not sensitive, it is not set as the current grabbed widget and
;;; this function does nothing.
;;; 
;;; widget :
;;; 	The widget that grabs keyboard and pointer events
;;; ----------------------------------------------------------------------------

(defcfun gtk-grab-add :void
  (widget g-object))

(defcfun (grab-add "gtk_grab_add") :void
  (widget g-object))

(export 'grab-add)

;;; ----------------------------------------------------------------------------
;;; gtk_grab_get_current ()
;;; 
;;; GtkWidget * gtk_grab_get_current (void)
;;; 
;;; Queries the current grab of the default window group.
;;; 
;;; Returns :
;;; 	The widget which currently has the grab or NULL if no grab is active.
;;; ----------------------------------------------------------------------------

(defcfun gtk-grab-get-current g-object)

(defcfun (grab-current "gtk_grab_get_current") g-object)

(export 'grab-current)

;;; ----------------------------------------------------------------------------
;;; gtk_grab_remove ()
;;; 
;;; void gtk_grab_remove (GtkWidget *widget)
;;; 
;;; Removes the grab from the given widget.
;;; 
;;; You have to pair calls to gtk_grab_add() and gtk_grab_remove().
;;; 
;;; If widget does not have the grab, this function does nothing.
;;; 
;;; widget :
;;; 	The widget which gives up the grab
;;; ----------------------------------------------------------------------------

(defcfun gtk-grab-remove :void
  (widget g-object))

(defcfun (grab-remove "gtk_grab_remove") :void
  (widget g-object))

(export 'grab-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_device_grab_add ()
;;; 
;;; void gtk_device_grab_add (GtkWidget *widget,
;;;                           GdkDevice *device,
;;;                           gboolean block_others)
;;; 
;;; Adds a GTK+ grab on device, so all the events on device and its associated
;;; pointer or keyboard (if any) are delivered to widget. If the block_others
;;; parameter is TRUE, any other devices will be unable to interact with widget
;;; during the grab.
;;; 
;;; widget :
;;; 	a GtkWidget
;;; 
;;; device :
;;; 	a GtkDevice to grab on.
;;; 
;;; block_others :
;;; 	TRUE to prevent other devices to interact with widget.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_device_grab_remove ()
;;; 
;;; void gtk_device_grab_remove (GtkWidget *widget, GdkDevice *device)
;;; 
;;; Removes a device grab from the given widget.
;;; 
;;; You have to pair calls to gtk_device_grab_add() and
;;; gtk_device_grab_remove().
;;; 
;;; widget :
;;; 	a GtkWidget
;;; 
;;; device :
;;; 	a GdkDevice
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRIORITY_RESIZE
;;; 
;;; #define GTK_PRIORITY_RESIZE (G_PRIORITY_HIGH_IDLE + 10)
;;; 
;;; Use this priority for functionality related to size allocation.
;;; 
;;; It is used internally by GTK+ to compute the sizes of widgets. This
;;; priority is higher than GDK_PRIORITY_REDRAW to avoid resizing a widget
;;; which was just redrawn.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_key_snooper_install ()
;;; 
;;; guint gtk_key_snooper_install (GtkKeySnoopFunc snooper, gpointer func_data)
;;; 
;;; Installs a key snooper function, which will get called on all key events
;;; before delivering them normally.
;;; 
;;; snooper :
;;; 	a GtkKeySnoopFunc
;;; 
;;; func_data :
;;; 	data to pass to snooper
;;; 
;;; Returns :
;;; 	a unique id for this key snooper for use with gtk_key_snooper_remove().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkKeySnoopFunc ()
;;; 
;;; gint (*GtkKeySnoopFunc) (GtkWidget *grab_widget,
;;;                          GdkEventKey *event,
;;;                          gpointer func_data)
;;; 
;;; Key snooper functions are called before normal event delivery. They can be
;;; used to implement custom key event handling.
;;; 
;;; grab_widget :
;;; 	the widget to which the event will be delivered
;;; 
;;; event :
;;; 	the key event
;;; 
;;; func_data :
;;; 	data supplied to gtk_key_snooper_install()
;;; 
;;; Returns :
;;; 	TRUE to stop further processing of event, FALSE to continue.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_key_snooper_remove ()
;;; 
;;; void gtk_key_snooper_remove (guint snooper_handler_id)
;;; 
;;; Removes the key snooper function with the given id.
;;; 
;;; snooper_handler_id :
;;; 	Identifies the key snooper to remove
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_get_current_event ()
;;; 
;;; GdkEvent * gtk_get_current_event (void)
;;; 
;;; Obtains a copy of the event currently being processed by GTK+.
;;; 
;;; For example, if you are handling a "clicked" signal, the current event will
;;; be the GdkEventButton that triggered the ::clicked signal.
;;; 
;;; Returns :
;;; 	a copy of the current event, or NULL if there is no current event. The
;;;     returned event must be freed with gdk_event_free(). [transfer full]
;;; ----------------------------------------------------------------------------

(defcfun (current-event "gtk_get_current_event")
         (g-boxed-foreign event :return))

(export 'current-event)

;;; ----------------------------------------------------------------------------
;;; gtk_get_current_event_time ()
;;; 
;;; guint32 gtk_get_current_event_time (void)
;;; 
;;; If there is a current event and it has a timestamp, return that timestamp,
;;; otherwise return GDK_CURRENT_TIME.
;;; 
;;; Returns :
;;; 	the timestamp from the current event, or GDK_CURRENT_TIME.
;;; ----------------------------------------------------------------------------

(defcfun (current-event-time "gtk_get_current_event_time") :uint32)

(export 'current-event-time)

;;; ----------------------------------------------------------------------------
;;; gtk_get_current_event_state ()
;;; 
;;; gboolean gtk_get_current_event_state (GdkModifierType *state)
;;; 
;;; If there is a current event and it has a state field, place that state
;;; field in state and return TRUE, otherwise return FALSE.
;;; 
;;; state :
;;; 	a location to store the state of the current event. [out]
;;; 
;;; Returns :
;;; 	TRUE if there was a current event and it had a state field
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_get_current_event_device ()
;;; 
;;; GdkDevice * gtk_get_current_event_device (void)
;;; 
;;; If there is a current event and it has a device, return that device,
;;; otherwise return NULL.
;;; 
;;; Returns :
;;; 	a GdkDevice, or NULL. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_get_event_widget ()
;;; 
;;; GtkWidget * gtk_get_event_widget (GdkEvent *event)
;;; 
;;; If event is NULL or the event was not associated with any widget, returns
;;; NULL, otherwise returns the widget that received the event originally.
;;; 
;;; event :
;;; 	a GdkEvent
;;; 
;;; Returns :
;;; 	the widget that originally received event, or NULL. [transfer none]
;;; ----------------------------------------------------------------------------

(defcfun (event-widget "gtk_get_event_widget") g-object
  (event (g-boxed-foreign event)))

(export 'event-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_propagate_event ()
;;; 
;;; void gtk_propagate_event (GtkWidget *widget, GdkEvent *event)
;;; 
;;; Sends an event to a widget, propagating the event to parent widgets if the
;;; event remains unhandled.
;;; 
;;; Events received by GTK+ from GDK normally begin in gtk_main_do_event().
;;; Depending on the type of event, existence of modal dialogs, grabs, etc.,
;;; the event may be propagated; if so, this function is used.
;;; 
;;; gtk_propagate_event() calls gtk_widget_event() on each widget it decides to
;;; send the event to. So gtk_widget_event() is the lowest-level function; it
;;; simply emits the "event" and possibly an event-specific signal on a widget.
;;; gtk_propagate_event() is a bit higher-level, and gtk_main_do_event() is the
;;; highest level.
;;; 
;;; All that said, you most likely don't want to use any of these functions;
;;; synthesizing events is rarely needed. There are almost certainly better
;;; ways to achieve your goals. For example, use gdk_window_invalidate_rect()
;;; or gtk_widget_queue_draw() instead of making up expose events.
;;; 
;;; widget :
;;; 	a GtkWidget
;;; 
;;; event :
;;; 	an event
;;; ----------------------------------------------------------------------------

(defcfun (propagate-event "gtk_propagate_event") :void
  (widget g-object)
  (event (g-boxed-foreign event)))

(export 'propagate-event)

;;; --- End of file gtk.main-loop.lisp -----------------------------------------
