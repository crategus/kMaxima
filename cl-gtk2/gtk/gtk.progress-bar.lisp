;;; ----------------------------------------------------------------------------
;;; gtk.progress-bar.lisp
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
;;; GtkProgressBar
;;; 
;;; GtkProgressBar — A widget which indicates progress visually
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;; struct              GtkProgressBar;
;;; GtkWidget *         gtk_progress_bar_new                (void);
;;; void                gtk_progress_bar_pulse              (GtkProgressBar *pbar);
;;; void                gtk_progress_bar_set_fraction       (GtkProgressBar *pbar,
;;;                                                          gdouble fraction);
;;; gdouble             gtk_progress_bar_get_fraction       (GtkProgressBar *pbar);
;;; void                gtk_progress_bar_set_inverted       (GtkProgressBar *pbar,
;;;                                                          gboolean inverted);
;;; gboolean            gtk_progress_bar_get_inverted       (GtkProgressBar *pbar);
;;; void                gtk_progress_bar_set_show_text      (GtkProgressBar *pbar,
;;;                                                          gboolean show_text);
;;; gboolean            gtk_progress_bar_get_show_text      (GtkProgressBar *pbar);
;;; void                gtk_progress_bar_set_text           (GtkProgressBar *pbar,
;;;                                                          const gchar *text);
;;; const gchar *       gtk_progress_bar_get_text           (GtkProgressBar *pbar);
;;; void                gtk_progress_bar_set_ellipsize      (GtkProgressBar *pbar,
;;;                                                          PangoEllipsizeMode mode);
;;; PangoEllipsizeMode  gtk_progress_bar_get_ellipsize      (GtkProgressBar *pbar);
;;; void                gtk_progress_bar_set_pulse_step     (GtkProgressBar *pbar,
;;;                                                          gdouble fraction);
;;; gdouble             gtk_progress_bar_get_pulse_step     (GtkProgressBar *pbar);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkProgressBar
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkProgressBar implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; Properties
;;; 
;;;   "ellipsize"                PangoEllipsizeMode    : Read / Write
;;;   "fraction"                 gdouble               : Read / Write
;;;   "inverted"                 gboolean              : Read / Write
;;;   "pulse-step"               gdouble               : Read / Write
;;;   "show-text"                gboolean              : Read / Write
;;;   "text"                     gchar*                : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "min-horizontal-bar-height" gint                  : Read / Write
;;;   "min-horizontal-bar-width" gint                  : Read / Write
;;;   "min-vertical-bar-height"  gint                  : Read / Write
;;;   "min-vertical-bar-width"   gint                  : Read / Write
;;;   "xspacing"                 gint                  : Read / Write
;;;   "yspacing"                 gint                  : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkProgressBar is typically used to display the progress of a long running operation. It provides a visual clue that processing is underway. The GtkProgressBar can be used in two different modes: percentage mode and activity mode.
;;; 
;;; When an application can determine how much work needs to take place (e.g. read a fixed number of bytes from a file) and can monitor its progress, it can use the GtkProgressBar in percentage mode and the user sees a growing bar indicating the percentage of the work that has been completed. In this mode, the application is required to call gtk_progress_bar_set_fraction() periodically to update the progress bar.
;;; 
;;; When an application has no accurate way of knowing the amount of work to do, it can use the GtkProgressBar in activity mode, which shows activity by a block moving back and forth within the progress area. In this mode, the application is required to call gtk_progress_bar_pulse() periodically to update the progress bar.
;;; 
;;; There is quite a bit of flexibility provided to control the appearance of the GtkProgressBar. Functions are provided to control the orientation of the bar, optional text can be displayed along with the bar, and the step size used in activity mode can be set.
;;; Details
;;; struct GtkProgressBar
;;; 
;;; struct GtkProgressBar;
;;; 
;;; gtk_progress_bar_new ()
;;; 
;;; GtkWidget *         gtk_progress_bar_new                (void);
;;; 
;;; Creates a new GtkProgressBar.
;;; 
;;; Returns :
;;; 	a GtkProgressBar.
;;; gtk_progress_bar_pulse ()
;;; 
;;; void                gtk_progress_bar_pulse              (GtkProgressBar *pbar);
;;; 
;;; Indicates that some progress is made, but you don't know how much. Causes the progress bar to enter "activity mode," where a block bounces back and forth. Each call to gtk_progress_bar_pulse() causes the block to move by a little bit (the amount of movement per pulse is determined by gtk_progress_bar_set_pulse_step()).
;;; 
;;; pbar :
;;; 	a GtkProgressBar
;;; gtk_progress_bar_set_fraction ()
;;; 
;;; void                gtk_progress_bar_set_fraction       (GtkProgressBar *pbar,
;;;                                                          gdouble fraction);
;;; 
;;; Causes the progress bar to "fill in" the given fraction of the bar. The fraction should be between 0.0 and 1.0, inclusive.
;;; 
;;; pbar :
;;; 	a GtkProgressBar
;;; 
;;; fraction :
;;; 	fraction of the task that's been completed
;;; gtk_progress_bar_get_fraction ()
;;; 
;;; gdouble             gtk_progress_bar_get_fraction       (GtkProgressBar *pbar);
;;; 
;;; Returns the current fraction of the task that's been completed.
;;; 
;;; pbar :
;;; 	a GtkProgressBar
;;; 
;;; Returns :
;;; 	a fraction from 0.0 to 1.0
;;; gtk_progress_bar_set_inverted ()
;;; 
;;; void                gtk_progress_bar_set_inverted       (GtkProgressBar *pbar,
;;;                                                          gboolean inverted);
;;; 
;;; Progress bars normally grow from top to bottom or left to right. Inverted progress bars grow in the opposite direction.
;;; 
;;; pbar :
;;; 	a GtkProgressBar
;;; 
;;; inverted :
;;; 	TRUE to invert the progress bar
;;; gtk_progress_bar_get_inverted ()
;;; 
;;; gboolean            gtk_progress_bar_get_inverted       (GtkProgressBar *pbar);
;;; 
;;; Gets the value set by gtk_progress_bar_set_inverted()
;;; 
;;; pbar :
;;; 	a GtkProgressBar
;;; 
;;; Returns :
;;; 	TRUE if the progress bar is inverted
;;; gtk_progress_bar_set_show_text ()
;;; 
;;; void                gtk_progress_bar_set_show_text      (GtkProgressBar *pbar,
;;;                                                          gboolean show_text);
;;; 
;;; Sets whether the progressbar will show text superimposed over the bar. The shown text is either the value of the "text" property or, if that is NULL, the "fraction" value, as a percentage.
;;; 
;;; To make a progress bar that is styled and sized suitably for containing text (even if the actual text is blank), set "show-text" to TRUE and "text" to the empty string (not NULL).
;;; 
;;; pbar :
;;; 	a GtkProgressBar
;;; 
;;; show_text :
;;; 	whether to show superimposed text
;;; 
;;; Since 3.0
;;; gtk_progress_bar_get_show_text ()
;;; 
;;; gboolean            gtk_progress_bar_get_show_text      (GtkProgressBar *pbar);
;;; 
;;; Gets the value of the "show-text" property. See gtk_progress_bar_set_show_text().
;;; 
;;; pbar :
;;; 	a GtkProgressBar
;;; 
;;; Returns :
;;; 	TRUE if text is shown in the progress bar
;;; 
;;; Since 3.0
;;; gtk_progress_bar_set_text ()
;;; 
;;; void                gtk_progress_bar_set_text           (GtkProgressBar *pbar,
;;;                                                          const gchar *text);
;;; 
;;; Causes the given text to appear superimposed on the progress bar.
;;; 
;;; If text is NULL and "show-text" is TRUE, the current value of "fraction" will be displayed as a percentage.
;;; 
;;; If text is non-NULL and "show-text" is TRUE, the text will be displayed. In this case, it will not display the progress percentage. If text is the empty string, the progress bar will still be styled and sized suitably for containing text, as long as "show-text" is TRUE.
;;; 
;;; pbar :
;;; 	a GtkProgressBar
;;; 
;;; text :
;;; 	a UTF-8 string, or NULL. [allow-none]
;;; gtk_progress_bar_get_text ()
;;; 
;;; const gchar *       gtk_progress_bar_get_text           (GtkProgressBar *pbar);
;;; 
;;; Retrieves the text displayed superimposed on the progress bar, if any, otherwise NULL. The return value is a reference to the text, not a copy of it, so will become invalid if you change the text in the progress bar.
;;; 
;;; pbar :
;;; 	a GtkProgressBar
;;; 
;;; Returns :
;;; 	text, or NULL; this string is owned by the widget and should not be modified or freed.
;;; gtk_progress_bar_set_ellipsize ()
;;; 
;;; void                gtk_progress_bar_set_ellipsize      (GtkProgressBar *pbar,
;;;                                                          PangoEllipsizeMode mode);
;;; 
;;; Sets the mode used to ellipsize (add an ellipsis: "...") the text if there is not enough space to render the entire string.
;;; 
;;; pbar :
;;; 	a GtkProgressBar
;;; 
;;; mode :
;;; 	a PangoEllipsizeMode
;;; 
;;; Since 2.6
;;; gtk_progress_bar_get_ellipsize ()
;;; 
;;; PangoEllipsizeMode  gtk_progress_bar_get_ellipsize      (GtkProgressBar *pbar);
;;; 
;;; Returns the ellipsizing position of the progressbar. See gtk_progress_bar_set_ellipsize().
;;; 
;;; pbar :
;;; 	a GtkProgressBar
;;; 
;;; Returns :
;;; 	PangoEllipsizeMode
;;; 
;;; Since 2.6
;;; gtk_progress_bar_set_pulse_step ()
;;; 
;;; void                gtk_progress_bar_set_pulse_step     (GtkProgressBar *pbar,
;;;                                                          gdouble fraction);
;;; 
;;; Sets the fraction of total progress bar length to move the bouncing block for each call to gtk_progress_bar_pulse().
;;; 
;;; pbar :
;;; 	a GtkProgressBar
;;; 
;;; fraction :
;;; 	fraction between 0.0 and 1.0
;;; gtk_progress_bar_get_pulse_step ()
;;; 
;;; gdouble             gtk_progress_bar_get_pulse_step     (GtkProgressBar *pbar);
;;; 
;;; Retrieves the pulse step set with gtk_progress_bar_set_pulse_step()
;;; 
;;; pbar :
;;; 	a GtkProgressBar
;;; 
;;; Returns :
;;; 	a fraction from 0.0 to 1.0
;;; Property Details
;;; The "ellipsize" property
;;; 
;;;   "ellipsize"                PangoEllipsizeMode    : Read / Write
;;; 
;;; The preferred place to ellipsize the string, if the progressbar does not have enough room to display the entire string, specified as a PangoEllisizeMode.
;;; 
;;; Note that setting this property to a value other than PANGO_ELLIPSIZE_NONE has the side-effect that the progressbar requests only enough space to display the ellipsis "...". Another means to set a progressbar's width is gtk_widget_set_size_request().
;;; 
;;; Default value: PANGO_ELLIPSIZE_NONE
;;; 
;;; Since 2.6
;;; The "fraction" property
;;; 
;;;   "fraction"                 gdouble               : Read / Write
;;; 
;;; The fraction of total work that has been completed.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0
;;; The "inverted" property
;;; 
;;;   "inverted"                 gboolean              : Read / Write
;;; 
;;; Invert the direction in which the progress bar grows.
;;; 
;;; Default value: FALSE
;;; The "pulse-step" property
;;; 
;;;   "pulse-step"               gdouble               : Read / Write
;;; 
;;; The fraction of total progress to move the bouncing block when pulsed.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.1
;;; The "show-text" property
;;; 
;;;   "show-text"                gboolean              : Read / Write
;;; 
;;; Whether the progress is shown as text.
;;; 
;;; Default value: FALSE
;;; The "text" property
;;; 
;;;   "text"                     gchar*                : Read / Write
;;; 
;;; Text to be displayed in the progress bar.
;;; 
;;; Default value: NULL
;;; Style Property Details
;;; The "min-horizontal-bar-height" style property
;;; 
;;;   "min-horizontal-bar-height" gint                  : Read / Write
;;; 
;;; Minimum horizontal height of the progress bar.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 20
;;; 
;;; Since 2.14
;;; The "min-horizontal-bar-width" style property
;;; 
;;;   "min-horizontal-bar-width" gint                  : Read / Write
;;; 
;;; The minimum horizontal width of the progress bar.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 150
;;; 
;;; Since 2.14
;;; The "min-vertical-bar-height" style property
;;; 
;;;   "min-vertical-bar-height"  gint                  : Read / Write
;;; 
;;; The minimum vertical height of the progress bar.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 80
;;; 
;;; Since 2.14
;;; The "min-vertical-bar-width" style property
;;; 
;;;   "min-vertical-bar-width"   gint                  : Read / Write
;;; 
;;; The minimum vertical width of the progress bar.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 22
;;; 
;;; Since 2.14
;;; The "xspacing" style property
;;; 
;;;   "xspacing"                 gint                  : Read / Write
;;; 
;;; Extra spacing applied to the width of a progress bar.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 7
;;; The "yspacing" style property
;;; 
;;;   "yspacing"                 gint                  : Read / Write
;;; 
;;; Extra spacing applied to the height of a progress bar.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 7

(in-package :gtk)

(defcfun (progress-bar-pulse "gtk_progress_bar_pulse") :void
  (progress-bar (g-object progress-bar)))

(export 'progress-bar-pulse)
