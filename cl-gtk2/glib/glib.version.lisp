;;; ----------------------------------------------------------------------------
;;; glib.version.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2 from
;;; http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GLib 2.30.2 Reference Manual
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
;;; Version Information
;;; 
;;; Variables and functions to check the GLib version
;;; 	
;;; Synopsis
;;; 
;;; #include <glib.h>
;;; 
;;; extern const guint  glib_major_version;
;;; extern const guint  glib_minor_version;
;;; extern const guint  glib_micro_version;
;;; extern const guint  glib_binary_age;
;;; extern const guint  glib_interface_age;
;;; const gchar *       glib_check_version  (guint required_major,
;;;                                          guint required_minor,
;;;                                          guint required_micro);
;;; 
;;; #define             GLIB_MAJOR_VERSION
;;; #define             GLIB_MINOR_VERSION
;;; #define             GLIB_MICRO_VERSION
;;; #define             GLIB_CHECK_VERSION   (major, minor, micro)
;;; 
;;; Description
;;; 
;;; GLib provides version information, primarily useful in configure checks for
;;; builds that have a configure script. Applications will not typically use the
;;; features described here.
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; glib_major_version
;;; 
;;; extern const guint glib_major_version;
;;; ----------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------- 
;;; glib_minor_version
;;; 
;;; extern const guint glib_minor_version;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; glib_micro_version
;;; 
;;; extern const guint glib_micro_version;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; glib_binary_age
;;; 
;;; extern const guint glib_binary_age;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; glib_interface_age
;;; 
;;; extern const guint glib_interface_age;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; glib_check_version ()
;;; 
;;; const gchar * glib_check_version (guint required_major,
;;;                                   guint required_minor,
;;;                                   guint required_micro)
;;; 
;;; Checks that the GLib library in use is compatible with the given version.
;;; Generally you would pass in the constants GLIB_MAJOR_VERSION,
;;; GLIB_MINOR_VERSION, GLIB_MICRO_VERSION as the three arguments to this
;;; function; that produces a check that the library in use is compatible with
;;; the version of GLib the application or module was compiled against.
;;; 
;;; Compatibility is defined by two things: first the version of the running
;;; library is newer than the version
;;; required_major.required_minor.required_micro. Second the running library
;;; must be binary compatible with the version
;;; required_major.required_minor.required_micro (same major version.)
;;; 
;;; required_major :
;;; 	the required major version.
;;; 
;;; required_minor :
;;; 	the required minor version.
;;; 
;;; required_micro :
;;; 	the required micro version.
;;; 
;;; Returns :
;;; 	NULL if the GLib library is compatible with the given version, or a
;;;     string describing the version mismatch. The returned string is owned by
;;;     GLib and must not be modified or freed.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GLIB_MAJOR_VERSION
;;; 
;;; #define GLIB_MAJOR_VERSION 2
;;; 
;;; The major version number of the GLib library.
;;; 
;;; Like glib_major_version, but from the headers used at application compile
;;; time, rather than from the library linked against at application run time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GLIB_MINOR_VERSION
;;; 
;;; #define GLIB_MINOR_VERSION 30
;;; 
;;; The minor version number of the GLib library.
;;; 
;;; Like gtk_minor_version, but from the headers used at application compile
;;; time, rather than from the library linked against at application run time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GLIB_MICRO_VERSION
;;; 
;;; #define GLIB_MICRO_VERSION 2
;;; 
;;; The micro version number of the GLib library.
;;; 
;;; Like gtk_micro_version, but from the headers used at application compile
;;; time, rather than from the library linked against at application run time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GLIB_CHECK_VERSION()
;;; 
;;; #define GLIB_CHECK_VERSION(major,minor,micro)
;;; 
;;; Checks the version of the GLib library that is being compiled against.
;;; 
;;; Example 1. Checking the version of the GLib library
;;; 
;;;  1 if (!GLIB_CHECK_VERSION (1, 2, 0))
;;;  2   g_error ("GLib version 1.2.0 or above is needed");
;;; 
;;; See glib_check_version() for a runtime check.
;;; 
;;; major :
;;; 	the major version to check for
;;; 
;;; minor :
;;; 	the minor version to check for
;;; 
;;; micro :
;;; 	the micro version to check for
;;; 
;;; Returns :
;;; 	TRUE if the version of the GLib header files is the same as or newer
;;;     than the passed-in version.
;;; ----------------------------------------------------------------------------

;;; --- End of file glib.version.lisp ------------------------------------------
