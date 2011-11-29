;;; ----------------------------------------------------------------------------
;;; glib.basic-types.lisp
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
;;; Basic Types
;;; 
;;; Standard GLib types, defined for ease-of-use and portability
;;; 	
;;; Synopsis
;;; 
;;; typedef gboolean;
;;; typedef gpointer;
;;; typedef gconstpointer;
;;; typedef gchar;
;;; typedef guchar;
;;;         
;;; typedef gint;
;;; typedef guint;
;;; typedef gshort;
;;; typedef gushort;
;;; typedef glong;
;;; typedef gulong;
;;;         
;;; typedef gint8;
;;; typedef guint8;
;;; typedef gint16;
;;; typedef guint16;
;;; typedef gint32;
;;; typedef guint32;
;;;         
;;; #define G_HAVE_GINT64
;;; typedef gint64;
;;; typedef guint64;
;;; #define G_GINT64_CONSTANT (val)
;;; #define G_GUINT64_CONSTANT (val)
;;;         
;;; typedef gfloat;
;;; typedef gdouble;
;;;         
;;; typedef gsize;
;;; typedef gssize;
;;; typedef goffset;
;;; #define G_GOFFSET_CONSTANT (val)
;;;         
;;; typedef gintptr;
;;; typedef guintptr;
;;; 
;;; Description
;;; 
;;; GLib defines a number of commonly used types, which can be divided into
;;; 4 groups:
;;; 
;;;     * New types which are not part of standard C (but are defined in
;;;       various C standard library header files) - gboolean, gsize, gssize,
;;;       goffset, gintptr, guintptr.
;;;
;;;     * Integer types which are guaranteed to be the same size across all
;;;       platforms - gint8, guint8, gint16, guint16, gint32, guint32, gint64,
;;;       guint64.
;;;
;;;     * Types which are easier to use than their standard C counterparts -
;;;       gpointer, gconstpointer, guchar, guint, gushort, gulong.
;;;
;;;     * Types which correspond exactly to standard C types, but are included
;;;       for completeness - gchar, gint, gshort, glong, gfloat, gdouble.
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; gboolean
;;; 
;;; typedef gint gboolean;
;;; 
;;; A standard boolean type. Variables of this type should only contain the
;;; value TRUE or FALSE.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gpointer
;;; 
;;; typedef void* gpointer;
;;; 
;;; An untyped pointer. gpointer looks better and is easier to use than void*.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gconstpointer
;;; 
;;; typedef const void *gconstpointer;
;;; 
;;; An untyped pointer to constant data. The data pointed to should not be
;;; changed.
;;; 
;;; This is typically used in function prototypes to indicate that the data
;;; pointed to will not be altered by the function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gchar
;;; 
;;; typedef char   gchar;
;;; 
;;; Corresponds to the standard C char type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; guchar
;;; 
;;; typedef unsigned char   guchar;
;;; 
;;; Corresponds to the standard C unsigned char type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gint
;;; 
;;; typedef int gint;
;;; 
;;; Corresponds to the standard C int type. Values of this type can range from
;;; G_MININT to G_MAXINT.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; guint
;;; 
;;; typedef unsigned int    guint;
;;; 
;;; Corresponds to the standard C unsigned int type. Values of this type can
;;; range from 0 to G_MAXUINT.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gshort
;;; 
;;; typedef short  gshort;
;;; 
;;; Corresponds to the standard C short type. Values of this type can range
;;; from G_MINSHORT to G_MAXSHORT.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gushort
;;; 
;;; typedef unsigned short  gushort;
;;; 
;;; Corresponds to the standard C unsigned short type. Values of this type can
;;; range from 0 to G_MAXUSHORT.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; glong
;;; 
;;; typedef long   glong;
;;; 
;;; Corresponds to the standard C long type. Values of this type can range
;;; from G_MINLONG to G_MAXLONG.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gulong
;;; 
;;; typedef unsigned long   gulong;
;;; 
;;; Corresponds to the standard C unsigned long type. Values of this type can
;;; range from 0 to G_MAXULONG.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gint8
;;; 
;;; typedef signed char gint8;
;;; 
;;; A signed integer guaranteed to be 8 bits on all platforms. Values of this
;;; type can range from G_MININT8 (= -128) to G_MAXINT8 (= 127).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; guint8
;;; 
;;; typedef unsigned char guint8;
;;; 
;;; An unsigned integer guaranteed to be 8 bits on all platforms. Values of
;;; this type can range from 0 to G_MAXUINT8 (= 255).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gint16
;;; 
;;; typedef signed short gint16;
;;; 
;;; A signed integer guaranteed to be 16 bits on all platforms. Values of this
;;; type can range from G_MININT16 (= -32,768) to G_MAXINT16 (= 32,767).
;;; 
;;; To print or scan values of this type, use G_GINT16_MODIFIER and/or
;;; G_GINT16_FORMAT.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; guint16
;;; 
;;; typedef unsigned short guint16;
;;; 
;;; An unsigned integer guaranteed to be 16 bits on all platforms. Values of
;;; this type can range from 0 to G_MAXUINT16 (= 65,535).
;;; 
;;; To print or scan values of this type, use G_GINT16_MODIFIER and/or
;;; G_GUINT16_FORMAT.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gint32
;;; 
;;; typedef signed int gint32;
;;; 
;;; A signed integer guaranteed to be 32 bits on all platforms. Values of this
;;; type can range from G_MININT32 (= -2,147,483,648) to G_MAXINT32
;;; (= 2,147,483,647).
;;; 
;;; To print or scan values of this type, use G_GINT32_MODIFIER and/or
;;; G_GINT32_FORMAT.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; guint32
;;; 
;;; typedef unsigned int guint32;
;;; 
;;; An unsigned integer guaranteed to be 32 bits on all platforms. Values of
;;; this type can range from 0 to G_MAXUINT32 (= 4,294,967,295).
;;; 
;;; To print or scan values of this type, use G_GINT32_MODIFIER and/or
;;; G_GUINT32_FORMAT.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_HAVE_GINT64
;;; 
;;; #define G_HAVE_GINT64 1          /* deprecated, always true */
;;; 
;;; Warning
;;; 
;;; G_HAVE_GINT64 is deprecated and should not be used in newly-written code.
;;; GLib requires 64-bit integer support since version 2.0, therefore
;;; G_HAVE_GINT64 is always defined.
;;; 
;;; This macro is defined if 64-bit signed and unsigned integers are available
;;; on the platform.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gint64
;;; 
;;; typedef signed long gint64;
;;; 
;;; A signed integer guaranteed to be 64 bits on all platforms. Values of this
;;; type can range from G_MININT64 (= -9,223,372,036,854,775,808) to
;;; G_MAXINT64 (= 9,223,372,036,854,775,807).
;;; 
;;; To print or scan values of this type, use G_GINT64_MODIFIER and/or
;;; G_GINT64_FORMAT.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; guint64
;;; 
;;; typedef unsigned long guint64;
;;; 
;;; An unsigned integer guaranteed to be 64 bits on all platforms. Values of
;;; this type can range from 0 to G_MAXUINT64 (= 18,446,744,073,709,551,615).
;;; 
;;; To print or scan values of this type, use G_GINT64_MODIFIER and/or
;;; G_GUINT64_FORMAT.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_GINT64_CONSTANT()
;;; 
;;; #define G_GINT64_CONSTANT(val) (val##L)
;;; 
;;; This macro is used to insert 64-bit integer literals into the source code.
;;; 
;;; val :
;;; 	a literal integer value, e.g. 0x1d636b02300a7aa7.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_GUINT64_CONSTANT()
;;; 
;;; #define G_GUINT64_CONSTANT(val) (val##UL)
;;; 
;;; This macro is used to insert 64-bit unsigned integer literals into the
;;; source code.
;;; 
;;; val :
;;; 	a literal integer value, e.g. 0x1d636b02300a7aa7U.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gfloat
;;; 
;;; typedef float   gfloat;
;;; 
;;; Corresponds to the standard C float type. Values of this type can range
;;; from -G_MAXFLOAT to G_MAXFLOAT.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdouble
;;; 
;;; typedef double  gdouble;
;;; 
;;; Corresponds to the standard C double type. Values of this type can range
;;; from -G_MAXDOUBLE to G_MAXDOUBLE.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsize
;;; 
;;; typedef unsigned long gsize;
;;; 
;;; An unsigned integer type of the result of the sizeof operator, corresponding
;;; to the size_t type defined in C99. This type is wide enough to hold the
;;; numeric value of a pointer, so it is usually 32bit wide on a 32bit platform
;;; and 64bit wide on a 64bit platform. Values of this type can range from 0 to
;;; G_MAXSIZE.
;;; 
;;; To print or scan values of this type, use G_GSIZE_MODIFIER and/or
;;; G_GSIZE_FORMAT.
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cond
    ((cffi-features:cffi-feature-p :x86-64) (defctype gsize :uint64))
    ((cffi-features:cffi-feature-p :x86)    (defctype gsize :ulong))
    ((cffi-features:cffi-feature-p :ppc32)  (defctype gsize :uint32))
    ((cffi-features:cffi-feature-p :ppc64)  (defctype gsize :uint64))
    (t
     (error "Can not define 'gsize', unknown CPU architecture (known are x86 and x86-64)"))))

(export 'gsize)

;;; ----------------------------------------------------------------------------
;;; gssize
;;; 
;;; typedef signed long gssize;
;;; 
;;; A signed variant of gsize, corresponding to the ssize_t defined on most
;;; platforms. Values of this type can range from G_MINSSIZE to G_MAXSSIZE.
;;; 
;;; To print or scan values of this type, use G_GSIZE_MODIFIER and/or
;;; G_GSSIZE_FORMAT.
;;; ----------------------------------------------------------------------------

(defctype gssize :long)

(export 'gssize)

;;; ----------------------------------------------------------------------------
;;; goffset
;;; 
;;; typedef gint64 goffset;
;;; 
;;; A signed integer type that is used for file offsets, corresponding to the
;;; C99 type off64_t. Values of this type can range from G_MINOFFSET to
;;; G_MAXOFFSET.
;;; 
;;; To print or scan values of this type, use G_GOFFSET_MODIFIER and/or
;;; G_GOFFSET_FORMAT.
;;;
;;; Since: 2.14
;;; ----------------------------------------------------------------------------

(defctype goffset :uint64)

(export 'goffset)

;;; ----------------------------------------------------------------------------
;;; G_GOFFSET_CONSTANT()
;;; 
;;; #define G_GOFFSET_CONSTANT(val) G_GINT64_CONSTANT(val)
;;; 
;;; This macro is used to insert goffset 64-bit integer literals into the
;;; source code. See also G_GINT64_CONSTANT.
;;; 
;;; val :
;;; 	a literal integer value, e.g. 0x1d636b02300a7aa7.
;;;
;;; Since: 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gintptr
;;; 
;;; typedef signed long gintptr;
;;; 
;;; Corresponds to the C99 type intptr_t, a signed integer type that can hold
;;; any pointer.
;;; 
;;; To print or scan values of this type, use G_GINTPTR_MODIFIER and/or
;;; G_GINTPTR_FORMAT.
;;;
;;; Since: 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; guintptr
;;; 
;;; typedef unsigned long guintptr;
;;; 
;;; Corresponds to the C99 type uintptr_t, an unsigned integer type that can
;;; hold any pointer.
;;; 
;;; To print or scan values of this type, use G_GINTPTR_MODIFIER and/or
;;; G_GUINTPTR_FORMAT.
;;;
;;; Since: 2.18
;;; ----------------------------------------------------------------------------

;;; --- End of file glib.basic-types.lisp --------------------------------------
