;;; ----------------------------------------------------------------------------
;;; gobject.gvalue.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2 from
;;; http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GObject 2.30.2 Reference Manual
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
;;; Generic values
;;; 
;;; A polymorphic type that can hold values of any other type
;;; 	
;;; Synopsis
;;;
;;;          GValue;
;;; 
;;; #define  G_VALUE_INIT                        
;;; #define  G_VALUE_HOLDS                   (value, type)
;;; #define  G_VALUE_TYPE                    (value)
;;; #define  G_VALUE_TYPE_NAME               (value)
;;; #define  G_TYPE_IS_VALUE                 (type)
;;; #define  G_TYPE_IS_VALUE_ABSTRACT        (type)
;;; #define  G_IS_VALUE                      (value)
;;;                     
;;; #define  G_TYPE_VALUE                    
;;; #define  G_TYPE_VALUE_ARRAY
;;;
;;; GValue * g_value_init                    (GValue *value, GType g_type)
;;; void     g_value_copy                    (const GValue *src_value,
;;;                                           GValue *dest_value)
;;; GValue * g_value_reset                   (GValue *value)
;;; void     g_value_unset                   (GValue *value)
;;; void     g_value_set_instance            (GValue *value, gpointer instance)
;;; gboolean g_value_fits_pointer            (const GValue *value)
;;; gpointer g_value_peek_pointer            (const GValue *value)
;;; gboolean g_value_type_compatible         (GType src_type, GType dest_type)
;;; gboolean g_value_type_transformable      (GType src_type, GType dest_type)
;;; gboolean g_value_transform               (const GValue *src_value,
;;;                                           GValue *dest_value)
;;; void     (*GValueTransform)              (const GValue *src_value,
;;;                                           GValue *dest_value)
;;; void     g_value_register_transform_func (GType src_type,
;;;                                           GType dest_type,
;;;                                           GValueTransform transform_func)
;;; gchar *  g_strdup_value_contents         (const GValue *value)
;;;
;;; Description
;;; 
;;; The GValue structure is basically a variable container that consists of a
;;; type identifier and a specific value of that type. The type identifier
;;; within a GValue structure always determines the type of the associated
;;; value. To create a undefined GValue structure, simply create a zero-filled
;;; GValue structure. To initialize the GValue, use the g_value_init() function.
;;; A GValue cannot be used until it is initialized. The basic type operations
;;; (such as freeing and copying) are determined by the GTypeValueTable
;;; associated with the type ID stored in the GValue. Other GValue operations
;;; (such as converting values between types) are provided by this interface.
;;; 
;;; The code in the example program below demonstrates GValue's features.
;;; 
;;;  1 #include <glib-object.h>
;;;  2
;;;  3 static void
;;;  4 int2string (const GValue *src_value,
;;;  5             GValue       *dest_value)
;;;  6 {
;;;  7   if (g_value_get_int (src_value) == 42)
;;;  8     g_value_set_static_string (dest_value, "An important number");
;;;  9   else
;;; 10     g_value_set_static_string (dest_value, "What's that?");
;;; 11 }
;;; 12
;;; 13 int
;;; 14 main (int   argc,
;;; 15       char *argv[])
;;; 16 {
;;; 17   /* GValues must be initialized */
;;; 18   GValue a = G_VALUE_INIT;
;;; 19   GValue b = G_VALUE_INIT;
;;; 20   const gchar *message;
;;; 21
;;; 22   g_type_init ();
;;; 23 
;;; 24   /* The GValue starts empty */
;;; 25   g_assert (!G_VALUE_HOLDS_STRING (&a));
;;; 26
;;; 27   /* Put a string in it */
;;; 28   g_value_init (&a, G_TYPE_STRING);
;;; 29   g_assert (G_VALUE_HOLDS_STRING (&a));
;;; 30   g_value_set_static_string (&a, "Hello, world!");
;;; 31   g_printf ("%s\n", g_value_get_string (&a));
;;; 32
;;; 33   /* Reset it to its pristine state */
;;; 34   g_value_unset (&a);
;;; 35
;;; 36   /* It can then be reused for another type */
;;; 37   g_value_init (&a, G_TYPE_INT);
;;; 38   g_value_set_int (&a, 42);
;;; 39
;;; 40   /* Attempt to transform it into a GValue of type STRING */
;;; 41   g_value_init (&b, G_TYPE_STRING);
;;; 42
;;; 43   /* An INT is transformable to a STRING */
;;; 44   g_assert (g_value_type_transformable (G_TYPE_INT, G_TYPE_STRING));
;;; 45 
;;; 46   g_value_transform (&a, &b);
;;; 47   g_printf ("%s\n", g_value_get_string (&b));
;;; 48
;;; 49   /* Attempt to transform it again using a custom transform function */
;;; 50   g_value_register_transform_func (G_TYPE_INT, G_TYPE_STRING, int2string);
;;; 51   g_value_transform (&a, &b);
;;; 52   g_printf ("%s\n", g_value_get_string (&b));
;;; 53   return 0;
;;; 54 }
;;; ---------------------------------------------------------------------------- 

(in-package :gobject)

;; Initializes the GValue in \"unset\" state.
;;    g-value - a C pointer to the GValue structure
(defun g-value-zero (g-value)
  (loop
     for i from 0 below (foreign-type-size 'g-value)
     do (setf (mem-ref g-value :uchar i) 0)))

(defmacro ev-case (keyform &body clauses)
  "Macro that is an analogue of CASE except that it evaluates keyforms"
  (let ((value (gensym)))
    `(let ((,value ,keyform))
       (cond
         ,@(loop
              for (key . forms) in clauses
              collect
                (if (eq key t)
                    `(t ,@forms)
                    `((equalp ,key ,value) ,@forms)))))))

(defgeneric parse-g-value-for-type (gvalue-ptr gtype parse-kind))

(defmethod parse-g-value-for-type :around (gvalue-ptr gtype parse-kind)
  (assert (typep gtype '(or gtype nil)))
  (call-next-method))

(defmethod parse-g-value-for-type (gvalue-ptr gtype parse-kind)
  (if (eq gtype (g-type-fundamental gtype))
      (call-next-method)
      (parse-g-value-for-type gvalue-ptr
                              (g-type-fundamental gtype)
                              parse-kind)))

;; Parses the GValue structure and returns the corresponding Lisp object.
;;    value  - a C pointer to the GValue structure
;;    return - value contained in the GValue structure. Type of value depends
;;             on GValue type
(defun parse-g-value (gvalue &key (parse-kind :get-property))
  (let* ((type (g-value-type gvalue))
         (fundamental-type (g-type-fundamental type)))
    (ev-case fundamental-type
      ((gtype +g-type-invalid+) (error "GValue is of invalid type (~A)"
                                       (gtype-name type)))
      ((gtype +g-type-void+) nil)
      ((gtype +g-type-char+) (g-value-get-char gvalue))
      ((gtype +g-type-uchar+) (g-value-get-uchar gvalue))
      ((gtype +g-type-boolean+) (g-value-get-boolean gvalue))
      ((gtype +g-type-int+) (g-value-get-int gvalue))
      ((gtype +g-type-uint+) (g-value-get-uint gvalue))
      ((gtype +g-type-long+) (g-value-get-long gvalue))
      ((gtype +g-type-ulong+) (g-value-get-ulong gvalue))
      ((gtype +g-type-int64+) (g-value-get-int64 gvalue))
      ((gtype +g-type-uint64+) (g-value-get-uint64 gvalue))
      ((gtype +g-type-enum+) (parse-g-value-enum gvalue))
      ((gtype +g-type-flags+) (parse-g-value-flags gvalue))
      ((gtype +g-type-float+) (g-value-get-float gvalue))
      ((gtype +g-type-double+) (g-value-get-double gvalue))
      ((gtype +g-type-string+) (g-value-get-string gvalue))
      (t (parse-g-value-for-type gvalue type parse-kind)))))

(defmethod parse-g-value-for-type (gvalue-ptr
                                   (type (eql (gtype +g-type-pointer+)))
                                   parse-kind)
  (declare (ignore parse-kind))
  (g-value-get-pointer gvalue-ptr))

(defmethod parse-g-value-for-type (gvalue-ptr
                                   (type (eql (gtype +g-type-param+)))
                                   parse-kind)
  (declare (ignore parse-kind))
  (parse-g-param-spec (g-value-get-param gvalue-ptr)))

(defgeneric set-gvalue-for-type (gvalue-ptr type value))

(defmethod set-gvalue-for-type :around (gvalue-ptr type value)
  (assert (typep type '(or gtype null)))
  (call-next-method))

(defmethod set-gvalue-for-type (gvalue-ptr type value)
  (if (eq type (g-type-fundamental type))
      (call-next-method)
      (set-gvalue-for-type gvalue-ptr (g-type-fundamental type) value)))

;; Assigns the GValue structure gvalue the value value of GType type.
;;   gvalue        - a C pointer to the GValue structure
;;   value         - a Lisp object that is to be assigned
;;   type          - a GType that is to be assigned
;;   zero-g-value  - a boolean specifying whether GValue should be
;;                   zero-initialized before assigning. See @fun{g-value-zero
;;   unset-g-value - a boolean specifying whether GValue should be \"unset\"
;;                   before assigning. See g-value-unset. The \"true\" value
;;                   should not be passed to both zero-g-value and unset-g-value
;;                   arguments
;;   g-value-init  - a boolean specifying where GValue should be initialized
(defun set-g-value (gvalue value type &key zero-g-value
                                           unset-g-value
                                           (g-value-init t))
  (setf type (gtype type))
  (cond (zero-g-value (g-value-zero gvalue))
        (unset-g-value (g-value-unset gvalue)))
  (when g-value-init (g-value-init gvalue type))
  (let ((fundamental-type (g-type-fundamental type)))
    (ev-case fundamental-type
      ((gtype +g-type-invalid+) (error "Invalid type (~A)" type))
      ((gtype +g-type-void+) nil)
      ((gtype +g-type-char+) (g-value-set-char gvalue value))
      ((gtype +g-type-uchar+) (g-value-set-uchar gvalue value))
      ((gtype +g-type-boolean+) (g-value-set-boolean gvalue value))
      ((gtype +g-type-int+) (g-value-set-int gvalue value))
      ((gtype +g-type-uint+) (g-value-set-uint gvalue value))
      ((gtype +g-type-long+) (g-value-set-long gvalue value))
      ((gtype +g-type-ulong+) (g-value-set-ulong gvalue value))
      ((gtype +g-type-int64+) (g-value-set-int64 gvalue value))
      ((gtype +g-type-uint64+) (g-value-set-uint64 gvalue value))
      ((gtype +g-type-enum+) (set-gvalue-enum gvalue value))
      ((gtype +g-type-flags+) (set-gvalue-flags gvalue value))
      ((gtype +g-type-float+)
       (unless (realp value) (error "~A is not a real number" value))
       (g-value-set-float gvalue (coerce value 'single-float)))
      ((gtype +g-type-double+)
       (unless (realp value) (error "~A is not a real number" value))
       (g-value-set-double gvalue (coerce value 'double-float)))
      ((gtype +g-type-string+) (g-value-set-string gvalue value))
      (t (set-gvalue-for-type gvalue type value)))))

(defmethod set-gvalue-for-type (gvalue-ptr
                                (type (eql (gtype +g-type-pointer+)))
                                value)
  (g-value-set-pointer gvalue-ptr value))

(defmethod set-gvalue-for-type (gvalue-ptr
                                (type (eql (gtype +g-type-param+)))
                                value)
  (declare (ignore gvalue-ptr value))
  (error "Setting of GParam is not implemented"))

;;Enums

(defvar *registered-enum-types* (make-hash-table :test 'equal))

(defun register-enum-type (name type)
  (setf (gethash name *registered-enum-types*) type))

(defun registered-enum-type (name)
  (gethash name *registered-enum-types*))

(defun parse-g-value-enum (gvalue)
  (let* ((g-type (g-value-type gvalue))
         (type-name (gtype-name g-type))
         (enum-type (registered-enum-type type-name)))
    (unless enum-type
      (error "Enum ~A is not registered" type-name))
    (convert-from-foreign (g-value-get-enum gvalue) enum-type)))

(defun set-gvalue-enum (gvalue value)
  (let* ((g-type (g-value-type gvalue))
         (type-name (gtype-name g-type))
         (enum-type (registered-enum-type type-name)))
    (unless enum-type
      (error "Enum ~A is not registered" type-name))
    (g-value-set-enum gvalue (convert-to-foreign value enum-type))))

;;Flags

(defvar *registered-flags-types* (make-hash-table :test 'equal))

(defun register-flags-type (name type)
  (setf (gethash name *registered-flags-types*) type))

(defun registered-flags-type (name)
  (gethash name *registered-flags-types*))

(defun parse-g-value-flags (gvalue)
  (let* ((g-type (g-value-type gvalue))
         (type-name (gtype-name g-type))
         (flags-type (registered-flags-type type-name)))
    (unless flags-type
      (error "Flags ~A is not registered" type-name))
    (convert-from-foreign (g-value-get-flags gvalue) flags-type)))

(defun set-gvalue-flags (gvalue value)
  (let* ((g-type (g-value-type gvalue))
         (type-name (gtype-name g-type))
         (flags-type (registered-flags-type type-name)))
    (unless flags-type
      (error "Flags ~A is not registered" type-name))
    (g-value-set-flags gvalue (convert-to-foreign value flags-type))))

;;; ----------------------------------------------------------------------------
;;; GValue
;;; 
;;; typedef struct {
;;; } GValue;
;;; 
;;; An opaque structure used to hold different types of values. The data within
;;; the structure has protected scope: it is accessible only to functions within
;;; a GTypeValueTable structure, or implementations of the g_value_*() API. That
;;; is, code portions which implement new fundamental types. GValue users cannot
;;; make any assumptions about how data is stored within the 2 element data
;;; union, and the g_type member should only be accessed through the
;;; G_VALUE_TYPE() macro.
;;; ----------------------------------------------------------------------------

(defcunion g-value-data
  (:int :int)
  (:uint :uint)
  (:long :long)
  (:ulong :ulong)
  (:int64 :int64)
  (:uint64 :uint64)
  (:float :float)
  (:double :double)
  (:pointer :pointer))

(defcstruct g-value
  (:type g-type-designator)
  (:data g-value-data :count 2))

;;; ----------------------------------------------------------------------------
;;; G_VALUE_INIT
;;; 
;;; #define G_VALUE_INIT  { 0, { { 0 } } }
;;; 
;;; A GValue must be initialized before it can be used. This macro can be used
;;; as initializer instead of an explicit { 0 } when declaring a variable, but
;;; it cannot be assigned to a variable.
;;; 
;;;  1 GValue value = G_VALUE_INIT;
;;; 
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS()
;;; 
;;; #define G_VALUE_HOLDS(value,type)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), (type)))
;;; 
;;; Checks if value holds (or contains) a value of type. This macro will also
;;; check for value != NULL and issue a warning if the check fails.
;;; 
;;; value :
;;; 	A GValue structure.
;;; 
;;; type :
;;; 	A GType value.
;;; 
;;; Returns :
;;; 	TRUE if value holds the type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_TYPE()
;;; 
;;; #define G_VALUE_TYPE(value) (((GValue*) (value))->g_type)
;;; 
;;; Get the type identifier of value.
;;; 
;;; value :
;;; 	A GValue structure.
;;; 
;;; Returns :
;;; 	the GType.
;;; ----------------------------------------------------------------------------

(defun g-value-type (value)
  (foreign-slot-value value 'g-value :type))

;;; ----------------------------------------------------------------------------
;;; G_VALUE_TYPE_NAME()
;;; 
;;; #define G_VALUE_TYPE_NAME(value) (g_type_name (G_VALUE_TYPE (value)))
;;; 
;;; Gets the the type name of value.
;;; 
;;; value :
;;; 	A GValue structure.
;;; 
;;; Returns :
;;; 	the type name.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_VALUE()
;;; 
;;; #define G_TYPE_IS_VALUE(type) (g_type_check_is_value_type (type))
;;; 
;;; Checks whether the passed in type ID can be used for g_value_init().
;;; That is, this macro checks whether this type provides an implementation of
;;; the GTypeValueTable functions required for a type to create a GValue of.
;;; 
;;; type :
;;; 	A GType value.
;;; 
;;; Returns :
;;; 	Whether type is suitable as a GValue type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_VALUE_ABSTRACT()
;;; 
;;; #define G_TYPE_IS_VALUE_ABSTRACT(type)
;;;         (g_type_test_flags ((type), G_TYPE_FLAG_VALUE_ABSTRACT))
;;; 
;;; Checks if type is an abstract value type. An abstract value type introduces
;;; a value table, but can't be used for g_value_init() and is normally used as
;;; an abstract base type for derived value types.
;;; 
;;; type :
;;; 	A GType value.
;;; 
;;; Returns :
;;; 	TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_VALUE()
;;; 
;;; #define G_IS_VALUE(value) (G_TYPE_CHECK_VALUE (value))
;;; 
;;; Checks if value is a valid and initialized GValue structure.
;;; 
;;; value :
;;; 	A GValue structure.
;;; 
;;; Returns :
;;; 	TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_VALUE
;;; 
;;; #define G_TYPE_VALUE (g_value_get_type ())
;;; 
;;; The type ID of the "GValue" type which is a boxed type, used to pass around
;;; pointers to GValues.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_VALUE_ARRAY
;;; 
;;; #define G_TYPE_VALUE_ARRAY (g_value_array_get_type ())
;;; 
;;; The type ID of the "GValueArray" type which is a boxed type, used to pass
;;; around pointers to GValueArrays.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_init ()
;;; 
;;; GValue * g_value_init (GValue *value, GType g_type)
;;; 
;;; Initializes value with the default value of type.
;;; 
;;; value :
;;; 	A zero-filled (uninitialized) GValue structure.
;;; 
;;; g_type :
;;; 	Type the GValue should hold values of.
;;; 
;;; Returns :
;;; 	the GValue structure that has been passed in. [transfer none]
;;; ----------------------------------------------------------------------------

(defcfun g-value-init (:pointer g-value)
  (value (:pointer g-value))
  (type g-type-designator))

;;; ----------------------------------------------------------------------------
;;; g_value_copy ()
;;; 
;;; void g_value_copy (const GValue *src_value, GValue *dest_value)
;;; 
;;; Copies the value of src_value into dest_value.
;;; 
;;; src_value :
;;; 	An initialized GValue structure.
;;; 
;;; dest_value :
;;; 	An initialized GValue structure of the same type as src_value.
;;; ----------------------------------------------------------------------------

(defcfun g-value-copy :void
  (src-value (:pointer g-value))
  (dst-value (:pointer g-value)))

;;; ----------------------------------------------------------------------------
;;; g_value_reset ()
;;; 
;;; GValue * g_value_reset (GValue *value)
;;; 
;;; Clears the current value in value and resets it to the default value (as if
;;; the value had just been initialized).
;;; 
;;; value :
;;; 	An initialized GValue structure.
;;; 
;;; Returns :
;;; 	the GValue structure that has been passed in
;;; ----------------------------------------------------------------------------

(defcfun g-value-reset (:pointer g-value)
  (value (:pointer g-value)))

;;; ----------------------------------------------------------------------------
;;; g_value_unset ()
;;; 
;;; void g_value_unset (GValue *value)
;;; 
;;; Clears the current value in value and "unsets" the type, this releases all
;;; resources associated with this GValue. An unset value is the same as an
;;; uninitialized (zero-filled) GValue structure.
;;; 
;;; value :
;;; 	An initialized GValue structure.
;;; ----------------------------------------------------------------------------

(defcfun g-value-unset (:pointer g-value)
  (value (:pointer g-value)))

;;; ----------------------------------------------------------------------------
;;; g_value_set_instance ()
;;; 
;;; void g_value_set_instance (GValue *value, gpointer instance)
;;; 
;;; Sets value from an instantiatable type via the value_table's collect_value()
;;; function.
;;; 
;;; value :
;;; 	An initialized GValue structure.
;;; 
;;; instance :
;;; 	the instance. [allow-none]
;;; ----------------------------------------------------------------------------

(defcfun g-value-set-instance :void
  (value (:pointer g-value))
  (instance :pointer))

;;; ----------------------------------------------------------------------------
;;; g_value_fits_pointer ()
;;; 
;;; gboolean g_value_fits_pointer (const GValue *value)
;;; 
;;; Determines if value will fit inside the size of a pointer value. This is an
;;; internal function introduced mainly for C marshallers.
;;; 
;;; value :
;;; 	An initialized GValue structure.
;;; 
;;; Returns :
;;; 	TRUE if value will fit inside a pointer value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_peek_pointer ()
;;; 
;;; gpointer g_value_peek_pointer (const GValue *value)
;;; 
;;; value :
;;; 	An initialized GValue structure.
;;; 
;;; Returns :
;;; 	the value contents as pointer. This function asserts that
;;; g_value_fits_pointer() returned TRUE for the passed in value. This is an
;;; internal function introduced mainly for C marshallers. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_type_compatible ()
;;; 
;;; gboolean g_value_type_compatible (GType src_type, GType dest_type)
;;; 
;;; Returns whether a GValue of type src_type can be copied into a GValue of
;;; type dest_type.
;;; 
;;; src_type :
;;; 	source type to be copied.
;;; 
;;; dest_type :
;;; 	destination type for copying.
;;; 
;;; Returns :
;;; 	TRUE if g_value_copy() is possible with src_type and dest_type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_type_transformable ()
;;; 
;;; gboolean g_value_type_transformable (GType src_type, GType dest_type)
;;; 
;;; Check whether g_value_transform() is able to transform values of type
;;; src_type into values of type dest_type.
;;; 
;;; src_type :
;;; 	Source type.
;;; 
;;; dest_type :
;;; 	Target type.
;;; 
;;; Returns :
;;; 	TRUE if the transformation is possible, FALSE otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_transform ()
;;; 
;;; gboolean g_value_transform (const GValue *src_value, GValue *dest_value)
;;; 
;;; Tries to cast the contents of src_value into a type appropriate to store in
;;; dest_value, e.g. to transform a G_TYPE_INT value into a G_TYPE_FLOAT value.
;;; Performing transformations between value types might incur precision
;;; lossage. Especially transformations into strings might reveal seemingly
;;; arbitrary results and shouldn't be relied upon for production code (such as
;;; rcfile value or object property serialization).
;;; 
;;; src_value :
;;; 	Source value.
;;; 
;;; dest_value :
;;; 	Target value.
;;; 
;;; Returns :
;;; 	Whether a transformation rule was found and could be applied. Upon
;;;     failing transformations, dest_value is left untouched.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GValueTransform ()
;;; 
;;; void (*GValueTransform) (const GValue *src_value, GValue *dest_value)
;;; 
;;; The type of value transformation functions which can be registered with
;;; g_value_register_transform_func().
;;; 
;;; src_value :
;;; 	Source value.
;;; 
;;; dest_value :
;;; 	Target value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_register_transform_func ()
;;; 
;;; void g_value_register_transform_func (GType src_type,
;;;                                       GType dest_type,
;;;                                       GValueTransform transform_func)
;;; 
;;; Registers a value transformation function for use in g_value_transform().
;;; A previously registered transformation function for src_type and dest_type
;;; will be replaced.
;;; 
;;; src_type :
;;; 	Source type.
;;; 
;;; dest_type :
;;; 	Target type.
;;; 
;;; transform_func :
;;; 	a function which transforms values of type src_type into value of type
;;;     dest_type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_strdup_value_contents ()
;;; 
;;; gchar * g_strdup_value_contents (const GValue *value)
;;; 
;;; Return a newly allocated string, which describes the contents of a GValue.
;;; The main purpose of this function is to describe GValue contents for
;;; debugging output, the way in which the contents are described may change
;;; between different GLib versions.
;;; 
;;; value :
;;; 	GValue which contents are to be described.
;;; 
;;; Returns :
;;; 	Newly allocated string.
;;; ----------------------------------------------------------------------------

(defcfun g-strdup-value-contents :string
  (value (:pointer g-value)))

;;; --- End of file gobject.gvalue.lisp  ---------------------------------------
