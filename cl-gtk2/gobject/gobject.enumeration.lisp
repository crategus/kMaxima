;;; ----------------------------------------------------------------------------
;;; gobject.enumeration.lisp
;;;
;;; Copyright (C) 2011, 2012 Dr. Dieter Kaiser
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
;;; Enumeration and Flag Types
;;; 
;;; Enumeration and flags types
;;; 	
;;; Synopsis
;;; 
;;; struct              g-enum-value
;;; struct              g-flags-value
;;; struct              g-enum-class
;;; struct              g-flags-class
;;;
;;; #define             G_ENUM_CLASS_TYPE                   (class)
;;; #define             G_ENUM_CLASS_TYPE_NAME              (class)
;;; #define             G_TYPE_IS_ENUM                      (type)
;;; #define             G_ENUM_CLASS                        (class)
;;; #define             G_IS_ENUM_CLASS                     (class)
;;; #define             G_TYPE_IS_FLAGS                     (type)
;;; #define             G_FLAGS_CLASS                       (class)
;;; #define             G_IS_FLAGS_CLASS                    (class)
;;; #define             G_FLAGS_CLASS_TYPE                  (class)
;;; #define             G_FLAGS_CLASS_TYPE_NAME             (class)
;;;
;;; struct              GFlagsValue;
;;; GEnumValue *        g_enum_get_value                    (GEnumClass *enum_class,
;;;                                                          gint value);
;;; GEnumValue *        g_enum_get_value_by_name            (GEnumClass *enum_class,
;;;                                                          const gchar *name);
;;; GEnumValue *        g_enum_get_value_by_nick            (GEnumClass *enum_class,
;;;                                                          const gchar *nick);
;;; GFlagsValue *       g_flags_get_first_value             (GFlagsClass *flags_class,
;;;                                                          guint value);
;;; GFlagsValue *       g_flags_get_value_by_name           (GFlagsClass *flags_class,
;;;                                                          const gchar *name);
;;; GFlagsValue *       g_flags_get_value_by_nick           (GFlagsClass *flags_class,
;;;                                                          const gchar *nick);
;;; GType               g_enum_register_static              (const gchar *name,
;;;                                                          const GEnumValue *const_static_values);
;;; GType               g_flags_register_static             (const gchar *name,
;;;                                                          const GFlagsValue *const_static_values);
;;; void                g_enum_complete_type_info           (GType g_enum_type,
;;;                                                          GTypeInfo *info,
;;;                                                          const GEnumValue *const_values);
;;; void                g_flags_complete_type_info          (GType g_flags_type,
;;;                                                          GTypeInfo *info,
;;;                                                          const GFlagsValue *const_values);
;;; 
;;; Description
;;; 
;;; The GLib type system provides fundamental types for enumeration and flags
;;; types. (Flags types are like enumerations, but allow their values to be
;;; combined by bitwise or). A registered enumeration or flags type associates
;;; a name and a nickname with each allowed value, and the methods
;;; g_enum_get_value_by_name(), g_enum_get_value_by_nick(),
;;; g_flags_get_value_by_name() and g_flags_get_value_by_nick() can look up
;;; values by their name or nickname. When an enumeration or flags type is
;;; registered with the GLib type system, it can be used as value type for
;;; object properties, using g_param_spec_enum() or g_param_spec_flags().
;;; 
;;; GObject ships with a utility called glib-mkenums that can construct
;;; suitable type registration functions from C enumeration definitions.
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------
;;; struct GEnumValue
;;; 
;;; struct GEnumValue {
;;;   gint	 value;
;;;   const gchar *value_name;
;;;   const gchar *value_nick;
;;; };
;;; 
;;; A structure which contains a single enum value, its name, and its nickname.
;;; 
;;; gint value;
;;; 	the enum value
;;; 
;;; const gchar *value_name;
;;; 	the name of the value
;;; 
;;; const gchar *value_nick;
;;; 	the nickname of the value
;;; ----------------------------------------------------------------------------

(defcstruct g-enum-value
  (:value :int)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))

;;; ----------------------------------------------------------------------------
;;; struct GFlagsValue
;;; 
;;; struct GFlagsValue {
;;;   guint	 value;
;;;   const gchar *value_name;
;;;   const gchar *value_nick;
;;; };
;;; 
;;; A structure which contains a single flags value, its name, and its nickname.
;;; 
;;; guint value;
;;; 	the flags value
;;; 
;;; const gchar *value_name;
;;; 	the name of the value
;;; 
;;; const gchar *value_nick;
;;; 	the nickname of the value
;;; ----------------------------------------------------------------------------

(defcstruct g-flags-value
  (:value :uint)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))

;;; ----------------------------------------------------------------------------
;;; struct GEnumClass
;;; 
;;; struct GEnumClass {
;;;   GTypeClass  g_type_class;
;;; 
;;;   gint	      minimum;
;;;   gint	      maximum;
;;;   guint	      n_values;
;;;   GEnumValue *values;
;;; };
;;; 
;;; The class of an enumeration type holds information about its possible
;;; values.
;;; 
;;; GTypeClass g_type_class;
;;; 	the parent class
;;; 
;;; gint minimum;
;;; 	the smallest possible value.
;;; 
;;; gint maximum;
;;; 	the largest possible value.
;;; 
;;; guint n_values;
;;; 	the number of possible values.
;;; 
;;; GEnumValue *values;
;;; 	an array of GEnumValue structs describing the individual values.
;;; ----------------------------------------------------------------------------

(defcstruct g-enum-class
  (:type-class g-type-class)
  (:minimum :int)
  (:maximum :int)
  (:n-values :uint)
  (:values (:pointer g-enum-value)))

;;; ----------------------------------------------------------------------------
;;; struct GFlagsClass
;;; 
;;; struct GFlagsClass {
;;;   GTypeClass   g_type_class;
;;;   
;;;   guint	       mask;
;;;   guint	       n_values;
;;;   GFlagsValue *values;
;;; };
;;; 
;;; The class of a flags type holds information about its possible values.
;;; 
;;; GTypeClass g_type_class;
;;; 	the parent class
;;; 
;;; guint mask;
;;; 	a mask covering all possible values.
;;; 
;;; guint n_values;
;;; 	the number of possible values.
;;; 
;;; GFlagsValue *values;
;;; 	an array of GFlagsValue structs describing the individual values.
;;; ----------------------------------------------------------------------------

(defcstruct g-flags-class
  (:type-class g-type-class)
  (:mask :uint)
  (:n-values :uint)
  (:values (:pointer g-flags-value)))

;;; ----------------------------------------------------------------------------
;;; G_ENUM_CLASS_TYPE()
;;; 
;;; #define G_ENUM_CLASS_TYPE(class) (G_TYPE_FROM_CLASS (class))
;;; 
;;; Get the type identifier from a given GEnumClass structure.
;;; 
;;; class :
;;; 	a GEnumClass
;;; 
;;; Returns :
;;; 	the GType
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_ENUM_CLASS_TYPE_NAME()
;;; 
;;; #define G_ENUM_CLASS_TYPE_NAME(class)
;;;         (g_type_name (G_ENUM_CLASS_TYPE (class)))
;;; 
;;; Get the static type name from a given GEnumClass structure.
;;; 
;;; class :
;;; 	a GEnumClass
;;; 
;;; Returns :
;;; 	the type name.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_ENUM()
;;; 
;;; #define G_TYPE_IS_ENUM(type) (G_TYPE_FUNDAMENTAL (type) == G_TYPE_ENUM)
;;; 
;;; Checks whether type "is a" G_TYPE_ENUM.
;;; 
;;; type :
;;; 	a GType ID.
;;; 
;;; Returns :
;;; 	TRUE if type "is a" G_TYPE_ENUM.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_ENUM_CLASS()
;;; 
;;; #define G_ENUM_CLASS(class) (G_TYPE_CHECK_CLASS_CAST ((class),
;;;                              G_TYPE_ENUM, GEnumClass))
;;; 
;;; Casts a derived GEnumClass structure into a GEnumClass structure.
;;; 
;;; class :
;;; 	a valid GEnumClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_ENUM_CLASS()
;;; 
;;; #define G_IS_ENUM_CLASS(class) (G_TYPE_CHECK_CLASS_TYPE ((class),
;;;                                 G_TYPE_ENUM))
;;; 
;;; Checks whether class "is a" valid GEnumClass structure of type G_TYPE_ENUM
;;; or derived.
;;; 
;;; class :
;;; 	a GEnumClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_FLAGS()
;;; 
;;; #define G_TYPE_IS_FLAGS(type) (G_TYPE_FUNDAMENTAL (type) == G_TYPE_FLAGS)
;;; 
;;; Checks whether type "is a" G_TYPE_FLAGS.
;;; 
;;; type :
;;; 	a GType ID.
;;; 
;;; Returns :
;;; 	TRUE if type "is a" G_TYPE_FLAGS.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS()
;;; 
;;; #define G_FLAGS_CLASS(class) (G_TYPE_CHECK_CLASS_CAST ((class),
;;;                               G_TYPE_FLAGS, GFlagsClass))
;;; 
;;; Casts a derived GFlagsClass structure into a GFlagsClass structure.
;;; 
;;; class :
;;; 	a valid GFlagsClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_FLAGS_CLASS()
;;; 
;;; #define G_IS_FLAGS_CLASS(class) (G_TYPE_CHECK_CLASS_TYPE ((class),
;;;                                  G_TYPE_FLAGS))
;;; 
;;; Checks whether class "is a" valid GFlagsClass structure of type
;;; G_TYPE_FLAGS or derived.
;;; 
;;; class :
;;; 	a GFlagsClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS_TYPE()
;;; 
;;; #define G_FLAGS_CLASS_TYPE(class) (G_TYPE_FROM_CLASS (class))
;;; 
;;; Get the type identifier from a given GFlagsClass structure.
;;; 
;;; class :
;;; 	a GFlagsClass
;;; 
;;; Returns :
;;; 	the GType
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS_TYPE_NAME()
;;; 
;;; #define G_FLAGS_CLASS_TYPE_NAME(class)
;;;         (g_type_name (G_FLAGS_CLASS_TYPE (class)))
;;; 
;;; Get the static type name from a given GFlagsClass structure.
;;; 
;;; class :
;;; 	a GFlagsClass
;;; 
;;; Returns :
;;; 	the type name.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_get_value ()
;;; 
;;; GEnumValue * g_enum_get_value (GEnumClass *enum_class, gint value);
;;; 
;;; Returns the GEnumValue for a value.
;;; 
;;; enum_class :
;;; 	a GEnumClass
;;; 
;;; value :
;;; 	the value to look up
;;; 
;;; Returns :
;;; 	the GEnumValue for value, or NULL if value is not a member of the
;;;     enumeration
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_get_value_by_name ()
;;; 
;;; GEnumValue * g_enum_get_value_by_name (GEnumClass *enum_class,
;;;                                        const gchar *name);
;;; 
;;; Looks up a GEnumValue by name.
;;; 
;;; enum_class :
;;; 	a GEnumClass
;;; 
;;; name :
;;; 	the name to look up
;;; 
;;; Returns :
;;; 	the GEnumValue with name name, or NULL if the enumeration doesn't have
;;;     a member with that name
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_get_value_by_nick ()
;;; 
;;; GEnumValue * g_enum_get_value_by_nick (GEnumClass *enum_class,
;;;                                        const gchar *nick);
;;; 
;;; Looks up a GEnumValue by nickname.
;;; 
;;; enum_class :
;;; 	a GEnumClass
;;; 
;;; nick :
;;; 	the nickname to look up
;;; 
;;; Returns :
;;; 	the GEnumValue with nickname nick, or NULL if the enumeration doesn't
;;;     have a member with that nickname
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_get_first_value ()
;;; 
;;; GFlagsValue * g_flags_get_first_value (GFlagsClass *flags_class,
;;;                                        guint value);
;;; 
;;; Returns the first GFlagsValue which is set in value.
;;; 
;;; flags_class :
;;; 	a GFlagsClass
;;; 
;;; value :
;;; 	the value
;;; 
;;; Returns :
;;; 	the first GFlagsValue which is set in value, or NULL if none is set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_get_value_by_name ()
;;; 
;;; GFlagsValue * g_flags_get_value_by_name (GFlagsClass *flags_class,
;;;                                          const gchar *name);
;;; 
;;; Looks up a GFlagsValue by name.
;;; 
;;; flags_class :
;;; 	a GFlagsClass
;;; 
;;; name :
;;; 	the name to look up
;;; 
;;; Returns :
;;; 	the GFlagsValue with name name, or NULL if there is no flag with that
;;;     name
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_get_value_by_nick ()
;;; 
;;; GFlagsValue * g_flags_get_value_by_nick (GFlagsClass *flags_class,
;;;                                          const gchar *nick);
;;; 
;;; Looks up a GFlagsValue by nickname.
;;; 
;;; flags_class :
;;; 	a GFlagsClass
;;; 
;;; nick :
;;; 	the nickname to look up
;;; 
;;; Returns :
;;; 	the GFlagsValue with nickname nick, or NULL if there is no flag with
;;;     that nickname
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_register_static ()
;;; 
;;; GType g_enum_register_static (const gchar *name,
;;;                               const GEnumValue *const_static_values);
;;; 
;;; Registers a new static enumeration type with the name name.
;;; 
;;; It is normally more convenient to let glib-mkenums generate a
;;; my_enum_get_type() function from a usual C enumeration definition than to
;;; write one yourself using g_enum_register_static().
;;; 
;;; name :
;;; 	A nul-terminated string used as the name of the new type.
;;; 
;;; const_static_values :
;;; 	An array of GEnumValue structs for the possible enumeration values.
;;;     The array is terminated by a struct with all members being 0. GObject
;;;     keeps a reference to the data, so it cannot be stack-allocated.
;;; 
;;; Returns :
;;; 	The new type identifier.
;;; ----------------------------------------------------------------------------

(defcfun g-enum-register-static g-type-designator
  (name :string)
  (static-values (:pointer g-enum-value)))

;;; ----------------------------------------------------------------------------
;;; g_flags_register_static ()
;;; 
;;; GType g_flags_register_static (const gchar *name,
;;;                                const GFlagsValue *const_static_values);
;;; 
;;; Registers a new static flags type with the name name.
;;; 
;;; It is normally more convenient to let glib-mkenums generate a
;;; my_flags_get_type() function from a usual C enumeration definition than to
;;; write one yourself using g_flags_register_static().
;;; 
;;; name :
;;; 	A nul-terminated string used as the name of the new type.
;;; 
;;; const_static_values :
;;; 	An array of GFlagsValue structs for the possible flags values. The
;;;     array is terminated by a struct with all members being 0. GObject keeps
;;;     a reference to the data, so it cannot be stack-allocated.
;;; 
;;; Returns :
;;; 	The new type identifier.
;;; ----------------------------------------------------------------------------

(defcfun g-flags-register-static g-type-designator
  (name :string)
  (static-values (:pointer g-flags-value)))

;;; ----------------------------------------------------------------------------
;;; g_enum_complete_type_info ()
;;; 
;;; void g_enum_complete_type_info (GType g_enum_type,
;;;                                 GTypeInfo *info,
;;;                                 const GEnumValue *const_values);
;;; 
;;; This function is meant to be called from the complete_type_info() function
;;; of a GTypePlugin implementation, as in the following example:
;;; 
;;;  1 static void
;;;  2 my_enum_complete_type_info (GTypePlugin     *plugin,
;;;  3                             GType            g_type,
;;;  4                             GTypeInfo       *info,
;;;  5                             GTypeValueTable *value_table)
;;;  6 {
;;;  7   static const GEnumValue values[] = {
;;;  8     { MY_ENUM_FOO, "MY_ENUM_FOO", "foo" },
;;;  9     { MY_ENUM_BAR, "MY_ENUM_BAR", "bar" },
;;; 10     { 0, NULL, NULL }
;;; 11   };
;;; 12 
;;; 13   g_enum_complete_type_info (type, info, values);
;;; 14 }
;;; 
;;; g_enum_type :
;;; 	the type identifier of the type being completed
;;; 
;;; info :
;;; 	the GTypeInfo struct to be filled in
;;; 
;;; const_values :
;;; 	An array of GEnumValue structs for the possible enumeration values.
;;;     The array is terminated by a struct with all members being 0.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_complete_type_info ()
;;; 
;;; void g_flags_complete_type_info (GType g_flags_type,
;;;                                  GTypeInfo *info,
;;;                                  const GFlagsValue *const_values);
;;; 
;;; This function is meant to be called from the complete_type_info() function
;;; of a GTypePlugin implementation, see the example for
;;; g_enum_complete_type_info() above.
;;; 
;;; g_flags_type :
;;; 	the type identifier of the type being completed
;;; 
;;; info :
;;; 	the GTypeInfo struct to be filled in
;;; 
;;; const_values :
;;; 	An array of GFlagsValue structs for the possible enumeration values.
;;;     The array is terminated by a struct with all members being 0.
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.enumeration.lisp -----------------------------------
