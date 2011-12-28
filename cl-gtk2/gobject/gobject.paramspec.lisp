;;; ----------------------------------------------------------------------------
;;; gobject.paramspec.lisp
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
;;; GParamSpec
;;; 
;;; Metadata for parameter specifications
;;; 	
;;; Synopsis
;;; 
;;; #include <glib-object.h>
;;; 
;;; #define             G_TYPE_IS_PARAM                     (type)
;;; #define             G_PARAM_SPEC                        (pspec)
;;; #define             G_IS_PARAM_SPEC                     (pspec)
;;; #define             G_PARAM_SPEC_CLASS                  (pclass)
;;; #define             G_IS_PARAM_SPEC_CLASS               (pclass)
;;; #define             G_PARAM_SPEC_GET_CLASS              (pspec)
;;; #define             G_PARAM_SPEC_TYPE                   (pspec)
;;; #define             G_PARAM_SPEC_TYPE_NAME              (pspec)
;;; #define             G_PARAM_SPEC_VALUE_TYPE             (pspec)
;;;
;;; struct              GParamSpec;
;;; struct              GParamSpecClass;
;;;
;;; enum                GParamFlags;
;;; #define             G_PARAM_READWRITE
;;; #define             G_PARAM_STATIC_STRINGS
;;; #define             G_PARAM_MASK
;;; #define             G_PARAM_USER_SHIFT
;;; GParamSpec *        g_param_spec_ref                    (GParamSpec *pspec);
;;; void                g_param_spec_unref                  (GParamSpec *pspec);
;;; void                g_param_spec_sink                   (GParamSpec *pspec);
;;; GParamSpec *        g_param_spec_ref_sink               (GParamSpec *pspec);
;;; void                g_param_value_set_default           (GParamSpec *pspec,
;;;                                                          GValue *value);
;;; gboolean            g_param_value_defaults              (GParamSpec *pspec,
;;;                                                          GValue *value);
;;; gboolean            g_param_value_validate              (GParamSpec *pspec,
;;;                                                          GValue *value);
;;; gboolean            g_param_value_convert               (GParamSpec *pspec,
;;;                                                          const GValue *src_value,
;;;                                                          GValue *dest_value,
;;;                                                          gboolean strict_validation);
;;; gint                g_param_values_cmp                  (GParamSpec *pspec,
;;;                                                          const GValue *value1,
;;;                                                          const GValue *value2);
;;; const gchar *       g_param_spec_get_name               (GParamSpec *pspec);
;;; const gchar *       g_param_spec_get_nick               (GParamSpec *pspec);
;;; const gchar *       g_param_spec_get_blurb              (GParamSpec *pspec);
;;; gpointer            g_param_spec_get_qdata              (GParamSpec *pspec,
;;;                                                          GQuark quark);
;;; void                g_param_spec_set_qdata              (GParamSpec *pspec,
;;;                                                          GQuark quark,
;;;                                                          gpointer data);
;;; void                g_param_spec_set_qdata_full         (GParamSpec *pspec,
;;;                                                          GQuark quark,
;;;                                                          gpointer data,
;;;                                                          GDestroyNotify destroy);
;;; gpointer            g_param_spec_steal_qdata            (GParamSpec *pspec,
;;;                                                          GQuark quark);
;;; GParamSpec *        g_param_spec_get_redirect_target    (GParamSpec *pspec);
;;; gpointer            g_param_spec_internal               (GType param_type,
;;;                                                          const gchar *name,
;;;                                                          const gchar *nick,
;;;                                                          const gchar *blurb,
;;;                                                          GParamFlags flags);
;;; struct              GParamSpecTypeInfo;
;;; GType               g_param_type_register_static        (const gchar *name,
;;;                                                          const GParamSpecTypeInfo *pspec_info);
;;;                     GParamSpecPool;
;;; GParamSpecPool *    g_param_spec_pool_new               (gboolean type_prefixing);
;;; void                g_param_spec_pool_insert            (GParamSpecPool *pool,
;;;                                                          GParamSpec *pspec,
;;;                                                          GType owner_type);
;;; void                g_param_spec_pool_remove            (GParamSpecPool *pool,
;;;                                                          GParamSpec *pspec);
;;; GParamSpec *        g_param_spec_pool_lookup            (GParamSpecPool *pool,
;;;                                                          const gchar *param_name,
;;;                                                          GType owner_type,
;;;                                                          gboolean walk_ancestors);
;;; GParamSpec **       g_param_spec_pool_list              (GParamSpecPool *pool,
;;;                                                          GType owner_type,
;;;                                                          guint *n_pspecs_p);
;;; GList *             g_param_spec_pool_list_owned        (GParamSpecPool *pool,
;;;                                                          GType owner_type);
;;; 
;;; Description
;;; 
;;; GParamSpec is an object structure that encapsulates the metadata required
;;; to specify parameters, such as e.g. GObject properties.
;;; 
;;; Parameter names need to start with a letter (a-z or A-Z). Subsequent
;;; characters can be letters, numbers or a '-'. All other characters are
;;; replaced by a '-' during construction. The result of this replacement is
;;; called the canonical name of the parameter.
;;; 
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_PARAM()
;;; 
;;; #define G_TYPE_IS_PARAM(type) (G_TYPE_FUNDAMENTAL (type) == G_TYPE_PARAM)
;;; 
;;; Checks whether type "is a" G_TYPE_PARAM.
;;; 
;;; type :
;;; 	a GType ID
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC()
;;; 
;;; #define G_PARAM_SPEC(pspec)	(G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;                              G_TYPE_PARAM,
;;;                              GParamSpec))
;;; 
;;; Casts a derived GParamSpec object (e.g. of type GParamSpecInt) into a
;;; GParamSpec object.
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC()
;;; 
;;; #define G_IS_PARAM_SPEC(pspec) (G_TYPE_CHECK_INSTANCE_TYPE ((pspec),
;;;                                 G_TYPE_PARAM))
;;; 
;;; Checks whether pspec "is a" valid GParamSpec structure of type G_TYPE_PARAM
;;; or derived.
;;; 
;;; pspec :
;;; 	a GParamSpec
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_CLASS()
;;; 
;;; #define G_PARAM_SPEC_CLASS(pclass) (G_TYPE_CHECK_CLASS_CAST ((pclass),
;;;                                     G_TYPE_PARAM, GParamSpecClass))
;;; 
;;; Casts a derived GParamSpecClass structure into a GParamSpecClass structure.
;;; 
;;; pclass :
;;; 	a valid GParamSpecClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_CLASS()
;;; 
;;; #define G_IS_PARAM_SPEC_CLASS(pclass) (G_TYPE_CHECK_CLASS_TYPE ((pclass),
;;;                                        G_TYPE_PARAM))
;;; 
;;; Checks whether pclass "is a" valid GParamSpecClass structure of type
;;; G_TYPE_PARAM or derived.
;;; 
;;; pclass :
;;; 	a GParamSpecClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_GET_CLASS()
;;; 
;;; #define G_PARAM_SPEC_GET_CLASS(pspec) (G_TYPE_INSTANCE_GET_CLASS ((pspec),
;;;                                        G_TYPE_PARAM, GParamSpecClass))
;;; 
;;; Retrieves the GParamSpecClass of a GParamSpec.
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_TYPE()
;;; 
;;; #define G_PARAM_SPEC_TYPE(pspec) (G_TYPE_FROM_INSTANCE (pspec))
;;; 
;;; Retrieves the GType of this pspec.
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_TYPE_NAME()
;;; 
;;; #define G_PARAM_SPEC_TYPE_NAME(pspec)
;;;         (g_type_name (G_PARAM_SPEC_TYPE (pspec)))
;;; 
;;; Retrieves the GType name of this pspec.
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; G_PARAM_SPEC_VALUE_TYPE()
;;; 
;;; #define G_PARAM_SPEC_VALUE_TYPE(pspec) (G_PARAM_SPEC (pspec)->value_type)
;;; 
;;; Retrieves the GType to initialize a GValue for this parameter.
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; ----------------------------------------------------------------------------
;;; struct GParamSpec
;;; 
;;; struct GParamSpec {
;;;   GTypeInstance  g_type_instance;
;;; 
;;;   const gchar  *name;          /* interned string */
;;;   GParamFlags  flags;
;;;   GType        value_type;
;;;   GType        owner_type; /* class or interface using this property */
;;; };
;;; 
;;; All other fields of the GParamSpec struct are private and should not be
;;; used directly.
;;; 
;;; GTypeInstance g_type_instance;
;;; 	private GTypeInstance portion
;;; 
;;; const gchar *name;
;;; 	name of this parameter: always an interned string
;;; 
;;; GParamFlags flags;
;;; 	GParamFlags flags for this parameter
;;; 
;;; GType value_type;
;;; 	the GValue type for this parameter
;;; 
;;; GType owner_type;
;;; 	GType type that uses (introduces) this parameter
;;; ----------------------------------------------------------------------------

(defbitfield g-param-flags
  :readable
  :writable
  :construct
  :construct-only
  :lax-validation
  :static-name
  :nick
  :blurb)

(defcstruct g-param-spec
  (:type-instance g-type-instance)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:flags g-param-flags)
  (:value-type g-type-designator)
  (:owner-type g-type-designator))

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecClass
;;; 
;;; struct GParamSpecClass {
;;;   GTypeClass      g_type_class;
;;; 
;;;   GType		  value_type;
;;; 
;;;   void	        (*finalize)		(GParamSpec   *pspec);
;;; 
;;;   /* GParam methods */
;;;   void          (*value_set_default)    (GParamSpec   *pspec,
;;; 					 GValue       *value);
;;;   gboolean      (*value_validate)       (GParamSpec   *pspec,
;;; 					 GValue       *value);
;;;   gint          (*values_cmp)           (GParamSpec   *pspec,
;;; 					 const GValue *value1,
;;; 					 const GValue *value2);
;;; };
;;; 
;;; The class structure for the GParamSpec type. Normally, GParamSpec classes
;;; are filled by g_param_type_register_static().
;;; 
;;; GTypeClass g_type_class;
;;; 	the parent class
;;; 
;;; GType value_type;
;;; 	the GValue type for this parameter
;;; 
;;; finalize ()
;;; 	The instance finalization function (optional), should chain up to the
;;;     finalize method of the parent class.
;;; 
;;; value_set_default ()
;;; 	Resets a value to the default value for this type (recommended, the
;;;     default is g_value_reset()), see g_param_value_set_default().
;;; 
;;; value_validate ()
;;; 	Ensures that the contents of value comply with the specifications set
;;;     out by this type (optional), see g_param_value_validate().
;;; 
;;; values_cmp ()
;;; 	Compares value1 with value2 according to this type (recommended, the
;;;     default is memcmp()), see g_param_values_cmp().
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-class
  (:type-class g-type-class)
  (:value-type g-type-designator)
  (:finalize :pointer)
  (:value-set-default :pointer)
  (:value-validate :pointer)
  (:values-cmp :pointer))

;;; ----------------------------------------------------------------------------
;;; enum GParamFlags
;;; 
;;; typedef enum {
;;;   G_PARAM_READABLE            = 1 << 0,
;;;   G_PARAM_WRITABLE            = 1 << 1,
;;;   G_PARAM_CONSTRUCT	          = 1 << 2,
;;;   G_PARAM_CONSTRUCT_ONLY      = 1 << 3,
;;;   G_PARAM_LAX_VALIDATION      = 1 << 4,
;;;   G_PARAM_STATIC_NAME	      = 1 << 5,
;;; #ifndef G_DISABLE_DEPRECATED
;;;   G_PARAM_PRIVATE	      = G_PARAM_STATIC_NAME,
;;; #endif
;;;   G_PARAM_STATIC_NICK	      = 1 << 6,
;;;   G_PARAM_STATIC_BLURB	      = 1 << 7,
;;;   /* User defined flags go up to 30 */
;;;   G_PARAM_DEPRECATED          = 1 << 31
;;; } GParamFlags;
;;; 
;;; Through the GParamFlags flag values, certain aspects of parameters can be
;;; configured.
;;; 
;;; G_PARAM_READABLE
;;; 	the parameter is readable
;;; 
;;; G_PARAM_WRITABLE
;;; 	the parameter is writable
;;; 
;;; G_PARAM_CONSTRUCT
;;; 	the parameter will be set upon object construction
;;; 
;;; G_PARAM_CONSTRUCT_ONLY
;;; 	the parameter will only be set upon object construction
;;; 
;;; G_PARAM_LAX_VALIDATION
;;; 	upon parameter conversion (see g_param_value_convert()) strict
;;;     validation is not required
;;; 
;;; G_PARAM_STATIC_NAME
;;; 	the string used as name when constructing the parameter is guaranteed
;;;     to remain valid and unmodified for the lifetime of the parameter.
;;;     Since 2.8
;;; 
;;; G_PARAM_PRIVATE
;;; 	internal
;;; 
;;; G_PARAM_STATIC_NICK
;;; 	the string used as nick when constructing the parameter is guaranteed
;;;     to remain valid and unmmodified for the lifetime of the parameter.
;;;     Since 2.8
;;; 
;;; G_PARAM_STATIC_BLURB
;;; 	the string used as blurb when constructing the parameter is guaranteed
;;;     to remain valid and unmodified for the lifetime of the parameter.
;;;     Since 2.8
;;; 
;;; G_PARAM_DEPRECATED
;;; 	the parameter is deprecated and will be removed in a future version.
;;;     A warning will be generated if it is used while running with
;;;     G_ENABLE_DIAGNOSTIC=1. Since: 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_READWRITE
;;; 
;;; #define G_PARAM_READWRITE (G_PARAM_READABLE | G_PARAM_WRITABLE)
;;; 
;;; GParamFlags value alias for G_PARAM_READABLE | G_PARAM_WRITABLE.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_STATIC_STRINGS
;;; 
;;; #define G_PARAM_STATIC_STRINGS (G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK |
;;;                                 G_PARAM_STATIC_BLURB)
;;; 
;;; GParamFlags value alias for G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK |
;;;                             G_PARAM_STATIC_BLURB.
;;; 
;;; Since 2.13.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_MASK
;;; 
;;; #define G_PARAM_MASK (0x000000ff)
;;; 
;;; Mask containing the bits of GParamSpec.flags which are reserved for GLib.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_USER_SHIFT
;;; 
;;; #define G_PARAM_USER_SHIFT (8)
;;; 
;;; Minimum shift count to be used for user defined flags, to be stored in
;;; GParamSpec.flags. The maximum allowed is 30 + G_PARAM_USER_SHIFT.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_ref ()
;;; 
;;; GParamSpec * g_param_spec_ref (GParamSpec *pspec);
;;; 
;;; Increments the reference count of pspec.
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; 
;;; Returns :
;;; 	the GParamSpec that was passed into this function
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_unref ()
;;; 
;;; void g_param_spec_unref (GParamSpec *pspec);
;;; 
;;; Decrements the reference count of a pspec.
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; ----------------------------------------------------------------------------

(defcfun g-param-spec-unref :void
  (param-spec (:pointer g-param-spec)))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_sink ()
;;; 
;;; void g_param_spec_sink (GParamSpec *pspec);
;;; 
;;; The initial reference count of a newly created GParamSpec is 1, even
;;; though no one has explicitly called g_param_spec_ref() on it yet. So the
;;; initial reference count is flagged as "floating", until someone calls
;;; g_param_spec_ref (pspec); g_param_spec_sink (pspec); in sequence on it,
;;; taking over the initial reference count (thus ending up with a pspec that
;;; has a reference count of 1 still, but is not flagged "floating" anymore).
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_ref_sink ()
;;; 
;;; GParamSpec * g_param_spec_ref_sink (GParamSpec *pspec);
;;; 
;;; Convenience function to ref and sink a GParamSpec.
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; 
;;; Returns :
;;; 	the GParamSpec that was passed into this function
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun g-param-spec-ref-sink (:pointer g-param-spec)
  (param-spec (:pointer g-param-spec)))

;;; ----------------------------------------------------------------------------
;;; g_param_value_set_default ()
;;; 
;;; void g_param_value_set_default (GParamSpec *pspec, GValue *value);
;;; 
;;; Sets value to its default value as specified in pspec.
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; 
;;; value :
;;; 	a GValue of correct type for pspec
;;; ----------------------------------------------------------------------------

(defcfun g-param-value-set-default :void
  (param-spec (:pointer g-param-spec))
  (value (:pointer g-value)))

;;; ----------------------------------------------------------------------------
;;; g_param_value_defaults ()
;;; 
;;; gboolean g_param_value_defaults (GParamSpec *pspec, GValue *value);
;;; 
;;; Checks whether value contains the default value as specified in pspec.
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; 
;;; value :
;;; 	a GValue of correct type for pspec
;;; 
;;; Returns :
;;; 	whether value contains the canonical default for this pspec
;;; ----------------------------------------------------------------------------

(defcfun g-param-value-defaults :boolean
  (param-spec (:pointer g-param-spec))
  (value (:pointer g-value)))

;;; ----------------------------------------------------------------------------
;;; g_param_value_validate ()
;;; 
;;; gboolean g_param_value_validate (GParamSpec *pspec, GValue *value);
;;; 
;;; Ensures that the contents of value comply with the specifications set out
;;; by pspec. For example, a GParamSpecInt might require that integers stored
;;; in value may not be smaller than -42 and not be greater than +42. If value
;;; contains an integer outside of this range, it is modified accordingly, so
;;; the resulting value will fit into the range -42 .. +42.
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; 
;;; value :
;;; 	a GValue of correct type for pspec
;;; 
;;; Returns :
;;; 	whether modifying value was necessary to ensure validity
;;; ----------------------------------------------------------------------------

(defcfun g-param-value-validate :boolean
  (param-spec (:pointer g-param-spec))
  (value (:pointer g-value)))

;;; ----------------------------------------------------------------------------
;;; g_param_value_convert ()
;;; 
;;; gboolean g_param_value_convert (GParamSpec *pspec,
;;;                                 const GValue *src_value,
;;;                                 GValue *dest_value,
;;;                                 gboolean strict_validation);
;;; 
;;; Transforms src_value into dest_value if possible, and then validates
;;; dest_value, in order for it to conform to pspec. If strict_validation is
;;; TRUE this function will only succeed if the transformed dest_value complied
;;; to pspec without modifications.
;;; 
;;; See also g_value_type_transformable(), g_value_transform() and
;;; g_param_value_validate().
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; 
;;; src_value :
;;; 	souce GValue
;;; 
;;; dest_value :
;;; 	destination GValue of correct type for pspec
;;; 
;;; strict_validation :
;;; 	TRUE requires dest_value to conform to pspec without modifications
;;; 
;;; Returns :
;;; 	TRUE if transformation and validation were successful, FALSE otherwise
;;;     and dest_value is left untouched.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_values_cmp ()
;;; 
;;; gint g_param_values_cmp (GParamSpec *pspec,
;;;                          const GValue *value1,
;;;                          const GValue *value2);
;;; 
;;; Compares value1 with value2 according to pspec, and return -1, 0 or +1, if
;;; value1 is found to be less than, equal to or greater than value2,
;;; respectively.
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; 
;;; value1 :
;;; 	a GValue of correct type for pspec
;;; 
;;; value2 :
;;; 	a GValue of correct type for pspec
;;; 
;;; Returns :
;;; 	-1, 0 or +1, for a less than, equal to or greater than result
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_get_name ()
;;; 
;;; const gchar * g_param_spec_get_name (GParamSpec *pspec);
;;; 
;;; Get the name of a GParamSpec.
;;; 
;;; The name is always an "interned" string (as per g_intern_string()). This
;;; allows for pointer-value comparisons.
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; 
;;; Returns :
;;; 	the name of pspec.
;;; ----------------------------------------------------------------------------

(defcfun g-param-spec-get-name :string
  (param-spec (:pointer g-param-spec)))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_get_nick ()
;;; 
;;; const gchar * g_param_spec_get_nick (GParamSpec *pspec);
;;; 
;;; Get the nickname of a GParamSpec.
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; 
;;; Returns :
;;; 	the nickname of pspec.
;;; ----------------------------------------------------------------------------

(defcfun g-param-spec-get-nick :string
  (param-spec (:pointer g-param-spec)))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_get_blurb ()
;;; 
;;; const gchar * g_param_spec_get_blurb (GParamSpec *pspec)
;;; 
;;; Get the short description of a GParamSpec.
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; 
;;; Returns :
;;; 	the short description of pspec.
;;; ----------------------------------------------------------------------------

(defcfun g-param-spec-get-blurb :string
  (param-spec (:pointer g-param-spec)))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_get_qdata ()
;;; 
;;; gpointer g_param_spec_get_qdata (GParamSpec *pspec, GQuark quark);
;;; 
;;; Gets back user data pointers stored via g_param_spec_set_qdata().
;;; 
;;; pspec :
;;; 	a valid GParamSpec
;;; 
;;; quark :
;;; 	a GQuark, naming the user data pointer
;;; 
;;; Returns :
;;; 	the user data pointer set, or NULL. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_set_qdata ()
;;; 
;;; void g_param_spec_set_qdata (GParamSpec *pspec,
;;;                              GQuark quark,
;;;                              gpointer data);
;;; 
;;; Sets an opaque, named pointer on a GParamSpec. The name is specified
;;; through a GQuark (retrieved e.g. via g_quark_from_static_string()), and the
;;; pointer can be gotten back from the pspec with g_param_spec_get_qdata().
;;; Setting a previously set user data pointer, overrides (frees) the old
;;; pointer set, using NULL as pointer essentially removes the data stored.
;;; 
;;; pspec :
;;; 	the GParamSpec to set store a user data pointer
;;; 
;;; quark :
;;; 	a GQuark, naming the user data pointer
;;; 
;;; data :
;;; 	an opaque user data pointer
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_set_qdata_full ()
;;; 
;;; void g_param_spec_set_qdata_full (GParamSpec *pspec,
;;;                                   GQuark quark,
;;;                                   gpointer data,
;;;                                   GDestroyNotify destroy);
;;; 
;;; This function works like g_param_spec_set_qdata(), but in addition, a
;;; void (*destroy) (gpointer) function may be specified which is called with
;;; data as argument when the pspec is finalized, or the data is being
;;; overwritten by a call to g_param_spec_set_qdata() with the same quark.
;;; 
;;; pspec :
;;; 	the GParamSpec to set store a user data pointer
;;; 
;;; quark :
;;; 	a GQuark, naming the user data pointer
;;; 
;;; data :
;;; 	an opaque user data pointer
;;; 
;;; destroy :
;;; 	function to invoke with data as argument, when data needs to be freed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_steal_qdata ()
;;; 
;;; gpointer g_param_spec_steal_qdata (GParamSpec *pspec, GQuark quark);
;;; 
;;; Gets back user data pointers stored via g_param_spec_set_qdata() and
;;; removes the data from pspec without invoking its destroy() function (if any
;;; was set). Usually, calling this function is only required to update user
;;; data pointers with a destroy notifier.
;;; 
;;; pspec :
;;; 	the GParamSpec to get a stored user data pointer from
;;; 
;;; quark :
;;; 	a GQuark, naming the user data pointer
;;; 
;;; Returns :
;;; 	the user data pointer set, or NULL. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_get_redirect_target ()
;;; 
;;; GParamSpec * g_param_spec_get_redirect_target (GParamSpec *pspec);
;;; 
;;; If the paramspec redirects operations to another paramspec, returns that
;;; paramspec. Redirect is used typically for providing a new implementation
;;; of a property in a derived type while preserving all the properties from
;;; the parent type. Redirection is established by creating a property of type
;;; GParamSpecOverride. See g_object_class_override_property() for an example
;;; of the use of this capability.
;;; 
;;; pspec :
;;; 	a GParamSpec
;;; 
;;; Returns :
;;; 	paramspec to which requests on this paramspec should be redirected, or
;;;     NULL if none.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_internal ()
;;; 
;;; gpointer g_param_spec_internal (GType param_type,
;;;                                 const gchar *name,
;;;                                 const gchar *nick,
;;;                                 const gchar *blurb,
;;;                                 GParamFlags flags)
;;; 
;;; Creates a new GParamSpec instance.
;;; 
;;; A property name consists of segments consisting of ASCII letters and digits,
;;; separated by either the '-' or '_' character. The first character of a
;;; property name must be a letter. Names which violate these rules lead to
;;; undefined behaviour.
;;; 
;;; When creating and looking up a GParamSpec, either separator can be used,
;;; but they cannot be mixed. Using '-' is considerably more efficient and in
;;; fact required when using property names as detail strings for signals.
;;; 
;;; Beyond the name, GParamSpecs have two more descriptive strings associated
;;; with them, the nick, which should be suitable for use as a label for the
;;; property in a property editor, and the blurb, which should be a somewhat
;;; longer description, suitable for e.g. a tooltip. The nick and blurb should
;;; ideally be localized.
;;; 
;;; param_type :
;;; 	the GType for the property; must be derived from G_TYPE_PARAM
;;; 
;;; name :
;;; 	the canonical name of the property
;;; 
;;; nick :
;;; 	the nickname of the property
;;; 
;;; blurb :
;;; 	a short description of the property
;;; 
;;; flags :
;;; 	a combination of GParamFlags
;;; 
;;; Returns :
;;; 	a newly allocated GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecTypeInfo
;;; 
;;; struct GParamSpecTypeInfo {
;;;   /* type system portion */
;;;   guint16         instance_size;                               /* obligatory */
;;;   guint16         n_preallocs;                                 /* optional */
;;;   void		(*instance_init) (GParamSpec   *pspec); /* optional */
;;; 
;;;   /* class portion */
;;;   GType           value_type;				       /* obligatory */
;;;   void          (*finalize)             (GParamSpec   *pspec); /* optional */
;;;   void          (*value_set_default)    (GParamSpec   *pspec,  /* recommended */
;;; 					 GValue       *value);
;;;   gboolean      (*value_validate)       (GParamSpec   *pspec,  /* optional */
;;; 					 GValue       *value);
;;;   gint          (*values_cmp)           (GParamSpec   *pspec,  /* recommended */
;;; 					 const GValue *value1,
;;; 					 const GValue *value2);
;;; };
;;; 
;;; This structure is used to provide the type system with the information
;;; required to initialize and destruct (finalize) a parameter's class and
;;; instances thereof. The initialized structure is passed to the
;;; g_param_type_register_static() The type system will perform a deep copy of
;;; this structure, so its memory does not need to be persistent across
;;; invocation of g_param_type_register_static().
;;; 
;;; guint16 instance_size;
;;; 	Size of the instance (object) structure.
;;; 
;;; guint16 n_preallocs;
;;; 	Prior to GLib 2.10, it specified the number of pre-allocated (cached)
;;;     instances to reserve memory for (0 indicates no caching).
;;;     Since GLib 2.10, it is ignored, since instances are allocated with the
;;;     slice allocator now.
;;; 
;;; instance_init ()
;;; 	Location of the instance initialization function (optional).
;;; 
;;; GType value_type;
;;; 	The GType of values conforming to this GParamSpec
;;; 
;;; finalize ()
;;; 	The instance finalization function (optional).
;;; 
;;; value_set_default ()
;;; 	Resets a value to the default value for pspec (recommended, the default
;;;     is g_value_reset()), see g_param_value_set_default().
;;; 
;;; value_validate ()
;;; 	Ensures that the contents of value comply with the specifications set
;;;     out by pspec (optional), see g_param_value_validate().
;;; 
;;; values_cmp ()
;;; 	Compares value1 with value2 according to pspec (recommended, the
;;;     default is memcmp()), see g_param_values_cmp().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_type_register_static ()
;;; 
;;; GType g_param_type_register_static (const gchar *name,
;;;                                     const GParamSpecTypeInfo *pspec_info);
;;; 
;;; Registers name as the name of a new static type derived from G_TYPE_PARAM.
;;; The type system uses the information contained in the GParamSpecTypeInfo
;;; structure pointed to by info to manage the GParamSpec type and its
;;; instances.
;;; 
;;; name :
;;; 	0-terminated string used as the name of the new GParamSpec type.
;;; 
;;; pspec_info :
;;; 	The GParamSpecTypeInfo for this GParamSpec type.
;;; 
;;; Returns :
;;; 	The new type identifier.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GParamSpecPool
;;; 
;;; typedef struct _GParamSpecPool GParamSpecPool;
;;; 
;;; A GParamSpecPool maintains a collection of GParamSpecs which can be quickly
;;; accessed by owner and name. The implementation of the GObject property
;;; system uses such a pool to store the GParamSpecs of the properties all
;;; object types.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pool_new ()
;;; 
;;; GParamSpecPool * g_param_spec_pool_new (gboolean type_prefixing);
;;; 
;;; Creates a new GParamSpecPool.
;;; 
;;; If type_prefixing is TRUE, lookups in the newly created pool will allow to
;;; specify the owner as a colon-separated prefix of the property name, like
;;; "GtkContainer:border-width". This feature is deprecated, so you should
;;; always set type_prefixing to FALSE.
;;; 
;;; type_prefixing :
;;; 	Whether the pool will support type-prefixed property names.
;;; 
;;; Returns :
;;; 	a newly allocated GParamSpecPool. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pool_insert ()
;;; 
;;; void g_param_spec_pool_insert (GParamSpecPool *pool,
;;;                                GParamSpec *pspec,
;;;                                GType owner_type);
;;; 
;;; Inserts a GParamSpec in the pool.
;;; 
;;; pool :
;;; 	a GParamSpecPool.
;;; 
;;; pspec :
;;; 	the GParamSpec to insert
;;; 
;;; owner_type :
;;; 	a GType identifying the owner of pspec
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pool_remove ()
;;; 
;;; void g_param_spec_pool_remove (GParamSpecPool *pool, GParamSpec *pspec);
;;; 
;;; Removes a GParamSpec from the pool.
;;; 
;;; pool :
;;; 	a GParamSpecPool
;;; 
;;; pspec :
;;; 	the GParamSpec to remove
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pool_lookup ()
;;; 
;;; GParamSpec * g_param_spec_pool_lookup (GParamSpecPool *pool,
;;;                                        const gchar *param_name,
;;;                                        GType owner_type,
;;;                                        gboolean walk_ancestors);
;;; 
;;; Looks up a GParamSpec in the pool.
;;; 
;;; pool :
;;; 	a GParamSpecPool
;;; 
;;; param_name :
;;; 	the name to look for
;;; 
;;; owner_type :
;;; 	the owner to look for
;;; 
;;; walk_ancestors :
;;; 	If TRUE, also try to find a GParamSpec with param_name owned by an
;;;     ancestor of owner_type.
;;; 
;;; Returns :
;;; 	The found GParamSpec, or NULL if no matching GParamSpec was found.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pool_list ()
;;; 
;;; GParamSpec ** g_param_spec_pool_list (GParamSpecPool *pool,
;;;                                       GType owner_type,
;;;                                       guint *n_pspecs_p);
;;; 
;;; Gets an array of all GParamSpecs owned by owner_type in the pool.
;;; 
;;; pool :
;;; 	a GParamSpecPool
;;; 
;;; owner_type :
;;; 	the owner to look for
;;; 
;;; n_pspecs_p :
;;; 	return location for the length of the returned array.
;;; 
;;; Returns :
;;; 	a newly allocated array containing pointers to all GParamSpecs owned
;;;     by owner_type in the pool.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pool_list_owned ()
;;; 
;;; GList * g_param_spec_pool_list_owned (GParamSpecPool *pool,
;;;                                       GType owner_type);
;;; 
;;; Gets an GList of all GParamSpecs owned by owner_type in the pool.
;;; 
;;; pool :
;;; 	a GParamSpecPool
;;; 
;;; owner_type :
;;; 	the owner to look for
;;; 
;;; Returns :
;;; 	a GList of all GParamSpecs owned by owner_type in the poolGParamSpecs.
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.paramspec.lisp -------------------------------------
