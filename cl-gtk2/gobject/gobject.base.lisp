;;; ----------------------------------------------------------------------------
;;; gobject.base.lisp
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
;;; The Base Object Type
;;; 
;;; The Base Object Type — The base object type
;;; 
;;; Synopsis
;;; 
;;; #include <glib-object.h>
;;; 
;;; struct              GObject;
;;; struct              GObjectClass;
;;; struct              GObjectConstructParam;
;;; void                (*GObjectGetPropertyFunc)           (GObject *object,
;;;                                                          guint property_id,
;;;                                                          GValue *value,
;;;                                                          GParamSpec *pspec);
;;; void                (*GObjectSetPropertyFunc)           (GObject *object,
;;;                                                          guint property_id,
;;;                                                          const GValue *value,
;;;                                                          GParamSpec *pspec);
;;; void                (*GObjectFinalizeFunc)              (GObject *object);
;;; #define             G_TYPE_IS_OBJECT                    (type)
;;; #define             G_OBJECT                            (object)
;;; #define             G_IS_OBJECT                         (object)
;;; #define             G_OBJECT_CLASS                      (class)
;;; #define             G_IS_OBJECT_CLASS                   (class)
;;; #define             G_OBJECT_GET_CLASS                  (object)
;;; #define             G_OBJECT_TYPE                       (object)
;;; #define             G_OBJECT_TYPE_NAME                  (object)
;;; #define             G_OBJECT_CLASS_TYPE                 (class)
;;; #define             G_OBJECT_CLASS_NAME                 (class)
;;; void                g_object_class_install_property     (GObjectClass *oclass,
;;;                                                          guint property_id,
;;;                                                          GParamSpec *pspec);
;;; void                g_object_class_install_properties   (GObjectClass *oclass,
;;;                                                          guint n_pspecs,
;;;                                                          GParamSpec **pspecs);
;;; GParamSpec *        g_object_class_find_property        (GObjectClass *oclass,
;;;                                                          const gchar *property_name);
;;; GParamSpec **       g_object_class_list_properties      (GObjectClass *oclass,
;;;                                                          guint *n_properties);
;;; void                g_object_class_override_property    (GObjectClass *oclass,
;;;                                                          guint property_id,
;;;                                                          const gchar *name);
;;; void                g_object_interface_install_property (gpointer g_iface,
;;;                                                          GParamSpec *pspec);
;;; GParamSpec *        g_object_interface_find_property    (gpointer g_iface,
;;;                                                          const gchar *property_name);
;;; GParamSpec **       g_object_interface_list_properties  (gpointer g_iface,
;;;                                                          guint *n_properties_p);
;;; gpointer            g_object_new                        (GType object_type,
;;;                                                          const gchar *first_property_name,
;;;                                                          ...);
;;; gpointer            g_object_newv                       (GType object_type,
;;;                                                          guint n_parameters,
;;;                                                          GParameter *parameters);
;;; struct              GParameter;
;;; gpointer            g_object_ref                        (gpointer object);
;;; void                g_object_unref                      (gpointer object);
;;; gpointer            g_object_ref_sink                   (gpointer object);
;;; void                g_clear_object                      (volatile GObject **object_ptr);
;;; typedef             GInitiallyUnowned;
;;; typedef             GInitiallyUnownedClass;
;;; #define             G_TYPE_INITIALLY_UNOWNED
;;; gboolean            g_object_is_floating                (gpointer object);
;;; void                g_object_force_floating             (GObject *object);
;;; void                (*GWeakNotify)                      (gpointer data,
;;;                                                          GObject *where_the_object_was);
;;; void                g_object_weak_ref                   (GObject *object,
;;;                                                          GWeakNotify notify,
;;;                                                          gpointer data);
;;; void                g_object_weak_unref                 (GObject *object,
;;;                                                          GWeakNotify notify,
;;;                                                          gpointer data);
;;; void                g_object_add_weak_pointer           (GObject *object,
;;;                                                          gpointer *weak_pointer_location);
;;; void                g_object_remove_weak_pointer        (GObject *object,
;;;                                                          gpointer *weak_pointer_location);
;;; void                (*GToggleNotify)                    (gpointer data,
;;;                                                          GObject *object,
;;;                                                          gboolean is_last_ref);
;;; void                g_object_add_toggle_ref             (GObject *object,
;;;                                                          GToggleNotify notify,
;;;                                                          gpointer data);
;;; void                g_object_remove_toggle_ref          (GObject *object,
;;;                                                          GToggleNotify notify,
;;;                                                          gpointer data);
;;; gpointer            g_object_connect                    (gpointer object,
;;;                                                          const gchar *signal_spec,
;;;                                                          ...);
;;; void                g_object_disconnect                 (gpointer object,
;;;                                                          const gchar *signal_spec,
;;;                                                          ...);
;;; void                g_object_set                        (gpointer object,
;;;                                                          const gchar *first_property_name,
;;;                                                          ...);
;;; void                g_object_get                        (gpointer object)
;;;                                                          const gchar *first_property_name,
;;;                                                          ...);
;;; void                g_object_notify                     (GObject *object,
;;;                                                          const gchar *property_name);
;;; void                g_object_notify_by_pspec            (GObject *object,
;;;                                                          GParamSpec *pspec);
;;; void                g_object_freeze_notify              (GObject *object);
;;; void                g_object_thaw_notify                (GObject *object);
;;; gpointer            g_object_get_data                   (GObject *object,
;;;                                                          const gchar *key);
;;; void                g_object_set_data                   (GObject *object,
;;;                                                          const gchar *key,
;;;                                                          gpointer data);
;;; void                g_object_set_data_full              (GObject *object,
;;;                                                          const gchar *key,
;;;                                                          gpointer data,
;;;                                                          GDestroyNotify destroy);
;;; gpointer            g_object_steal_data                 (GObject *object,
;;;                                                          const gchar *key);
;;; gpointer            g_object_get_qdata                  (GObject *object,
;;;                                                          GQuark quark);
;;; void                g_object_set_qdata                  (GObject *object,
;;;                                                          GQuark quark,
;;;                                                          gpointer data);
;;; void                g_object_set_qdata_full             (GObject *object,
;;;                                                          GQuark quark,
;;;                                                          gpointer data,
;;;                                                          GDestroyNotify destroy);
;;; gpointer            g_object_steal_qdata                (GObject *object,
;;;                                                          GQuark quark);
;;; void                g_object_set_property               (GObject *object,
;;;                                                          const gchar *property_name,
;;;                                                          const GValue *value);
;;; void                g_object_get_property               (GObject *object,
;;;                                                          const gchar *property_name,
;;;                                                          GValue *value);
;;; GObject *           g_object_new_valist                 (GType object_type,
;;;                                                          const gchar *first_property_name,
;;;                                                          va_list var_args);
;;; void                g_object_set_valist                 (GObject *object,
;;;                                                          const gchar *first_property_name,
;;;                                                          va_list var_args);
;;; void                g_object_get_valist                 (GObject *object,
;;;                                                          const gchar *first_property_name,
;;;                                                          va_list var_args);
;;; void                g_object_watch_closure              (GObject *object,
;;;                                                          GClosure *closure);
;;; void                g_object_run_dispose                (GObject *object);
;;; #define             G_OBJECT_WARN_INVALID_PROPERTY_ID   (object,
;;;                                                          property_id,
;;;                                                          pspec)
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GBinding
;;;    +----GTypeModule
;;; 
;;; Signals
;;; 
;;;   "notify"                                         : No Hooks
;;; 
;;; Description
;;; 
;;; GObject is the fundamental type providing the common attributes and methods
;;; for all object types in GTK+, Pango and other libraries based on GObject.
;;; The GObject class provides methods for object construction and destruction,
;;; property access methods, and signal support. Signals are described in detail
;;; in Signals(3).
;;; 
;;; GInitiallyUnowned is derived from GObject. The only difference between the
;;; two is that the initial reference of a GInitiallyUnowned is flagged as a
;;; floating reference. This means that it is not specifically claimed to be
;;; "owned" by any code portion. The main motivation for providing floating
;;; references is C convenience. In particular, it allows code to be written as:
;;; 
;;;  1 container = create_container ();
;;;  2 container_add_child (container, create_child());
;;; 
;;; If container_add_child() will g_object_ref_sink() the passed in child, no
;;; reference of the newly created child is leaked. Without floating references,
;;; container_add_child() can only g_object_ref() the new child, so to implement
;;; this code without reference leaks, it would have to be written as:
;;; 
;;;  1 Child *child;
;;;  2 container = create_container ();
;;;  3 child = create_child ();
;;;  4 container_add_child (container, child);
;;;  5 g_object_unref (child);
;;; 
;;; The floating reference can be converted into an ordinary reference by
;;; calling g_object_ref_sink(). For already sunken objects (objects that don't
;;; have a floating reference anymore), g_object_ref_sink() is equivalent to
;;; g_object_ref() and returns a new reference. Since floating references are
;;; useful almost exclusively for C convenience, language bindings that provide
;;; automated reference and memory ownership maintenance (such as smart pointers
;;; or garbage collection) should not expose floating references in their API.
;;; 
;;; Some object implementations may need to save an objects floating state
;;; across certain code portions (an example is GtkMenu), to achieve this, the
;;; following sequence can be used:
;;; 
;;;  1 /* save floating state */
;;;  2 gboolean was_floating = g_object_is_floating (object);
;;;  3 g_object_ref_sink (object);
;;;  4 /* protected code portion */
;;;  5 ...;
;;;  6 /* restore floating state */
;;;  7 if (was_floating)
;;;  8   g_object_force_floating (object);
;;;  9 g_object_unref (object); /* release previously acquired reference */
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------
;;; struct GObject
;;;
;;; struct GObject;
;;;
;;; All the fields in the GObject structure are private to the GObject
;;; implementation and should never be accessed directly.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GObjectClass
;;;
;;; struct GObjectClass {
;;;   GTypeClass   g_type_class;
;;; 
;;;   /* seldom overidden */
;;;   GObject*   (*constructor)     (GType                  type,
;;;                                  guint                  n_construct_properties,
;;;                                  GObjectConstructParam *construct_properties);
;;;   /* overridable methods */     
;;;   void       (*set_property)    (GObject        *object,
;;;                                  guint           property_id,
;;;                                  const GValue   *value,
;;;                                  GParamSpec     *pspec);
;;;   void       (*get_property)    (GObject        *object,
;;;                                  guint           property_id,
;;;                                  GValue         *value,
;;;                                  GParamSpec     *pspec);
;;;   void       (*dispose)         (GObject        *object);
;;;   void       (*finalize)        (GObject        *object);
;;;   /* seldom overidden */                         
;;;   void       (*dispatch_properties_changed) (GObject      *object,
;;;                                              guint         n_pspecs,
;;;                                              GParamSpec  **pspecs);
;;;   /* signals */
;;;   void       (*notify)          (GObject *object,
;;;                                  GParamSpec *pspec);
;;; 
;;;   /* called when done constructing */
;;;   void       (*constructed)     (GObject *object);
;;; };
;;; 
;;; The class structure for the GObject type.
;;; 
;;; Example 1. Implementing singletons using a constructor
;;; 
;;;  1 static MySingleton *the_singleton = NULL;
;;;  2 
;;;  3 static GObject*
;;;  4 my_singleton_constructor (GType                  type,
;;;  5                           guint                  n_construct_params,
;;;  6                           GObjectConstructParam *construct_params)
;;;  7 {
;;;  8   GObject *object;
;;;  9   
;;; 10   if (!the_singleton)
;;; 11     {
;;; 12       object = G_OBJECT_CLASS (parent_class)->constructor (type,
;;; 13                                                            n_construct_params,
;;; 14                                                            construct_params);
;;; 15       the_singleton = MY_SINGLETON (object);
;;; 16     }
;;; 17   else
;;; 18     object = g_object_ref (G_OBJECT (the_singleton));
;;; 19 
;;; 20   return object;
;;; 21 }
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTypeClass g_type_class;
;;; 	the parent class
;;; 
;;; constructor ()
;;;     the constructor function is called by g_object_new() to complete the
;;;     object initialization after all the construction properties are set. The
;;;     first thing a constructor implementation must do is chain up to the
;;;     constructor of the parent class. Overriding constructor should be rarely
;;;     needed, e.g. to handle construct properties, or to implement singletons.
;;; 
;;; set_property ()
;;;     the generic setter for all properties of this type. Should be overridden
;;;     for every type with properties. Implementations of set_property don't
;;;     need to emit property change notification explicitly, this is handled by
;;;     the type system.
;;; 
;;; get_property ()
;;;     the generic getter for all properties of this type. Should be overridden
;;;     for every type with properties.
;;; 
;;; dispose ()
;;;     the dispose function is supposed to drop all references to other
;;;     objects, but keep the instance otherwise intact, so that client method
;;;     invocations still work. It may be run multiple times (due to reference
;;;     loops). Before returning, dispose should chain up to the dispose method
;;;     of the parent class.
;;; 
;;; finalize ()
;;;     instance finalization function, should finish the finalization of the
;;;     instance begun in dispose and chain up to the finalize method of the
;;;     parent class.
;;; 
;;; dispatch_properties_changed ()
;;;     emits property change notification for a bunch of properties. Overriding
;;;     dispatch_properties_changed should be rarely needed.
;;; 
;;; notify ()
;;;     the class closure for the notify signal
;;; 
;;; constructed ()
;;;     the constructed function is called by g_object_new() as the final step
;;;     of the object creation process. At the point of the call, all
;;;     construction properties have been set on the object. The purpose of this
;;;     call is to allow for object initialisation steps that can only be
;;;     performed after construction properties have been set. constructed
;;;     implementors should chain up to the constructed call of their parent
;;;     class to allow it to complete its initialisation.
;;; ----------------------------------------------------------------------------

;;; #|
;;; struct GObjectConstructParam
;;; 
;;; struct GObjectConstructParam {
;;;   GParamSpec *pspec;
;;;   GValue     *value;
;;; };
;;; 
;;; The GObjectConstructParam struct is an auxiliary structure used to hand GParamSpec/GValue pairs to the constructor of a GObjectClass.
;;; 
;;; GParamSpec *pspec;
;;; 	the GParamSpec of the construct parameter
;;; 
;;; GValue *value;
;;; 	the value to set the parameter to
;;; GObjectGetPropertyFunc ()
;;; 
;;; void                (*GObjectGetPropertyFunc)           (GObject *object,
;;;                                                          guint property_id,
;;;                                                          GValue *value,
;;;                                                          GParamSpec *pspec);
;;; 
;;; The type of the get_property function of GObjectClass.
;;; 
;;; object :
;;; 	a GObject
;;; 
;;; property_id :
;;; 	the numeric id under which the property was registered with g_object_class_install_property().
;;; 
;;; value :
;;; 	a GValue to return the property value in
;;; 
;;; pspec :
;;; 	the GParamSpec describing the property
;;; GObjectSetPropertyFunc ()
;;; 
;;; void                (*GObjectSetPropertyFunc)           (GObject *object,
;;;                                                          guint property_id,
;;;                                                          const GValue *value,
;;;                                                          GParamSpec *pspec);
;;; 
;;; The type of the set_property function of GObjectClass.
;;; 
;;; object :
;;; 	a GObject
;;; 
;;; property_id :
;;; 	the numeric id under which the property was registered with g_object_class_install_property().
;;; 
;;; value :
;;; 	the new value for the property
;;; 
;;; pspec :
;;; 	the GParamSpec describing the property
;;; GObjectFinalizeFunc ()
;;; 
;;; void                (*GObjectFinalizeFunc)              (GObject *object);
;;; 
;;; The type of the finalize function of GObjectClass.
;;; 
;;; object :
;;; 	the GObject being finalized
;;; G_TYPE_IS_OBJECT()
;;; 
;;; #define G_TYPE_IS_OBJECT(type)      (G_TYPE_FUNDAMENTAL (type) == G_TYPE_OBJECT)
;;; 
;;; Check if the passed in type id is a G_TYPE_OBJECT or derived from it.
;;; 
;;; type :
;;; 	Type id to check
;;; 
;;; Returns :
;;; 	FALSE or TRUE, indicating whether type is a G_TYPE_OBJECT.
;;; G_OBJECT()
;;; 
;;; #define G_OBJECT(object)            (G_TYPE_CHECK_INSTANCE_CAST ((object), G_TYPE_OBJECT, GObject))
;;; 
;;; Casts a GObject or derived pointer into a (GObject*) pointer. Depending on the current debugging level, this function may invoke certain runtime checks to identify invalid casts.
;;; 
;;; object :
;;; 	Object which is subject to casting.
;;; G_IS_OBJECT()
;;; 
;;; #define G_IS_OBJECT(object)         (G_TYPE_CHECK_INSTANCE_TYPE ((object), G_TYPE_OBJECT))
;;; 
;;; Checks whether a valid GTypeInstance pointer is of type G_TYPE_OBJECT.
;;; 
;;; object :
;;; 	Instance to check for being a G_TYPE_OBJECT.
;;; G_OBJECT_CLASS()
;;; 
;;; #define G_OBJECT_CLASS(class)       (G_TYPE_CHECK_CLASS_CAST ((class), G_TYPE_OBJECT, GObjectClass))
;;; 
;;; Casts a derived GObjectClass structure into a GObjectClass structure.
;;; 
;;; class :
;;; 	a valid GObjectClass
;;; G_IS_OBJECT_CLASS()
;;; 
;;; #define G_IS_OBJECT_CLASS(class)    (G_TYPE_CHECK_CLASS_TYPE ((class), G_TYPE_OBJECT))
;;; 
;;; Checks whether class "is a" valid GObjectClass structure of type G_TYPE_OBJECT or derived.
;;; 
;;; class :
;;; 	a GObjectClass
;;; G_OBJECT_GET_CLASS()
;;; 
;;; #define G_OBJECT_GET_CLASS(object)  (G_TYPE_INSTANCE_GET_CLASS ((object), G_TYPE_OBJECT, GObjectClass))
;;; 
;;; Get the class structure associated to a GObject instance.
;;; 
;;; object :
;;; 	a GObject instance.
;;; 
;;; Returns :
;;; 	pointer to object class structure.
;;; G_OBJECT_TYPE()
;;; 
;;; #define G_OBJECT_TYPE(object)       (G_TYPE_FROM_INSTANCE (object))
;;; 
;;; Get the type id of an object.
;;; 
;;; object :
;;; 	Object to return the type id for.
;;; 
;;; Returns :
;;; 	Type id of object.
;;; G_OBJECT_TYPE_NAME()
;;; 
;;; #define G_OBJECT_TYPE_NAME(object)  (g_type_name (G_OBJECT_TYPE (object)))
;;; 
;;; Get the name of an object's type.
;;; 
;;; object :
;;; 	Object to return the type name for.
;;; 
;;; Returns :
;;; 	Type name of object. The string is owned by the type system and should not be freed.
;;; G_OBJECT_CLASS_TYPE()
;;; 
;;; #define G_OBJECT_CLASS_TYPE(class)  (G_TYPE_FROM_CLASS (class))
;;; 
;;; Get the type id of a class structure.
;;; 
;;; class :
;;; 	a valid GObjectClass
;;; 
;;; Returns :
;;; 	Type id of class.
;;; G_OBJECT_CLASS_NAME()
;;; 
;;; #define G_OBJECT_CLASS_NAME(class)  (g_type_name (G_OBJECT_CLASS_TYPE (class)))
;;; 
;;; Return the name of a class structure's type.
;;; 
;;; class :
;;; 	a valid GObjectClass
;;; 
;;; Returns :
;;; 	Type name of class. The string is owned by the type system and should not be freed.
;;; |#

;;; ----------------------------------------------------------------------------
;;; g_object_class_install_property ()
;;;
;;; void g_object_class_install_property (GObjectClass *oclass,
;;;                                       guint property_id,
;;;                                       GParamSpec *pspec)
;;;
;;; Installs a new property. This is usually done in the class initializer.
;;;
;;; Note that it is possible to redefine a property in a derived class, by
;;; installing a property with the same name. This can be useful at times, e.g.
;;; to change the range of allowed values or the default value.
;;;
;;; oclass      : a GObjectClass
;;; property_id : the id for the new property
;;; pspec       : the GParamSpec for the new property
;;; ----------------------------------------------------------------------------

(defcfun g-object-class-install-property :void
  (class (:pointer g-object-class))
  (property-id :uint)
  (param-spec (:pointer g-param-spec)))

;;; #|
;;; g_object_class_install_properties ()
;;; 
;;; void                g_object_class_install_properties   (GObjectClass *oclass,
;;;                                                          guint n_pspecs,
;;;                                                          GParamSpec **pspecs);
;;; 
;;; Installs new properties from an array of GParamSpecs. This is usually done in the class initializer.
;;; 
;;; The property id of each property is the index of each GParamSpec in the pspecs array.
;;; 
;;; The property id of 0 is treated specially by GObject and it should not be used to store a GParamSpec.
;;; 
;;; This function should be used if you plan to use a static array of GParamSpecs and g_object_notify_by_pspec(). For instance, this class initialization:
;;; 
;;; 1
;;; 2
;;; 3
;;; 4
;;; 5
;;; 6
;;; 7
;;; 8
;;; 9
;;; 10
;;; 11
;;; 12
;;; 13
;;; 14
;;; 15
;;; 16
;;; 17
;;; 18
;;; 19
;;; 20
;;; 21
;;; 22
;;; 23
;;; 24
;;; 25
;;; 26
;;; 27
;;; 28
;;; 
;;; 	
;;; 
;;; enum {
;;;   PROP_0, PROP_FOO, PROP_BAR, N_PROPERTIES
;;; };
;;; 
;;; static GParamSpec *obj_properties[N_PROPERTIES] = { NULL, };
;;; 
;;; static void
;;; my_object_class_init (MyObjectClass *klass)
;;; {
;;;   GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
;;; 
;;;   obj_properties[PROP_FOO] =
;;;     g_param_spec_int ("foo", "Foo", "Foo",
;;;                       -1, G_MAXINT,
;;;                       0,
;;;                       G_PARAM_READWRITE);
;;; 
;;;   obj_properties[PROP_BAR] =
;;;     g_param_spec_string ("bar", "Bar", "Bar",
;;;                          NULL,
;;;                          G_PARAM_READWRITE);
;;; 
;;;   gobject_class->set_property = my_object_set_property;
;;;   gobject_class->get_property = my_object_get_property;
;;;   g_object_class_install_properties (gobject_class,
;;;                                      N_PROPERTIES,
;;;                                      obj_properties);
;;; }
;;; 
;;; allows calling g_object_notify_by_pspec() to notify of property changes:
;;; 
;;; 1
;;; 2
;;; 3
;;; 4
;;; 5
;;; 6
;;; 7
;;; 8
;;; 9
;;; 
;;; 	
;;; 
;;; void
;;; my_object_set_foo (MyObject *self, gint foo)
;;; {
;;;   if (self->foo != foo)
;;;     {
;;;       self->foo = foo;
;;;       g_object_notify_by_pspec (G_OBJECT (self), obj_properties[PROP_FOO]);
;;;     }
;;;  }
;;; 
;;; oclass :
;;; 	a GObjectClass
;;; 
;;; n_pspecs :
;;; 	the length of the GParamSpecs array
;;; 
;;; pspecs :
;;; 	the GParamSpecs array defining the new properties. [array length=n_pspecs]
;;; 
;;; Since 2.26
;;; |#

;;; ----------------------------------------------------------------------------
;;; g_object_class_find_property ()
;;;
;;; GParamSpec *        g_object_class_find_property        (GObjectClass *oclass,
;;;                                                         const gchar *property_name);
;;;
;;; Looks up the GParamSpec for a property of a class.
;;;
;;; oclass        : a GObjectClass
;;; property_name : the name of the property to look up
;;; Returns       : the GParamSpec for the property, or NULL if the class
;;;                 doesn't have a property of that name. [transfer none]
;;; ----------------------------------------------------------------------------

(defcfun g-object-class-find-property (:pointer g-param-spec)
  (class (:pointer g-object-class))
  (property-name :string))

;;; ----------------------------------------------------------------------------
;;; g_object_class_list_properties ()
;;;
;;; GParamSpec ** g_object_class_list_properties (GObjectClass *oclass,
;;;                                               guint *n_properties)
;;;
;;; Get an array of GParamSpec* for all properties of a class.
;;;
;;; oclass       : a GObjectClass
;;; n_properties : return location for the length of the returned array.
;;; Returns      : an array of GParamSpec* which should be freed after use.
;;; ----------------------------------------------------------------------------

(defcfun g-object-class-list-properties (:pointer (:pointer g-param-spec))
  (class (:pointer g-object-class))
  (n-properties (:pointer :uint)))

;;; ----------------------------------------------------------------------------
;;; g_object_class_override_property ()
;;;
;;; void g_object_class_override_property (GObjectClass *oclass,
;;;                                        guint property_id,
;;;                                        const gchar *name)
;;;
;;; Registers property_id as referring to a property with the name name in a
;;; parent class or in an interface implemented by oclass. This allows this
;;; class to override a property implementation in a parent class or to provide
;;; the implementation of a property from an interface.
;;;
;;; Note
;;; Internally, overriding is implemented by creating a property of type
;;; GParamSpecOverride; generally operations that query the properties of the
;;; object class, such as g_object_class_find_property() or
;;; g_object_class_list_properties() will return the overridden property.
;;; However, in one case, the construct_properties argument of the constructor
;;; virtual function, the GParamSpecOverride is passed instead, so that the
;;; param_id field of the GParamSpec will be correct. For virtually all uses,
;;; this makes no difference. If you need to get the overridden property, you
;;; can call g_param_spec_get_redirect_target().
;;;
;;; oclass      : a GObjectClass
;;; property_id : the new property ID
;;; name        : the name of a property registered in a parent class or in an
;;;               interface of this class.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun g-object-class-override-property :void
  (class (:pointer g-object-class))
  (property-id :uint)
  (name :string))

;;; ----------------------------------------------------------------------------
;;; g_object_interface_install_property ()
;;;
;;; void g_object_interface_install_property (gpointer g_iface,
;;;                                           GParamSpec *pspec)
;;;
;;; Add a property to an interface; this is only useful for interfaces that are
;;; added to GObject-derived types. Adding a property to an interface forces all
;;; objects classes with that interface to have a compatible property. The
;;; compatible property could be a newly created GParamSpec, but normally
;;; g_object_class_override_property() will be used so that the object class
;;; only needs to provide an implementation and inherits the property
;;; description, default value, bounds, and so forth from the interface
;;; property.
;;;
;;; This function is meant to be called from the interface's default vtable
;;; initialization function (the class_init member of GTypeInfo.) It must not be
;;; called after after class_init has been called for any object types
;;; implementing this interface.
;;;
;;; g_iface : any interface vtable for the interface, or the default vtable for
;;;           the interface.
;;; pspec   : the GParamSpec for the new property
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun g-object-interface-install-property :void
  (interface :pointer)
  (param-spec (:pointer g-param-spec)))

;;; ----------------------------------------------------------------------------
;;; g_object_interface_find_property ()
;;;
;;; GParamSpec * g_object_interface_find_property (gpointer g_iface,
;;;                                                const gchar *property_name)
;;;
;;; Find the GParamSpec with the given name for an interface. Generally, the
;;; interface vtable passed in as g_iface will be the default vtable from
;;; g_type_default_interface_ref(), or, if you know the interface has already
;;; been loaded, g_type_default_interface_peek().
;;;
;;; g_iface       : any interface vtable for the interface, or the default
;;;                 vtable for the interface
;;; property_name : name of a property to lookup.
;;; Returns       : the GParamSpec for the property of the interface with the
;;;                 name property_name, or NULL if no such property exists.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun g-object-interface-find-property (:pointer g-param-spec)
  (interface :pointer)
  (property-name :string))

;;; ----------------------------------------------------------------------------
;;; g_object_interface_list_properties ()
;;;
;;; GParamSpec ** g_object_interface_list_properties (gpointer g_iface,
;;;                                                   guint *n_properties_p)
;;;
;;; Lists the properties of an interface.Generally, the interface vtable passed
;;; in as g_iface will be the default vtable from
;;; g_type_default_interface_ref(), or, if you know the interface has already
;;; been loaded, g_type_default_interface_peek().
;;;
;;; g_iface        : any interface vtable for the interface, or the default
;;;                  vtable for the interface
;;; n_properties_p : location to store number of properties returned.
;;; Returns        : a pointer to an array of pointers to GParamSpec structures.
;;;                  The paramspecs are owned by GLib, but the array should be
;;;                  freed with g_free() when you are done with it.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun g-object-interface-list-properties (:pointer g-param-spec)
  (interface :pointer)
  (n-properties (:pointer :uint)))

;;; ----------------------------------------------------------------------------
;;; g_object_new ()
;;;
;;; gpointer g_object_new (GType object_type,
;;;                        const gchar *first_property_name, ...)
;;;
;;; Creates a new instance of a GObject subtype and sets its properties.
;;;
;;; Construction parameters (see G_PARAM_CONSTRUCT, G_PARAM_CONSTRUCT_ONLY)
;;; which are not explicitly specified are set to their default values.
;;;
;;; object_type         : the type id of the GObject subtype to instantiate
;;; first_property_name : the name of the first property
;;; ...                 : the value of the first property, followed optionally
;;;                       by more name/value pairs, followed by NULL
;;; Returns             : a new instance of object_type. [transfer full]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_newv ()
;;;
;;; gpointer g_object_newv (GType object_type,
;;;                         guint n_parameters,
;;;                         GParameter *parameters)
;;;
;;; Creates a new instance of a GObject subtype and sets its properties.
;;;
;;; Construction parameters (see G_PARAM_CONSTRUCT, G_PARAM_CONSTRUCT_ONLY)
;;; which are not explicitly specified are set to their default values.
;;;
;; Rename to: g_object_new
;;;
;;; object_type  : the type id of the GObject subtype to instantiate
;;; n_parameters : the length of the parameters array
;;; parameters   : an array of GParameter.
;;; Returns      : a new instance of object_type.
;;; ----------------------------------------------------------------------------

(defcfun g-object-newv :pointer
  (object-type g-type-designator)
  (n-parameter :uint)
  (parameters (:pointer g-parameter)))

;;; #|
;;; struct GParameter
;;; 
;;; struct GParameter {
;;;   const gchar *name;
;;;   GValue       value;
;;; };
;;; 
;;; The GParameter struct is an auxiliary structure used to hand parameter name/value pairs to g_object_newv().
;;; 
;;; const gchar *name;
;;; 	the parameter name
;;; 
;;; GValue value;
;;; 	the parameter value
;;; |#

;;; ----------------------------------------------------------------------------
;;; g_object_ref ()
;;;
;;; gpointer g_object_ref (gpointer object)
;;;
;;; Increases the reference count of object.
;;;
;;; object  : a GObject.
;;; Returns : the same object.
;;; ----------------------------------------------------------------------------

(defcfun g-object-ref :pointer
  (object :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_unref ()
;;;
;;; void g_object_unref (gpointer object)
;;;
;;; Decreases the reference count of object. When its reference count drops
;;; to 0, the object is finalized (i.e. its memory is freed).
;;;
;;; object : a GObject.
;;; ----------------------------------------------------------------------------

(defcfun g-object-unref :void
  (object :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_ref_sink ()
;;;
;;; gpointer g_object_ref_sink (gpointer object)
;;;
;;; Increase the reference count of object, and possibly remove the floating
;;; reference, if object has a floating reference.
;;;
;;; In other words, if the object is floating, then this call "assumes
;;; ownership" of the floating reference, converting it to a normal reference
;;; by clearing the floating flag while leaving the reference count unchanged.
;;; If the object is not floating, then this call adds a new normal reference
;;; increasing the reference count by one.
;;;
;;; object  : a GObject.
;;; Returns : object.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun g-object-ref-sink :pointer
  (object :pointer))

;;; ----------------------------------------------------------------------------
;;; g_clear_object ()
;;;
;;; void g_clear_object (volatile GObject **object_ptr)
;;;
;;; Clears a reference to a GObject.
;;;
;;; object_ptr must not be NULL.
;;;
;;; If the reference is NULL then this function does nothing. Otherwise, the
;;; reference count of the object is decreased and the pointer is set to NULL.
;;;
;;; This function is threadsafe and modifies the pointer atomically, using
;;; memory barriers where needed.
;;;
;;; A macro is also included that allows this function to be used without
;;; pointer casts.
;;;
;;; object_ptr : a pointer to a GObject reference
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; #|
;;; GInitiallyUnowned
;;; 
;;; typedef struct _GObject                  GInitiallyUnowned;
;;; 
;;; All the fields in the GInitiallyUnowned structure are private to the GInitiallyUnowned implementation and should never be accessed directly.
;;; GInitiallyUnownedClass
;;; 
;;; typedef struct _GObjectClass             GInitiallyUnownedClass;
;;; 
;;; The class structure for the GInitiallyUnowned type.
;;; G_TYPE_INITIALLY_UNOWNED
;;; 
;;; #define G_TYPE_INITIALLY_UNOWNED	      (g_initially_unowned_get_type())
;;; 
;;; The type for GInitiallyUnowned.
;;; |#

;;; ----------------------------------------------------------------------------
;;; g_object_is_floating ()
;;;
;;; gboolean g_object_is_floating (gpointer object)
;;;
;;; Checks whether object has a floating reference.
;;;
;;; object  : a GObject.
;;; Returns : TRUE if object has a floating reference
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun g-object-is-floating :boolean
  (object :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_force_floating ()
;;;
;;; void g_object_force_floating (GObject *object)
;;;
;;; This function is intended for GObject implementations to re-enforce a
;;; floating object reference. Doing this is seldom required: all
;;; GInitiallyUnowneds are created with a floating reference which usually just
;;; needs to be sunken by calling g_object_ref_sink().
;;;
;;; object : a GObject
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun g-object-force-floating :void
  (object :pointer))

;;; #|
;;; GWeakNotify ()
;;; 
;;; void                (*GWeakNotify)                      (gpointer data,
;;;                                                          GObject *where_the_object_was);
;;; 
;;; A GWeakNotify function can be added to an object as a callback that gets triggered when the object is finalized. Since the object is already being finalized when the GWeakNotify is called, there's not much you could do with the object, apart from e.g. using its address as hash-index or the like.
;;; 
;;; data :
;;; 	data that was provided when the weak reference was established
;;; 
;;; where_the_object_was :
;;; 	the object being finalized
;;; |#

;;; ----------------------------------------------------------------------------
;;; g_object_weak_ref ()
;;;
;;; void g_object_weak_ref (GObject *object, GWeakNotify notify, gpointer data)
;;;
;;; Adds a weak reference callback to an object. Weak references are used for
;;; notification when an object is finalized. They are called "weak references"
;;; because they allow you to safely hold a pointer to an object without calling
;;; g_object_ref() (g_object_ref() adds a strong reference, that is, forces the
;;; object to stay alive).
;;;
;;; object : GObject to reference weakly
;;; notify : callback to invoke before the object is freed
;;; data   : extra data to pass to notify
;;; ----------------------------------------------------------------------------

(defcfun g-object-weak-ref :void
  (object :pointer)
  (notify :pointer)
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_weak_unref ()
;;;
;;; void g_object_weak_unref (GObject *object,
;;;                           GWeakNotify notify,
;;;                           gpointer data)
;;;
;;; Removes a weak reference callback to an object.
;;;
;;; object : GObject to remove a weak reference from
;;; notify : callback to search for
;;; data   : data to search for
;;; ----------------------------------------------------------------------------

(defcfun g-object-weak-unref :void
  (object :pointer)
  (notify :pointer)
  (data :pointer))

;;; #|
;;; g_object_add_weak_pointer ()
;;; 
;;; void                g_object_add_weak_pointer           (GObject *object,
;;;                                                          gpointer *weak_pointer_location);
;;; 
;;; Adds a weak reference from weak_pointer to object to indicate that the pointer located at weak_pointer_location is only valid during the lifetime of object. When the object is finalized, weak_pointer will be set to NULL.
;;; 
;;; object :
;;; 	The object that should be weak referenced.
;;; 
;;; weak_pointer_location :
;;; 	The memory address of a pointer. [inout]
;;; g_object_remove_weak_pointer ()
;;; 
;;; void                g_object_remove_weak_pointer        (GObject *object,
;;;                                                          gpointer *weak_pointer_location);
;;; 
;;; Removes a weak reference from object that was previously added using g_object_add_weak_pointer(). The weak_pointer_location has to match the one used with g_object_add_weak_pointer().
;;; 
;;; object :
;;; 	The object that is weak referenced.
;;; 
;;; weak_pointer_location :
;;; 	The memory address of a pointer. [inout]
;;; GToggleNotify ()
;;; 
;;; void                (*GToggleNotify)                    (gpointer data,
;;;                                                          GObject *object,
;;;                                                          gboolean is_last_ref);
;;; 
;;; A callback function used for notification when the state of a toggle reference changes. See g_object_add_toggle_ref().
;;; 
;;; data :
;;; 	Callback data passed to g_object_add_toggle_ref()
;;; 
;;; object :
;;; 	The object on which g_object_add_toggle_ref() was called.
;;; 
;;; is_last_ref :
;;; 	TRUE if the toggle reference is now the last reference to the object. FALSE if the toggle reference was the last reference and there are now other references.
;;; g_object_add_toggle_ref ()
;;; 
;;; void                g_object_add_toggle_ref             (GObject *object,
;;;                                                          GToggleNotify notify,
;;;                                                          gpointer data);
;;; 
;;; Increases the reference count of the object by one and sets a callback to be called when all other references to the object are dropped, or when this is already the last reference to the object and another reference is established.
;;; 
;;; This functionality is intended for binding object to a proxy object managed by another memory manager. This is done with two paired references: the strong reference added by g_object_add_toggle_ref() and a reverse reference to the proxy object which is either a strong reference or weak reference.
;;; 
;;; The setup is that when there are no other references to object, only a weak reference is held in the reverse direction from object to the proxy object, but when there are other references held to object, a strong reference is held. The notify callback is called when the reference from object to the proxy object should be toggled from strong to weak (is_last_ref true) or weak to strong (is_last_ref false).
;;; 
;;; Since a (normal) reference must be held to the object before calling g_object_add_toggle_ref(), the initial state of the reverse link is always strong.
;;; 
;;; Multiple toggle references may be added to the same gobject, however if there are multiple toggle references to an object, none of them will ever be notified until all but one are removed. For this reason, you should only ever use a toggle reference if there is important state in the proxy object.
;;; 
;;; object :
;;; 	a GObject
;;; 
;;; notify :
;;; 	a function to call when this reference is the last reference to the object, or is no longer the last reference.
;;; 
;;; data :
;;; 	data to pass to notify
;;; 
;;; Since 2.8
;;; g_object_remove_toggle_ref ()
;;; 
;;; void                g_object_remove_toggle_ref          (GObject *object,
;;;                                                          GToggleNotify notify,
;;;                                                          gpointer data);
;;; 
;;; Removes a reference added with g_object_add_toggle_ref(). The reference count of the object is decreased by one.
;;; 
;;; object :
;;; 	a GObject
;;; 
;;; notify :
;;; 	a function to call when this reference is the last reference to the object, or is no longer the last reference.
;;; 
;;; data :
;;; 	data to pass to notify
;;; 
;;; Since 2.8
;;; g_object_connect ()
;;; 
;;; gpointer            g_object_connect                    (gpointer object,
;;;                                                          const gchar *signal_spec,
;;;                                                          ...);
;;; 
;;; A convenience function to connect multiple signals at once.
;;; 
;;; The signal specs expected by this function have the form "modifier::signal_name", where modifier can be one of the following:
;;; 
;;; signal
;;; 	
;;; 
;;; equivalent to g_signal_connect_data (..., NULL, 0)
;;; 
;;; object_signal, object-signal
;;; 	
;;; 
;;; equivalent to g_signal_connect_object (..., 0)
;;; 
;;; swapped_signal, swapped-signal
;;; 	
;;; 
;;; equivalent to g_signal_connect_data (..., NULL, G_CONNECT_SWAPPED)
;;; 
;;; swapped_object_signal, swapped-object-signal
;;; 	
;;; 
;;; equivalent to g_signal_connect_object (..., G_CONNECT_SWAPPED)
;;; 
;;; signal_after, signal-after
;;; 	
;;; 
;;; equivalent to g_signal_connect_data (..., NULL, G_CONNECT_AFTER)
;;; 
;;; object_signal_after, object-signal-after
;;; 	
;;; 
;;; equivalent to g_signal_connect_object (..., G_CONNECT_AFTER)
;;; 
;;; swapped_signal_after, swapped-signal-after
;;; 	
;;; 
;;; equivalent to g_signal_connect_data (..., NULL, G_CONNECT_SWAPPED | G_CONNECT_AFTER)
;;; 
;;; swapped_object_signal_after, swapped-object-signal-after
;;; 	
;;; 
;;; equivalent to g_signal_connect_object (..., G_CONNECT_SWAPPED | G_CONNECT_AFTER)
;;; 
;;; 1
;;; 2
;;; 3
;;; 4
;;; 5
;;; 6
;;; 7
;;; 8
;;; 
;;; 	
;;; 
;;; menu->toplevel = g_object_connect (g_object_new (GTK_TYPE_WINDOW,
;;;                            "type", GTK_WINDOW_POPUP,
;;;                            "child", menu,
;;;                            NULL),
;;;                      "signal::event", gtk_menu_window_event, menu,
;;;                      "signal::size_request", gtk_menu_window_size_request, menu,
;;;                      "signal::destroy", gtk_widget_destroyed, &menu->toplevel,
;;;                      NULL);
;;; 
;;; object :
;;; 	a GObject
;;; 
;;; signal_spec :
;;; 	the spec for the first signal
;;; 
;;; ... :
;;; 	GCallback for the first signal, followed by data for the first signal, followed optionally by more signal spec/callback/data triples, followed by NULL
;;; 
;;; Returns :
;;; 	object. [transfer none]
;;; g_object_disconnect ()
;;; 
;;; void                g_object_disconnect                 (gpointer object,
;;;                                                          const gchar *signal_spec,
;;;                                                          ...);
;;; 
;;; A convenience function to disconnect multiple signals at once.
;;; 
;;; The signal specs expected by this function have the form "any_signal", which means to disconnect any signal with matching callback and data, or "any_signal::signal_name", which only disconnects the signal named "signal_name".
;;; 
;;; object :
;;; 	a GObject
;;; 
;;; signal_spec :
;;; 	the spec for the first signal
;;; 
;;; ... :
;;; 	GCallback for the first signal, followed by data for the first signal, followed optionally by more signal spec/callback/data triples, followed by NULL
;;; g_object_set ()
;;; 
;;; void                g_object_set                        (gpointer object,
;;;                                                          const gchar *first_property_name,
;;;                                                          ...);
;;; 
;;; Sets properties on an object.
;;; 
;;; object :
;;; 	a GObject
;;; 
;;; first_property_name :
;;; 	name of the first property to set
;;; 
;;; ... :
;;; 	value for the first property, followed optionally by more name/value pairs, followed by NULL
;;; g_object_get ()
;;; 
;;; void                g_object_get                        (gpointer object,
;;;                                                          const gchar *first_property_name,
;;;                                                          ...);
;;; 
;;; Gets properties of an object.
;;; 
;;; In general, a copy is made of the property contents and the caller is responsible for freeing the memory in the appropriate manner for the type, for instance by calling g_free() or g_object_unref().
;;; 
;;; Example 2. Using g_object_get()
;;; An example of using g_object_get() to get the contents of three properties - one of type G_TYPE_INT, one of type G_TYPE_STRING, and one of type G_TYPE_OBJECT:
;;; 
;;;  gint intval;
;;;  gchar *strval;
;;;  GObject *objval;
;;; 
;;;  g_object_get (my_object,
;;;                "int-property", &intval,
;;;                "str-property", &strval,
;;;                "obj-property", &objval,
;;;                NULL);
;;; 
;;;  // Do something with intval, strval, objval
;;; 
;;;  g_free (strval);
;;;  g_object_unref (objval);
;;; 
;;; 
;;; object :
;;; 	a GObject
;;; 
;;; first_property_name :
;;; 	name of the first property to get
;;; 
;;; ... :
;;; 	return location for the first property, followed optionally by more name/return location pairs, followed by NULL
;;; g_object_notify ()
;;; 
;;; void                g_object_notify                     (GObject *object,
;;;                                                          const gchar *property_name);
;;; 
;;; Emits a "notify" signal for the property property_name on object.
;;; 
;;; When possible, eg. when signaling a property change from within the class that registered the property, you should use g_object_notify_by_pspec() instead.
;;; 
;;; object :
;;; 	a GObject
;;; 
;;; property_name :
;;; 	the name of a property installed on the class of object.
;;; g_object_notify_by_pspec ()
;;; 
;;; void                g_object_notify_by_pspec            (GObject *object,
;;;                                                          GParamSpec *pspec);
;;; 
;;; Emits a "notify" signal for the property specified by pspec on object.
;;; 
;;; This function omits the property name lookup, hence it is faster than g_object_notify().
;;; 
;;; One way to avoid using g_object_notify() from within the class that registered the properties, and using g_object_notify_by_pspec() instead, is to store the GParamSpec used with g_object_class_install_property() inside a static array, e.g.:
;;; 
;;; 1
;;; 2
;;; 3
;;; 4
;;; 5
;;; 6
;;; 7
;;; 8
;;; 9
;;; 10
;;; 11
;;; 12
;;; 13
;;; 14
;;; 15
;;; 16
;;; 17
;;; 18
;;; 19
;;; 20
;;; 
;;; 	
;;; 
;;; enum
;;; {
;;;   PROP_0,
;;;   PROP_FOO,
;;;   PROP_LAST
;;; };
;;; 
;;; static GParamSpec *properties[PROP_LAST];
;;; 
;;; static void
;;; my_object_class_init (MyObjectClass *klass)
;;; {
;;;   properties[PROP_FOO] = g_param_spec_int ("foo", "Foo", "The foo",
;;;                                            0, 100,
;;;                                            50,
;;;                                            G_PARAM_READWRITE);
;;;   g_object_class_install_property (gobject_class,
;;;                                    PROP_FOO,
;;;                                    properties[PROP_FOO]);
;;; }
;;; 
;;; and then notify a change on the "foo" property with:
;;; 
;;; 1
;;; 
;;; 	
;;; 
;;; g_object_notify_by_pspec (self, properties[PROP_FOO]);
;;; 
;;; object :
;;; 	a GObject
;;; 
;;; pspec :
;;; 	the GParamSpec of a property installed on the class of object.
;;; 
;;; Since 2.26
;;; g_object_freeze_notify ()
;;; 
;;; void                g_object_freeze_notify              (GObject *object);
;;; 
;;; Increases the freeze count on object. If the freeze count is non-zero, the emission of "notify" signals on object is stopped. The signals are queued until the freeze count is decreased to zero.
;;; 
;;; This is necessary for accessors that modify multiple properties to prevent premature notification while the object is still being modified.
;;; 
;;; object :
;;; 	a GObject
;;; g_object_thaw_notify ()
;;; 
;;; void                g_object_thaw_notify                (GObject *object);
;;; 
;;; Reverts the effect of a previous call to g_object_freeze_notify(). The freeze count is decreased on object and when it reaches zero, all queued "notify" signals are emitted.
;;; 
;;; It is an error to call this function when the freeze count is zero.
;;; 
;;; object :
;;; 	a GObject
;;; g_object_get_data ()
;;; 
;;; gpointer            g_object_get_data                   (GObject *object,
;;;                                                          const gchar *key);
;;; 
;;; Gets a named field from the objects table of associations (see g_object_set_data()).
;;; 
;;; object :
;;; 	GObject containing the associations
;;; 
;;; key :
;;; 	name of the key for that association
;;; 
;;; Returns :
;;; 	the data if found, or NULL if no such data exists. [transfer none]
;;; g_object_set_data ()
;;; 
;;; void                g_object_set_data                   (GObject *object,
;;;                                                          const gchar *key,
;;;                                                          gpointer data);
;;; 
;;; Each object carries around a table of associations from strings to pointers. This function lets you set an association.
;;; 
;;; If the object already had an association with that name, the old association will be destroyed.
;;; 
;;; object :
;;; 	GObject containing the associations.
;;; 
;;; key :
;;; 	name of the key
;;; 
;;; data :
;;; 	data to associate with that key
;;; g_object_set_data_full ()
;;; 
;;; void                g_object_set_data_full              (GObject *object,
;;;                                                          const gchar *key,
;;;                                                          gpointer data,
;;;                                                          GDestroyNotify destroy);
;;; 
;;; Like g_object_set_data() except it adds notification for when the association is destroyed, either by setting it to a different value or when the object is destroyed.
;;; 
;;; Note that the destroy callback is not called if data is NULL.
;;; 
;;; object :
;;; 	GObject containing the associations
;;; 
;;; key :
;;; 	name of the key
;;; 
;;; data :
;;; 	data to associate with that key
;;; 
;;; destroy :
;;; 	function to call when the association is destroyed
;;; g_object_steal_data ()
;;; 
;;; gpointer            g_object_steal_data                 (GObject *object,
;;;                                                          const gchar *key);
;;; 
;;; Remove a specified datum from the object's data associations, without invoking the association's destroy handler.
;;; 
;;; object :
;;; 	GObject containing the associations
;;; 
;;; key :
;;; 	name of the key
;;; 
;;; Returns :
;;; 	the data if found, or NULL if no such data exists. [transfer full]
;;; g_object_get_qdata ()
;;; 
;;; gpointer            g_object_get_qdata                  (GObject *object,
;;;                                                          GQuark quark);
;;; 
;;; This function gets back user data pointers stored via g_object_set_qdata().
;;; 
;;; object :
;;; 	The GObject to get a stored user data pointer from
;;; 
;;; quark :
;;; 	A GQuark, naming the user data pointer
;;; 
;;; Returns :
;;; 	The user data pointer set, or NULL. [transfer none]
;;; g_object_set_qdata ()
;;; 
;;; void                g_object_set_qdata                  (GObject *object,
;;;                                                          GQuark quark,
;;;                                                          gpointer data);
;;; 
;;; This sets an opaque, named pointer on an object. The name is specified through a GQuark (retrived e.g. via g_quark_from_static_string()), and the pointer can be gotten back from the object with g_object_get_qdata() until the object is finalized. Setting a previously set user data pointer, overrides (frees) the old pointer set, using NULL as pointer essentially removes the data stored.
;;; 
;;; object :
;;; 	The GObject to set store a user data pointer
;;; 
;;; quark :
;;; 	A GQuark, naming the user data pointer
;;; 
;;; data :
;;; 	An opaque user data pointer
;;; g_object_set_qdata_full ()
;;; 
;;; void                g_object_set_qdata_full             (GObject *object,
;;;                                                          GQuark quark,
;;;                                                          gpointer data,
;;;                                                          GDestroyNotify destroy);
;;; 
;;; This function works like g_object_set_qdata(), but in addition, a void (*destroy) (gpointer) function may be specified which is called with data as argument when the object is finalized, or the data is being overwritten by a call to g_object_set_qdata() with the same quark.
;;; 
;;; object :
;;; 	The GObject to set store a user data pointer
;;; 
;;; quark :
;;; 	A GQuark, naming the user data pointer
;;; 
;;; data :
;;; 	An opaque user data pointer
;;; 
;;; destroy :
;;; 	Function to invoke with data as argument, when data needs to be freed
;;; g_object_steal_qdata ()
;;; 
;;; gpointer            g_object_steal_qdata                (GObject *object,
;;;                                                          GQuark quark);
;;; 
;;; This function gets back user data pointers stored via g_object_set_qdata() and removes the data from object without invoking its destroy() function (if any was set). Usually, calling this function is only required to update user data pointers with a destroy notifier, for example:
;;; 
;;; 1
;;; 2
;;; 3
;;; 4
;;; 5
;;; 6
;;; 7
;;; 8
;;; 9
;;; 10
;;; 11
;;; 12
;;; 13
;;; 14
;;; 15
;;; 16
;;; 17
;;; 18
;;; 19
;;; 20
;;; 21
;;; 22
;;; 23
;;; 
;;; 	
;;; 
;;; void
;;; object_add_to_user_list (GObject     *object,
;;;                          const gchar *new_string)
;;; {
;;;   // the quark, naming the object data
;;;   GQuark quark_string_list = g_quark_from_static_string ("my-string-list");
;;;   // retrive the old string list
;;;   GList *list = g_object_steal_qdata (object, quark_string_list);
;;; 
;;;   // prepend new string
;;;   list = g_list_prepend (list, g_strdup (new_string));
;;;   // this changed 'list', so we need to set it again
;;;   g_object_set_qdata_full (object, quark_string_list, list, free_string_list);
;;; }
;;; static void
;;; free_string_list (gpointer data)
;;; {
;;;   GList *node, *list = data;
;;; 
;;;   for (node = list; node; node = node->next)
;;;     g_free (node->data);
;;;   g_list_free (list);
;;; }
;;; 
;;; Using g_object_get_qdata() in the above example, instead of g_object_steal_qdata() would have left the destroy function set, and thus the partial string list would have been freed upon g_object_set_qdata_full().
;;; 
;;; object :
;;; 	The GObject to get a stored user data pointer from
;;; 
;;; quark :
;;; 	A GQuark, naming the user data pointer
;;; 
;;; Returns :
;;; 	The user data pointer set, or NULL. [transfer full]
;;; g_object_set_property ()
;;; 
;;; void                g_object_set_property               (GObject *object,
;;;                                                          const gchar *property_name,
;;;                                                          const GValue *value);
;;; 
;;; Sets a property on an object.
;;; 
;;; object :
;;; 	a GObject
;;; 
;;; property_name :
;;; 	the name of the property to set
;;; 
;;; value :
;;; 	the value
;;; g_object_get_property ()
;;; 
;;; void                g_object_get_property               (GObject *object,
;;;                                                          const gchar *property_name,
;;;                                                          GValue *value);
;;; 
;;; Gets a property of an object. value must have been initialized to the expected type of the property (or a type to which the expected type can be transformed) using g_value_init().
;;; 
;;; In general, a copy is made of the property contents and the caller is responsible for freeing the memory by calling g_value_unset().
;;; 
;;; Note that g_object_get_property() is really intended for language bindings, g_object_get() is much more convenient for C programming.
;;; 
;;; object :
;;; 	a GObject
;;; 
;;; property_name :
;;; 	the name of the property to get
;;; 
;;; value :
;;; 	return location for the property value
;;; g_object_new_valist ()
;;; 
;;; GObject *           g_object_new_valist                 (GType object_type,
;;;                                                          const gchar *first_property_name,
;;;                                                          va_list var_args);
;;; 
;;; Creates a new instance of a GObject subtype and sets its properties.
;;; 
;;; Construction parameters (see G_PARAM_CONSTRUCT, G_PARAM_CONSTRUCT_ONLY) which are not explicitly specified are set to their default values.
;;; 
;;; object_type :
;;; 	the type id of the GObject subtype to instantiate
;;; 
;;; first_property_name :
;;; 	the name of the first property
;;; 
;;; var_args :
;;; 	the value of the first property, followed optionally by more name/value pairs, followed by NULL
;;; 
;;; Returns :
;;; 	a new instance of object_type
;;; g_object_set_valist ()
;;; 
;;; void                g_object_set_valist                 (GObject *object,
;;;                                                          const gchar *first_property_name,
;;;                                                          va_list var_args);
;;; 
;;; Sets properties on an object.
;;; 
;;; object :
;;; 	a GObject
;;; 
;;; first_property_name :
;;; 	name of the first property to set
;;; 
;;; var_args :
;;; 	value for the first property, followed optionally by more name/value pairs, followed by NULL
;;; g_object_get_valist ()
;;; 
;;; void                g_object_get_valist                 (GObject *object,
;;;                                                          const gchar *first_property_name,
;;;                                                          va_list var_args);
;;; 
;;; Gets properties of an object.
;;; 
;;; In general, a copy is made of the property contents and the caller is responsible for freeing the memory in the appropriate manner for the type, for instance by calling g_free() or g_object_unref().
;;; 
;;; See g_object_get().
;;; 
;;; object :
;;; 	a GObject
;;; 
;;; first_property_name :
;;; 	name of the first property to get
;;; 
;;; var_args :
;;; 	return location for the first property, followed optionally by more name/return location pairs, followed by NULL
;;; g_object_watch_closure ()
;;; 
;;; void                g_object_watch_closure              (GObject *object,
;;;                                                          GClosure *closure);
;;; 
;;; This function essentially limits the life time of the closure to the life time of the object. That is, when the object is finalized, the closure is invalidated by calling g_closure_invalidate() on it, in order to prevent invocations of the closure with a finalized (nonexisting) object. Also, g_object_ref() and g_object_unref() are added as marshal guards to the closure, to ensure that an extra reference count is held on object during invocation of the closure. Usually, this function will be called on closures that use this object as closure data.
;;; 
;;; object :
;;; 	GObject restricting lifetime of closure
;;; 
;;; closure :
;;; 	GClosure to watch
;;; g_object_run_dispose ()
;;; 
;;; void                g_object_run_dispose                (GObject *object);
;;; 
;;; Releases all references to other objects. This can be used to break reference cycles.
;;; 
;;; This functions should only be called from object system implementations.
;;; 
;;; object :
;;; 	a GObject
;;; G_OBJECT_WARN_INVALID_PROPERTY_ID()
;;; 
;;; #define             G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec)
;;; 
;;; This macro should be used to emit a standard warning about unexpected properties in set_property() and get_property() implementations.
;;; 
;;; object :
;;; 	the GObject on which set_property() or get_property() was called
;;; 
;;; property_id :
;;; 	the numeric id of the property
;;; 
;;; pspec :
;;; 	the GParamSpec of the property
;;; Signal Details
;;; The "notify" signal
;;; 
;;; void                user_function                      (GObject    *gobject,
;;;                                                         GParamSpec *pspec,
;;;                                                         gpointer    user_data)      : No Hooks
;;; 
;;; The notify signal is emitted on an object when one of its properties has been changed. Note that getting this signal doesn't guarantee that the value of the property has actually changed, it may also be emitted when the setter for the property is called to reinstate the previous value.
;;; 
;;; This signal is typically used to obtain change notification for a single property, by specifying the property name as a detail in the g_signal_connect() call, like this:
;;; 
;;; 1
;;; 2
;;; 3
;;; 
;;; 	
;;; 
;;; g_signal_connect (text_view->buffer, "notify::paste-target-list",
;;;                   G_CALLBACK (gtk_text_view_target_list_notify),
;;;                   text_view)
;;; 
;;; It is important to note that you must use canonical parameter names as detail strings for the notify signal.
;;; 
;;; gobject :
;;; 	the object which received the signal.
;;; 
;;; pspec :
;;; 	the GParamSpec of the property which changed.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; See Also
;;; GParamSpecObject, g_param_spec_object()
;;; |#
