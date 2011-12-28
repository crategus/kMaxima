;;; ----------------------------------------------------------------------------
;;; gobject.closures.lisp
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
;;; Closures
;;;
;;; Functions as first-class objects
;;; 
;;; Synopsis
;;; 
;;; #define             G_CLOSURE_NEEDS_MARSHAL             (closure)
;;; #define             G_CLOSURE_N_NOTIFIERS               (cl)
;;; #define             G_CCLOSURE_SWAP_DATA                (cclosure)
;;; #define             G_CALLBACK                          (f)
;;; void                (*GCallback)                        (void);
;;; struct              GClosure;
;;; #define             G_TYPE_CLOSURE
;;; struct              GCClosure;
;;; void                (*GClosureMarshal)                  (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                (*GClosureNotify)                   (gpointer data,
;;;                                                          GClosure *closure);
;;; GClosure *          g_cclosure_new                      (GCallback callback_func,
;;;                                                          gpointer user_data,
;;;                                                          GClosureNotify destroy_data);
;;; GClosure *          g_cclosure_new_swap                 (GCallback callback_func,
;;;                                                          gpointer user_data,
;;;                                                          GClosureNotify destroy_data);
;;; GClosure *          g_cclosure_new_object               (GCallback callback_func,
;;;                                                          GObject *object);
;;; GClosure *          g_cclosure_new_object_swap          (GCallback callback_func,
;;;                                                          GObject *object);
;;; void                g_cclosure_marshal_generic          (GClosure *closure,
;;;                                                          GValue *return_gvalue,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; GClosure *          g_closure_new_object                (guint sizeof_closure,
;;;                                                          GObject *object);
;;; GClosure *          g_closure_ref                       (GClosure *closure);
;;; void                g_closure_sink                      (GClosure *closure);
;;; void                g_closure_unref                     (GClosure *closure);
;;; void                g_closure_invoke                    (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint);
;;; void                g_closure_invalidate                (GClosure *closure);
;;; void                g_closure_add_finalize_notifier     (GClosure *closure,
;;;                                                          gpointer notify_data,
;;;                                                          GClosureNotify notify_func);
;;; void                g_closure_add_invalidate_notifier   (GClosure *closure,
;;;                                                          gpointer notify_data,
;;;                                                          GClosureNotify notify_func);
;;; void                g_closure_remove_finalize_notifier  (GClosure *closure,
;;;                                                          gpointer notify_data,
;;;                                                          GClosureNotify notify_func);
;;; void                g_closure_remove_invalidate_notifier
;;;                                                         (GClosure *closure,
;;;                                                          gpointer notify_data,
;;;                                                          GClosureNotify notify_func);
;;; GClosure *          g_closure_new_simple                (guint sizeof_closure,
;;;                                                          gpointer data);
;;; void                g_closure_set_marshal               (GClosure *closure,
;;;                                                          GClosureMarshal marshal);
;;; void                g_closure_add_marshal_guards        (GClosure *closure,
;;;                                                          gpointer pre_marshal_data,
;;;                                                          GClosureNotify pre_marshal_notify,
;;;                                                          gpointer post_marshal_data,
;;;                                                          GClosureNotify post_marshal_notify);
;;; void                g_closure_set_meta_marshal          (GClosure *closure,
;;;                                                          gpointer marshal_data,
;;;                                                          GClosureMarshal meta_marshal);
;;; void                g_source_set_closure                (GSource *source,
;;;                                                          GClosure *closure);
;;; void                g_source_set_dummy_callback         (GSource *source);
;;; 
;;; void                g_cclosure_marshal_VOID__VOID       (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__BOOLEAN    (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__CHAR       (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__UCHAR      (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__INT        (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__UINT       (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__LONG       (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__ULONG      (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__ENUM       (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__FLAGS      (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__FLOAT      (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__DOUBLE     (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__STRING     (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__PARAM      (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__BOXED      (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__POINTER    (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__OBJECT     (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__VARIANT    (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_STRING__OBJECT_POINTER
;;;                                                         (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_VOID__UINT_POINTER
;;;                                                         (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; void                g_cclosure_marshal_BOOLEAN__FLAGS   (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; #define             g_cclosure_marshal_BOOL__FLAGS
;;; void                g_cclosure_marshal_BOOLEAN__BOXED_BOXED
;;;                                                         (GClosure *closure,
;;;                                                          GValue *return_value,
;;;                                                          guint n_param_values,
;;;                                                          const GValue *param_values,
;;;                                                          gpointer invocation_hint,
;;;                                                          gpointer marshal_data);
;;; #define             g_cclosure_marshal_BOOL__BOXED_BOXED
;;; 
;;; Description
;;; 
;;; A GClosure represents a callback supplied by the programmer. It will
;;; generally comprise a function of some kind and a marshaller used to call it.
;;; It is the reponsibility of the marshaller to convert the arguments for the
;;; invocation from GValues into a suitable form, perform the callback on the
;;; converted arguments, and transform the return value back into a GValue.
;;; 
;;; In the case of C programs, a closure usually just holds a pointer to a
;;; function and maybe a data argument, and the marshaller converts between
;;; GValue and native C types. The GObject library provides the GCClosure type
;;; for this purpose. Bindings for other languages need marshallers which
;;; convert between GValues and suitable representations in the runtime of the
;;; language in order to use functions written in that languages as callbacks.
;;; 
;;; Within GObject, closures play an important role in the implementation of
;;; signals. When a signal is registered, the c_marshaller argument to
;;; g_signal_new() specifies the default C marshaller for any closure which is
;;; connected to this signal. GObject provides a number of C marshallers for
;;; this purpose, see the g_cclosure_marshal_*() functions. Additional
;;; C marshallers can be generated with the glib-genmarshal utility. Closures
;;; can be explicitly connected to signals with g_signal_connect_closure(), but
;;; it usually more convenient to let GObject create a closure automatically by
;;; using one of the g_signal_connect_*() functions which take a callback
;;; function/user data pair.
;;; 
;;; Using closures has a number of important advantages over a simple callback
;;; function/data pointer combination:
;;; 
;;; * Closures allow the callee to get the types of the callback parameters,
;;;   which means that language bindings don't have to write individual
;;;   glue for each callback type.
;;; * The reference counting of GClosure makes it easy to handle reentrancy
;;;   right; if a callback is removed while it is being invoked, the closure
;;;   and its parameters won't be freed until the invocation finishes.
;;; * g_closure_invalidate() and invalidation notifiers allow callbacks to
;;;   be automatically removed when the objects they point to go away.
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------
;;; G_CLOSURE_NEEDS_MARSHAL()
;;; 
;;; #define G_CLOSURE_NEEDS_MARSHAL(closure)
;;;         (((GClosure*) (closure))->marshal == NULL)
;;; 
;;; Check if the closure still needs a marshaller. See g_closure_set_marshal().
;;; 
;;; closure :
;;; 	a GClosure
;;; 
;;; Returns :
;;; 	TRUE if a GClosureMarshal marshaller has not yet been set on closure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_CLOSURE_N_NOTIFIERS()
;;; 
;;; #define G_CLOSURE_N_NOTIFIERS(cl)
;;; 
;;; Get the total number of notifiers connected with the closure cl. The count
;;; includes the meta marshaller, the finalize and invalidate notifiers and the
;;; marshal guards. Note that each guard counts as two notifiers. See
;;; g_closure_set_meta_marshal(), g_closure_add_finalize_notifier(),
;;; g_closure_add_invalidate_notifier() and g_closure_add_marshal_guards().
;;; 
;;; cl :
;;; 	a GClosure
;;; 
;;; Returns :
;;; 	number of notifiers
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_CCLOSURE_SWAP_DATA()
;;; 
;;; #define G_CCLOSURE_SWAP_DATA(cclosure)
;;;         (((GClosure*) (cclosure))->derivative_flag)
;;; 
;;; Checks whether the user data of the GCClosure should be passed as the first
;;; parameter to the callback. See g_cclosure_new_swap().
;;; 
;;; cclosure :
;;; 	a GCClosure
;;; 
;;; Returns :
;;; 	TRUE if data has to be swapped.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_CALLBACK()
;;; 
;;; #define G_CALLBACK(f) ((GCallback) (f))
;;; 
;;; Cast a function pointer to a GCallback.
;;; 
;;; f :
;;; 	a function pointer.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GCallback ()
;;; 
;;; void (*GCallback) (void);
;;; 
;;; The type used for callback functions in structure definitions and function
;;; signatures. This doesn't mean that all callback functions must take no
;;; parameters and return void. The required signature of a callback function
;;; is determined by the context in which is used (e.g. the signal to which it
;;; is connected). Use G_CALLBACK() to cast the callback function to a
;;; GCallback.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GClosure
;;; 
;;; struct GClosure {
;;;   volatile guint in_marshal : 1;
;;;   volatile guint is_invalid : 1;
;;; };
;;; 
;;; A GClosure represents a callback supplied by the programmer.
;;; 
;;; volatile guint in_marshal : 1;
;;; 	Indicates whether the closure is currently being invoked with
;;;     g_closure_invoke()
;;; 
;;; volatile guint is_invalid : 1;
;;; 	Indicates whether the closure has been invalidated by
;;;     g_closure_invalidate()
;;; ----------------------------------------------------------------------------

(defcstruct g-closure
  (:private-data :uint32)
  (:marshal :pointer)
  (:data :pointer)
  (:notifiers :pointer))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CLOSURE
;;; 
;;; #define G_TYPE_CLOSURE (g_closure_get_type ())
;;; 
;;; The GType for GClosure.
;;; struct GCClosure
;;; 
;;; struct GCClosure {
;;;   GClosure closure;
;;;   gpointer callback;
;;; };
;;; 
;;; A GCClosure is a specialization of GClosure for C function callbacks.
;;; 
;;; GClosure closure;
;;; 	the GClosure
;;; 
;;; gpointer callback;
;;; 	the callback function
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GClosureMarshal ()
;;; 
;;; void (*GClosureMarshal) (GClosure *closure,
;;;                          GValue *return_value,
;;;                          guint n_param_values,
;;;                          const GValue *param_values,
;;;                          gpointer invocation_hint,
;;;                          gpointer marshal_data);
;;; 
;;; The type used for marshaller functions.
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	a GValue to store the return value. May be NULL if the callback of
;;;     closure doesn't return a value.
;;; 
;;; n_param_values :
;;; 	the length of the param_values array
;;; 
;;; param_values :
;;; 	an array of GValues holding the arguments on which to invoke the
;;;     callback of closure.
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke().
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller, see
;;;     g_closure_set_marshal() and g_closure_set_meta_marshal().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GClosureNotify ()
;;; 
;;; void (*GClosureNotify) (gpointer data, GClosure *closure);
;;; 
;;; The type used for the various notification callbacks which can be
;;; registered on closures.
;;; 
;;; data :
;;; 	data specified when registering the notification callback
;;; 
;;; closure :
;;; 	the GClosure on which the notification is emitted
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_new ()
;;; 
;;; GClosure * g_cclosure_new (GCallback callback_func,
;;;                            gpointer user_data,
;;;                            GClosureNotify destroy_data);
;;; 
;;; Creates a new closure which invokes callback_func with user_data as the
;;; last parameter.
;;; 
;;; callback_func :
;;; 	the function to invoke
;;; 
;;; user_data :
;;; 	user data to pass to callback_func
;;; 
;;; destroy_data :
;;; 	destroy notify to be called when user_data is no longer used
;;; 
;;; Returns :
;;; 	a new GCClosure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_new_swap ()
;;; 
;;; GClosure * g_cclosure_new_swap (GCallback callback_func,
;;;                                 gpointer user_data,
;;;                                 GClosureNotify destroy_data);
;;; 
;;; Creates a new closure which invokes callback_func with user_data as the
;;; first parameter.
;;; 
;;; callback_func :
;;; 	the function to invoke
;;; 
;;; user_data :
;;; 	user data to pass to callback_func
;;; 
;;; destroy_data :
;;; 	destroy notify to be called when user_data is no longer used
;;; 
;;; Returns :
;;; 	a new GCClosure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_new_object ()
;;; 
;;; GClosure * g_cclosure_new_object (GCallback callback_func, GObject *object)
;;; 
;;; A variant of g_cclosure_new() which uses object as user_data and calls
;;; g_object_watch_closure() on object and the created closure. This function
;;; is useful when you have a callback closely associated with a GObject, and
;;; want the callback to no longer run after the object is is freed.
;;; 
;;; callback_func :
;;; 	the function to invoke
;;; 
;;; object :
;;; 	a GObject pointer to pass to callback_func
;;; 
;;; Returns :
;;; 	a new GCClosure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_new_object_swap ()
;;; 
;;; GClosure * g_cclosure_new_object_swap (GCallback callback_func,
;;;                                        GObject *object);
;;; 
;;; A variant of g_cclosure_new_swap() which uses object as user_data and calls
;;; g_object_watch_closure() on object and the created closure. This function
;;; is useful when you have a callback closely associated with a GObject, and
;;; want the callback to no longer run after the object is is freed.
;;; 
;;; callback_func :
;;; 	the function to invoke
;;; 
;;; object :
;;; 	a GObject pointer to pass to callback_func
;;; 
;;; Returns :
;;; 	a new GCClosure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_generic ()
;;; 
;;; void g_cclosure_marshal_generic (GClosure *closure,
;;;                                  GValue *return_gvalue,
;;;                                  guint n_param_values,
;;;                                  const GValue *param_values,
;;;                                  gpointer invocation_hint,
;;;                                  gpointer marshal_data);
;;; 
;;; A generic marshaller function implemented via libffi.
;;; 
;;; closure :
;;; 	A GClosure.
;;; 
;;; return_gvalue :
;;; 	A GValue to store the return value. May be NULL if the callback of
;;;     closure doesn't return a value.
;;; 
;;; n_param_values :
;;; 	The length of the param_values array.
;;; 
;;; param_values :
;;; 	An array of GValues holding the arguments on which to invoke the
;;;     callback of closure.
;;; 
;;; invocation_hint :
;;; 	The invocation hint given as the last argument to g_closure_invoke().
;;; 
;;; marshal_data :
;;; 	Additional data specified when registering the marshaller, see
;;;     g_closure_set_marshal() and g_closure_set_meta_marshal()
;;; 
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_closure_new_object ()
;;; 
;;; GClosure * g_closure_new_object (guint sizeof_closure, GObject *object);
;;; 
;;; A variant of g_closure_new_simple() which stores object in the data field
;;; of the closure and calls g_object_watch_closure() on object and the created
;;; closure. This function is mainly useful when implementing new types of
;;; closures.
;;; 
;;; sizeof_closure :
;;; 	the size of the structure to allocate, must be at least sizeof
;;;     (GClosure)
;;; 
;;; object :
;;; 	a GObject pointer to store in the data field of the newly allocated
;;;     GClosure
;;; 
;;; Returns :
;;; 	a newly allocated GClosure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_closure_ref ()
;;; 
;;; GClosure * g_closure_ref (GClosure *closure)
;;; 
;;; Increments the reference count on a closure to force it staying alive while
;;; the caller holds a pointer to it.
;;; 
;;; closure :
;;;     GClosure to increment the reference count on
;;; 
;;; Returns :
;;;     The closure passed in, for convenience. [transfer none]
;;; ----------------------------------------------------------------------------

(defcfun g-closure-ref (:pointer g-closure)
  (closure (:pointer g-closure)))

;;; ----------------------------------------------------------------------------
;;; g_closure_sink ()
;;; 
;;; void g_closure_sink (GClosure *closure)
;;; 
;;; Takes over the initial ownership of a closure. Each closure is initially
;;; created in a floating state, which means that the initial reference count
;;; is not owned by any caller. g_closure_sink() checks to see if the object is
;;; still floating, and if so, unsets the floating state and decreases the
;;; reference count. If the closure is not floating, g_closure_sink() does
;;; nothing. The reason for the existence of the floating state is to prevent
;;; cumbersome code sequences like:
;;;
;;;  1 closure = g_cclosure_new (cb_func, cb_data);
;;;  2 g_source_set_closure (source, closure);
;;;  3 g_closure_unref (closure); // XXX GObject doesn't really need this
;;; 
;;; Because g_source_set_closure() (and similar functions) take ownership of
;;; the initial reference count, if it is unowned, we instead can write:	
;;; 
;;;  1 g_source_set_closure (source, g_cclosure_new (cb_func, cb_data));
;;; 
;;; Generally, this function is used together with g_closure_ref(). Ane example
;;; of storing a closure for later notification looks like:
;;; 
;;;  1 static GClosure *notify_closure = NULL;
;;;  2 void
;;;  3 foo_notify_set_closure (GClosure *closure)
;;;  4 {
;;;  5   if (notify_closure)
;;;  6     g_closure_unref (notify_closure);
;;;  7   notify_closure = closure;
;;;  8   if (notify_closure)
;;;  9     {
;;; 10       g_closure_ref (notify_closure);
;;; 11       g_closure_sink (notify_closure);
;;; 12     }
;;; 13 }
;;; 
;;; Because g_closure_sink() may decrement the reference count of a closure
;;; (if it hasn't been called on closure yet) just like g_closure_unref(),
;;; g_closure_ref() should be called prior to this function.
;;; 
;;; closure :
;;; 	GClosure to decrement the initial reference count on, if it's still
;;;     being held
;;; ----------------------------------------------------------------------------

(defcfun g-closure-sink :void
  (closure (:pointer g-closure)))

;;; ----------------------------------------------------------------------------
;;; g_closure_unref ()
;;; 
;;; void g_closure_unref (GClosure *closure)
;;; 
;;; Decrements the reference count of a closure after it was previously
;;; incremented by the same caller. If no other callers are using the closure,
;;; then the closure will be destroyed and freed.
;;; 
;;; closure :
;;; 	GClosure to decrement the reference count on
;;; ----------------------------------------------------------------------------

(defcfun g-closure-unref :void
  (closure (:pointer g-closure)))

;;; ----------------------------------------------------------------------------
;;; g_closure_invoke ()
;;; 
;;; void g_closure_invoke (GClosure *closure,
;;;                        GValue *return_value,
;;;                        guint n_param_values,
;;;                        const GValue *param_values,
;;;                        gpointer invocation_hint);
;;; 
;;; Invokes the closure, i.e. executes the callback represented by the closure.
;;; 
;;; closure :
;;; 	a GClosure
;;; 
;;; return_value :
;;; 	a GValue to store the return value. May be NULL if the callback of
;;;     closure doesn't return a value.
;;; 
;;; n_param_values :
;;; 	the length of the param_values array
;;; 
;;; param_values :
;;; 	an array of GValues holding the arguments on which to invoke the
;;;     callback of closure.
;;; 
;;; invocation_hint :
;;; 	a context-dependent invocation hint.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_closure_invalidate ()
;;; 
;;; void g_closure_invalidate (GClosure *closure)
;;; 
;;; Sets a flag on the closure to indicate that its calling environment has
;;; become invalid, and thus causes any future invocations of g_closure_invoke()
;;; on this closure to be ignored. Also, invalidation notifiers installed on the
;;; closure will be called at this point. Note that unless you are holding a
;;; reference to the closure yourself, the invalidation notifiers may unref the
;;; closure and cause it to be destroyed, so if you need to access the closure
;;; after calling g_closure_invalidate(), make sure that you've previously
;;; called g_closure_ref().
;;; 
;;; Note that g_closure_invalidate() will also be called when the reference
;;; count of a closure drops to zero (unless it has already been invalidated
;;; before).
;;; 
;;; closure :
;;; 	GClosure to invalidate
;;; ----------------------------------------------------------------------------

(defcfun g-closure-invalidate :void
  (closure (:pointer g-closure)))

;;; ----------------------------------------------------------------------------
;;; g_closure_add_finalize_notifier ()
;;; 
;;; void g_closure_add_finalize_notifier (GClosure *closure,
;;;                                       gpointer notify_data,
;;;                                       GClosureNotify notify_func)
;;; 
;;; Registers a finalization notifier which will be called when the reference
;;; count of closure goes down to 0. Multiple finalization notifiers on a
;;; single closure are invoked in unspecified order. If a single call to
;;; g_closure_unref() results in the closure being both invalidated and
;;; finalized, then the invalidate notifiers will be run before the finalize
;;; notifiers.
;;; 
;;; closure :
;;; 	a GClosure
;;; 
;;; notify_data :
;;; 	data to pass to notify_func
;;; 
;;; notify_func :
;;; 	the callback function to register
;;; ----------------------------------------------------------------------------

(defcfun g-closure-add-finalize-notifier :void
  (closure (:pointer g-closure))
  (notify-data :pointer)
  (notify-func :pointer))

;;; ----------------------------------------------------------------------------
;;; g_closure_add_invalidate_notifier ()
;;; 
;;; void g_closure_add_invalidate_notifier (GClosure *closure,
;;;                                         gpointer notify_data,
;;;                                         GClosureNotify notify_func)
;;; 
;;; Registers an invalidation notifier which will be called when the closure is
;;; invalidated with g_closure_invalidate(). Invalidation notifiers are invoked
;;; before finalization notifiers, in an unspecified order.
;;; 
;;; closure :
;;; 	a GClosure
;;; 
;;; notify_data :
;;; 	data to pass to notify_func
;;; 
;;; notify_func :
;;; 	the callback function to register
;;; ----------------------------------------------------------------------------

(defcfun g-closure-add-invalidate-notifier :void
  (closure (:pointer g-closure))
  (notify-data :pointer)
  (notify-func :pointer))

;;; ----------------------------------------------------------------------------
;;; g_closure_remove_finalize_notifier ()
;;; 
;;; voidg_closure_remove_finalize_notifier (GClosure *closure,
;;;                                         gpointer notify_data,
;;;                                         GClosureNotify notify_func);
;;; 
;;; Removes a finalization notifier.
;;; 
;;; Notice that notifiers are automatically removed after they are run.
;;; 
;;; closure :
;;; 	a GClosure
;;; 
;;; notify_data :
;;; 	data which was passed to g_closure_add_finalize_notifier() when
;;;     registering notify_func
;;; 
;;; notify_func :
;;; 	the callback function to remove
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_closure_remove_invalidate_notifier ()
;;; 
;;; void g_closure_remove_invalidate_notifier (GClosure *closure,
;;;                                            gpointer notify_data,
;;;                                            GClosureNotify notify_func);
;;; 
;;; Removes an invalidation notifier.
;;; 
;;; Notice that notifiers are automatically removed after they are run.
;;; 
;;; closure :
;;; 	a GClosure
;;; 
;;; notify_data :
;;; 	data which was passed to g_closure_add_invalidate_notifier() when
;;;     registering notify_func
;;; 
;;; notify_func :
;;; 	the callback function to remove
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_closure_new_simple ()
;;; 
;;; GClosure * g_closure_new_simple (guint sizeof_closure, gpointer data)
;;; 
;;; Allocates a struct of the given size and initializes the initial part as a
;;; GClosure. This function is mainly useful when implementing new types of
;;; closures.
;;; 
;;;  1 typedef struct _MyClosure MyClosure;
;;;  2 struct _MyClosure
;;;  3 {
;;;  4   GClosure closure;
;;;  5   // extra data goes here
;;;  6 };
;;;  7
;;;  8 static void
;;;  9 my_closure_finalize (gpointer  notify_data,
;;; 10                      GClosure *closure)
;;; 11 {
;;; 12   MyClosure *my_closure = (MyClosure *)closure;
;;; 13 
;;; 14   // free extra data here
;;; 15 }
;;; 16
;;; 17 MyClosure *my_closure_new (gpointer data)
;;; 18 {
;;; 19   GClosure *closure;
;;; 20   MyClosure *my_closure;
;;; 21
;;; 22   closure = g_closure_new_simple (sizeof (MyClosure), data);
;;; 23   my_closure = (MyClosure *) closure;
;;; 24
;;; 25   // initialize extra data here
;;; 26
;;; 27   g_closure_add_finalize_notifier (closure, notify_data,
;;; 28                                    my_closure_finalize);
;;; 29   return my_closure;
;;; 30 }
;;; 
;;; sizeof_closure :
;;;     the size of the structure to allocate, must be at least
;;;     sizeof (GClosure)
;;; 
;;; data :
;;;     data to store in the data field of the newly allocated GClosure
;;; 
;;; Returns :
;;;     a newly allocated GClosure. [transfer full]
;;; ----------------------------------------------------------------------------

(defcfun g-closure-new-simple (:pointer g-closure)
  (sizeof-closure :uint)
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_closure_set_marshal ()
;;; 
;;; void g_closure_set_marshal (GClosure *closure, GClosureMarshal marshal)
;;; 
;;; Sets the marshaller of closure. The marshal_data of marshal provides a way
;;; for a meta marshaller to provide additional information to the marshaller.
;;; (See g_closure_set_meta_marshal().) For GObject's C predefined marshallers
;;; (the g_cclosure_marshal_*() functions), what it provides is a callback
;;; function to use instead of closure->callback.
;;; 
;;; closure :
;;; 	a GClosure
;;; 
;;; marshal :
;;; 	a GClosureMarshal function
;;; ----------------------------------------------------------------------------

(defcfun g-closure-set-marshal :void
  (closure (:pointer g-closure))
  (marshal :pointer))

;;; ----------------------------------------------------------------------------
;;; g_closure_add_marshal_guards ()
;;; 
;;; void g_closure_add_marshal_guards (GClosure *closure,
;;;                                    gpointer pre_marshal_data,
;;;                                    GClosureNotify pre_marshal_notify,
;;;                                    gpointer post_marshal_data,
;;;                                    GClosureNotify post_marshal_notify);
;;; 
;;; Adds a pair of notifiers which get invoked before and after the closure
;;; callback, respectively. This is typically used to protect the extra
;;; arguments for the duration of the callback. See g_object_watch_closure()
;;; for an example of marshal guards.
;;; 
;;; closure :
;;; 	a GClosure
;;; 
;;; pre_marshal_data :
;;; 	data to pass to pre_marshal_notify
;;; 
;;; pre_marshal_notify :
;;; 	a function to call before the closure callback
;;; 
;;; post_marshal_data :
;;; 	data to pass to post_marshal_notify
;;; 
;;; post_marshal_notify :
;;; 	a function to call after the closure callback
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_closure_set_meta_marshal ()
;;; 
;;; void g_closure_set_meta_marshal (GClosure *closure,
;;;                                  gpointer marshal_data,
;;;                                  GClosureMarshal meta_marshal);
;;; 
;;; Sets the meta marshaller of closure. A meta marshaller wraps
;;; closure->marshal and modifies the way it is called in some fashion. The
;;; most common use of this facility is for C callbacks. The same marshallers
;;; (generated by glib-genmarshal) are used everywhere, but the way that we get
;;; the callback function differs. In most cases we want to use
;;; closure->callback, but in other cases we want to use some different
;;; technique to retrieve the callback function.
;;; 
;;; For example, class closures for signals (see g_signal_type_cclosure_new())
;;; retrieve the callback function from a fixed offset in the class structure.
;;; The meta marshaller retrieves the right callback and passes it to the
;;; marshaller as the marshal_data argument.
;;; 
;;; closure :
;;; 	a GClosure
;;; 
;;; marshal_data :
;;; 	context-dependent data to pass to meta_marshal
;;; 
;;; meta_marshal :
;;; 	a GClosureMarshal function
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_set_closure ()
;;; 
;;; void g_source_set_closure (GSource *source, GClosure *closure);
;;; 
;;; Set the callback for a source as a GClosure.
;;; 
;;; If the source is not one of the standard GLib types, the closure_callback
;;; and closure_marshal fields of the GSourceFuncs structure must have been
;;; filled in with pointers to appropriate functions.
;;; 
;;; source :
;;; 	the source
;;; 
;;; closure :
;;; 	a GClosure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_set_dummy_callback ()
;;; 
;;; void g_source_set_dummy_callback (GSource *source)
;;; 
;;; Sets a dummy callback for source. The callback will do nothing, and if the
;;; source expects a gboolean return value, it will return TRUE. (If the source
;;; expects any other type of return value, it will return a 0/NULL value;
;;; whatever g_value_init() initializes a GValue to for that type.)
;;; 
;;; If the source is not one of the standard GLib types, the closure_callback
;;; and closure_marshal fields of the GSourceFuncs structure must have been
;;; filled in with pointers to appropriate functions.
;;; 
;;; source :
;;; 	the source
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__VOID ()
;;; 
;;; void g_cclosure_marshal_VOID__VOID (GClosure *closure,
;;;                                     GValue *return_value,
;;;                                     guint n_param_values,
;;;                                     const GValue *param_values,
;;;                                     gpointer invocation_hint,
;;;                                     gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type void (*callback)
;;; (gpointer instance, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	1
;;; 
;;; param_values :
;;; 	a GValue array holding only the instance
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__BOOLEAN ()
;;; 
;;; void g_cclosure_marshal_VOID__BOOLEAN (GClosure *closure,
;;;                                        GValue *return_value,
;;;                                        guint n_param_values,
;;;                                        const GValue *param_values,
;;;                                        gpointer invocation_hint,
;;;                                        gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type void (*callback)
;;; (gpointer instance, gboolean arg1, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the gboolean parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__CHAR ()
;;; 
;;; void g_cclosure_marshal_VOID__CHAR (GClosure *closure,
;;;                                     GValue *return_value,
;;;                                     guint n_param_values,
;;;                                     const GValue *param_values,
;;;                                     gpointer invocation_hint,
;;;                                     gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type void (*callback)
;;; (gpointer instance, gchar arg1, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the gchar parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__UCHAR ()
;;; 
;;; void g_cclosure_marshal_VOID__UCHAR (GClosure *closure,
;;;                                      GValue *return_value,
;;;                                      guint n_param_values,
;;;                                      const GValue *param_values,
;;;                                      gpointer invocation_hint,
;;;                                      gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, guchar arg1, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the guchar parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__INT ()
;;; 
;;; void g_cclosure_marshal_VOID__INT (GClosure *closure,
;;;                                    GValue *return_value,
;;;                                    guint n_param_values,
;;;                                    const GValue *param_values,
;;;                                    gpointer invocation_hint,
;;;                                    gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, gint arg1, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the gint parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__UINT ()
;;; 
;;; void g_cclosure_marshal_VOID__UINT (GClosure *closure,
;;;                                     GValue *return_value,
;;;                                     guint n_param_values,
;;;                                     const GValue *param_values,
;;;                                     gpointer invocation_hint,
;;;                                     gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, guint arg1, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the guint parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__LONG ()
;;; 
;;; void g_cclosure_marshal_VOID__LONG (GClosure *closure,
;;;                                     GValue *return_value,
;;;                                     guint n_param_values,
;;;                                     const GValue *param_values,
;;;                                     gpointer invocation_hint,
;;;                                     gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, glong arg1, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the glong parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__ULONG ()
;;; 
;;; void g_cclosure_marshal_VOID__ULONG (GClosure *closure,
;;;                                      GValue *return_value,
;;;                                      guint n_param_values,
;;;                                      const GValue *param_values,
;;;                                      gpointer invocation_hint,
;;;                                      gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, gulong arg1, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the gulong parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__ENUM ()
;;; 
;;; void g_cclosure_marshal_VOID__ENUM (GClosure *closure,
;;;                                     GValue *return_value,
;;;                                     guint n_param_values,
;;;                                     const GValue *param_values,
;;;                                     gpointer invocation_hint,
;;;                                     gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, gint arg1, gpointer user_data) where
;;; the gint parameter denotes an enumeration type..
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the enumeration parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__FLAGS ()
;;; 
;;; void g_cclosure_marshal_VOID__FLAGS (GClosure *closure,
;;;                                      GValue *return_value,
;;;                                      guint n_param_values,
;;;                                      const GValue *param_values,
;;;                                      gpointer invocation_hint,
;;;                                      gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, gint arg1, gpointer user_data)
;;; where the gint parameter denotes a flags type.
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the flags parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__FLOAT ()
;;; 
;;; void g_cclosure_marshal_VOID__FLOAT (GClosure *closure,
;;;                                      GValue *return_value,
;;;                                      guint n_param_values,
;;;                                      const GValue *param_values,
;;;                                      gpointer invocation_hint,
;;;                                      gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, gfloat arg1, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the gfloat parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__DOUBLE ()
;;; 
;;; void g_cclosure_marshal_VOID__DOUBLE (GClosure *closure,
;;;                                       GValue *return_value,
;;;                                       guint n_param_values,
;;;                                       const GValue *param_values,
;;;                                       gpointer invocation_hint,
;;;                                       gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, gdouble arg1, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the gdouble parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__STRING ()
;;; 
;;; void g_cclosure_marshal_VOID__STRING (GClosure *closure,
;;;                                       GValue *return_value,
;;;                                       guint n_param_values,
;;;                                       const GValue *param_values,
;;;                                       gpointer invocation_hint,
;;;                                       gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, const gchar *arg1, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the gchar* parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__PARAM ()
;;; 
;;; void g_cclosure_marshal_VOID__PARAM (GClosure *closure,
;;;                                      GValue *return_value,
;;;                                      guint n_param_values,
;;;                                      const GValue *param_values,
;;;                                      gpointer invocation_hint,
;;;                                      gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, GParamSpec *arg1, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the GParamSpec* parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__BOXED ()
;;; 
;;; void g_cclosure_marshal_VOID__BOXED (GClosure *closure,
;;;                                      GValue *return_value,
;;;                                      guint n_param_values,
;;;                                      const GValue *param_values,
;;;                                      gpointer invocation_hint,
;;;                                      gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, GBoxed *arg1, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the GBoxed* parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__POINTER ()
;;; 
;;; void g_cclosure_marshal_VOID__POINTER (GClosure *closure,
;;;                                        GValue *return_value,
;;;                                        guint n_param_values,
;;;                                        const GValue *param_values,
;;;                                        gpointer invocation_hint,
;;;                                        gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, gpointer arg1, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the gpointer parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__OBJECT ()
;;; 
;;; void g_cclosure_marshal_VOID__OBJECT (GClosure *closure,
;;;                                       GValue *return_value,
;;;                                       guint n_param_values,
;;;                                       const GValue *param_values,
;;;                                       gpointer invocation_hint,
;;;                                       gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, GObject *arg1, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the GObject* parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__VARIANT ()
;;; 
;;; void g_cclosure_marshal_VOID__VARIANT (GClosure *closure,
;;;                                        GValue *return_value,
;;;                                        guint n_param_values,
;;;                                        const GValue *param_values,
;;;                                        gpointer invocation_hint,
;;;                                        gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, GVariant *arg1, gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding the instance and the GVariant* parameter
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; 
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_STRING__OBJECT_POINTER ()
;;; 
;;; void g_cclosure_marshal_STRING__OBJECT_POINTER (GClosure *closure,
;;;                                                 GValue *return_value,
;;;                                                 guint n_param_values,
;;;                                                 const GValue *param_values,
;;;                                                 gpointer invocation_hint,
;;;                                                 gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; gchar* (*callback) (gpointer instance, GObject *arg1, gpointer arg2,
;;;                     gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	a GValue, which can store the returned string
;;; 
;;; n_param_values :
;;; 	3
;;; 
;;; param_values :
;;; 	a GValue array holding instance, arg1 and arg2
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_VOID__UINT_POINTER ()
;;; 
;;; void g_cclosure_marshal_VOID__UINT_POINTER (GClosure *closure,
;;;                                             GValue *return_value,
;;;                                             guint n_param_values,
;;;                                             const GValue *param_values,
;;;                                             gpointer invocation_hint,
;;;                                             gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; void (*callback) (gpointer instance, guint arg1, gpointer arg2,
;;;                   gpointer user_data).
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	ignored
;;; 
;;; n_param_values :
;;; 	3
;;; 
;;; param_values :
;;; 	a GValue array holding instance, arg1 and arg2
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_BOOLEAN__FLAGS ()
;;; 
;;; void g_cclosure_marshal_BOOLEAN__FLAGS (GClosure *closure,
;;;                                         GValue *return_value,
;;;                                         guint n_param_values,
;;;                                         const GValue *param_values,
;;;                                         gpointer invocation_hint,
;;;                                         gpointer marshal_data);
;;; 
;;; A marshaller for a GCClosure with a callback of type
;;; gboolean (*callback) (gpointer instance, gint arg1, gpointer user_data)
;;; where the gint parameter denotes a flags type.
;;; 
;;; closure :
;;; 	the GClosure to which the marshaller belongs
;;; 
;;; return_value :
;;; 	a GValue which can store the returned gboolean
;;; 
;;; n_param_values :
;;; 	2
;;; 
;;; param_values :
;;; 	a GValue array holding instance and arg1
;;; 
;;; invocation_hint :
;;; 	the invocation hint given as the last argument to g_closure_invoke()
;;; 
;;; marshal_data :
;;; 	additional data specified when registering the marshaller
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_BOOL__FLAGS
;;; 
;;; #define g_cclosure_marshal_BOOL__FLAGS
;;; 
;;; Another name for g_cclosure_marshal_BOOLEAN__FLAGS().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_BOOLEAN__BOXED_BOXED ()
;;; 
;;; void g_cclosure_marshal_BOOLEAN__BOXED_BOXED (GClosure *closure,
;;;                                               GValue *return_value,
;;;                                               guint n_param_values,
;;;                                               const GValue *param_values,
;;;                                               gpointer invocation_hint,
;;;                                               gpointer marshal_data);
;;; ----------------------------------------------------------------------------
;;; g_cclosure_marshal_BOOL__BOXED_BOXED
;;; 
;;; #define g_cclosure_marshal_BOOL__BOXED_BOXED
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.closure.lisp ---------------------------------------
