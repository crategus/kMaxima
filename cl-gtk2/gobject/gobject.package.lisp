(defpackage :gobject
  (:nicknames :g)
  (:use :c2cl :glib :cffi :tg :bordeaux-threads :iter :closer-mop)
  (:export #:g-type
           #:gtype
           #:gtype-name
           #:gtype-id
           #:g-type-children
           #:g-type-parent
           #:g-type-designator
           #:g-type-fundamental
           #:g-type-depth
           #:g-type-next-base
           #:g-type-is-a
           #:g-type-interfaces
           #:g-type-interface-prerequisites
           #:g-strv-get-type
           #:g-closure-get-type
           #:g-class-property-definition
           #:g-class-property-definition-name
           #:g-class-property-definition-type
           #:g-class-property-definition-readable
           #:g-class-property-definition-writable
           #:g-class-property-definition-constructor
           #:g-class-property-definition-constructor-only
           #:g-class-property-definition-owner-type
           #:parse-g-param-spec
           #:class-properties
           #:interface-properties
           #:enum-item
           #:enum-item-name
           #:enum-item-value
           #:enum-item-nick
           #:get-enum-items
           #:flags-item
           #:flags-item-name
           #:flags-item-value
           #:flags-item-nick
           #:get-flags-items
           #:signal-connect
           #:signal-info
           #:signal-info-id
           #:signal-info-name
           #:signal-info-owner-type
           #:signal-info-flags
           #:signal-info-return-type
           #:signal-info-param-types
           #:signal-info-detail
           #:query-signal-info
           #:type-signals
           #:parse-signal-name
           #:class-property-info
           #:+g-type-invalid+
           #:+g-type-void+
           #:+g-type-interface+
           #:+g-type-char+
           #:+g-type-uchar+
           #:+g-type-boolean+
           #:+g-type-int+
           #:+g-type-uint+
           #:+g-type-long+
           #:+g-type-ulong+
           #:+g-type-int64+
           #:+g-type-uint64+
           #:+g-type-enum+
           #:+g-type-flags+
           #:+g-type-float+
           #:+g-type-double+
           #:+g-type-string+
           #:+g-type-pointer+
           #:+g-type-boxed+
           #:+g-type-param+
           #:+g-type-object+
           #:g-object
           #:pointer
           #:g-type-from-object
           #:g-signal-connect
           #:define-g-object-class
           #:g-initially-unowned
           #:define-g-enum
           #:*lisp-name-package*
           #:define-g-flags
           #:fixed-array
           #:g-boxed-inline
           #:g-boxed-ptr
           #:define-g-interface
           #:release
           #:using
           #:using*
           #:g-boxed-ref
           #:allocate-stable-pointer
           #:free-stable-pointer
           #:get-stable-pointer-value
           #:with-stable-pointer
           #:release*
           #:disown-boxed-ref
           #:g-type-interface
           #:g-value
           #:register-object-type-implementation
           #:ensure-g-type
           #:define-vtable
           #:g-type
           #:set-g-value
           #:parse-g-value
           #:emit-signal
           #:g-value-unset
           #:g-value-zero
           #:g-value-init
           #:g-type-class-ref
           #:g-object-class
           #:gobject-class
           #:g-param-spec
           #:type-instance
           #:g-type-class-unref
           #:registered-object-type-by-name
           #:g-type-children
           #:g-signal-lookup
           #:g-type-parent
           #:connect-signal
           #:boxed-c-structure-name
           #:g-type-designator
           #:g-type-fundamental
           #:g-type-depth
           #:g-type-next-base
           #:g-type-is-a
           #:g-type-interfaces
           #:g-type-interface-prerequisites
           #:g-type-name
           #:g-type
           #:g-type-children
           #:g-type-parent
           #:g-type-designator
           #:g-type-fundamental
           #:g-type-depth
           #:g-type-next-base
           #:g-type-is-a
           #:g-type-interfaces
           #:g-type-interface-prerequisites
           #:g-strv-get-type
           #:g-closure-get-type
           #:g-class-property-definition
           #:g-class-property-definition-name
           #:g-class-property-definition-type
           #:g-class-property-definition-readable
           #:g-class-property-definition-writable
           #:g-class-property-definition-constructor
           #:g-class-property-definition-constructor-only
           #:g-class-property-definition-owner-type
           #:parse-g-param-spec
           #:class-properties
           #:interface-properties
           #:enum-item
           #:enum-item-name
           #:enum-item-value
           #:enum-item-nick
           #:get-enum-items
           #:flags-item
           #:flags-item-name
           #:flags-item-value
           #:flags-item-nick
           #:get-flags-items
           #:stable-pointer-value
           #:g-value-type
           #:g-object-call-constructor
           #:g-object-call-get-property
           #:g-object-call-set-property
           #:register-enum-type
           #:register-flags-type
           #:register-object-type
           #:generate-types-hierarchy-to-file
           #:get-g-flags-definition
           #:get-g-enum-definition
           #:get-g-interface-definition
           #:get-g-class-definition
           #:*strip-prefix*
           #:*lisp-name-exceptions*
           #:*additional-properties*
           #:g-type=
           #:g-type/=
           #:define-g-boxed-cstruct
           #:define-g-boxed-opaque
           #:g-boxed-opaque
           #:g-boxed-opaque-pointer
           #:define-g-boxed-variant-cstruct
           #:g-boxed-foreign
           #:boxed-related-symbols
           #:define-boxed-opaque-accessor
           #:glib-defcallback
           #:create-signal-handler-closure
           #:save-handler-to-object
           #:retrieve-handler-from-object
           #:delete-handler-from-object
           #:disconnect-signal
           #:define-cb-methods
           #:create-fn-ref
           #:copy-boxed-slots-to-foreign
           #:with-foreign-boxed-array
           #:get-g-type-definition)
  (:documentation
   "CL-GTK2-GOBJECT is a binding to GObject type system."))

(in-package :gobject)

(defvar *gobject-debug* nil)

(defvar *debug-gc* nil)
(defvar *debug-subclass* nil)

(defvar *debug-stream* t)

(defmacro log-for (categories control-string &rest args)
  (let ((vars (iter (for sym in (if (listp categories) categories (list categories)))
                    (collect (intern (format nil "*DEBUG-~A*" (symbol-name sym)) (find-package :gobject))))))
    `(progn
       (when (or ,@vars)
         (format *debug-stream* ,control-string ,@args))
       nil)))
