(in-package :gobject)

(defcfun g-strv-get-type g-type-designator
  "Returns the type designator (see @class{g-type-designator}) for GStrv type. As a side effect, ensures that the type is registered.")

(at-init nil (g-strv-get-type))

(defcfun g-closure-get-type g-type-designator
  "Returns the type designator (see @class{g-type-designator}) for GClosure type. As a side effect, ensure that the type is registered.")

(at-init nil (g-closure-get-type))


(defcstruct %g-object
  (:type-instance g-type-instance)
  (:ref-count :uint)
  (:data :pointer))

(defctype %g-initially-unowned %g-object)

(defcstruct g-object-class
  (:type-class g-type-class)
  (:construct-properties :pointer)
  (:constructor :pointer)
  (:set-property :pointer)
  (:get-property :pointer)
  (:dispose :pointer)
  (:finalize :pointer)
  (:dispatch-properties-changed :pointer)
  (:notify :pointer)
  (:constructed :pointer)
  (:pdummy :pointer :count 7))

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

(defcstruct g-object-construct-param
  (:param-spec (:pointer g-param-spec))
  (:value (:pointer g-value)))

(defcstruct g-parameter
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:value g-value))

(defcstruct g-enum-value
  (:value :int)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))

(defcstruct g-enum-class
  (:type-class g-type-class)
  (:minimum :int)
  (:maximum :int)
  (:n-values :uint)
  (:values (:pointer g-enum-value)))

(defcstruct g-flags-value
  (:value :uint)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))

(defcstruct g-flags-class
  (:type-class g-type-class)
  (:mask :uint)
  (:n-values :uint)
  (:values (:pointer g-flags-value)))

(defcstruct g-param-spec-boolean
  (:parent-instance g-param-spec)
  (:default-value :boolean))

(defcstruct g-param-spec-char
  (:parent-instance g-param-spec)
  (:minimum :int8)
  (:maximum :int8)
  (:default-value :int8))

(defcstruct g-param-spec-uchar
  (:parent-instance g-param-spec)
  (:minimum :uint8)
  (:maximum :uint8)
  (:default-value :uint8))

(defcstruct g-param-spec-int
  (:parent-instance g-param-spec)
  (:minimum :int)
  (:maximum :int)
  (:default-value :int))

(defcstruct g-param-spec-uint
  (:parent-instance g-param-spec)
  (:minimum :uint)
  (:maximum :uint)
  (:default-value :uint))

(defcstruct g-param-spec-long
  (:parent-instance g-param-spec)
  (:minimum :long)
  (:maximum :long)
  (:default-value :ulong))

(defcstruct g-param-spec-ulong
  (:parent-instance g-param-spec)
  (:minimum :ulong)
  (:maximum :ulong)
  (:default-value :ulong))

(defcstruct g-param-spec-int64
  (:parent-instance g-param-spec)
  (:minimum :uint64)
  (:maximum :uint64)
  (:default-value :uint64))

(defcstruct g-param-spec-uint64
  (:parent-instance g-param-spec)
  (:minimum :uint64)
  (:maximum :uint64)
  (:default-value :uint64))

(defcstruct g-param-spec-float
  (:parent-instance g-param-spec)
  (:minimum :float)
  (:maximum :float)
  (:default-value :float)
  (:epsilon :float))

(defcstruct g-param-spec-double
  (:parent-instance g-param-spec)
  (:minimum :double)
  (:maximum :double)
  (:default-value :double)
  (:epsilon :double))

(defcstruct g-param-spec-enum
  (:parent-instance g-param-spec)
  (:enum-class (:pointer g-enum-class))
  (:default-value :int))

(defcstruct g-param-spec-flags
  (:parent-instance g-param-spec)
  (:flags-class (:pointer g-flags-class))
  (:default-value :uint))

(defcstruct g-param-spec-string
  (:parent-instance g-param-spec)
  (:default-value (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-first (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-nth (:string :free-to-foreign nil :free-from-foreign nil))
  (:substitutor :char)
  (:flags-for-null :uint))

(defcstruct g-param-spec-param
  (:parent-instance g-param-spec))

(defcstruct g-param-spec-boxed
  (:parent-instance g-param-spec))

(defcstruct g-param-spec-pointer
  (:parent-instance g-param-spec))

(defcstruct g-param-spec-object
  (:parent-instance g-param-spec))

(defcstruct g-param-spec-value-array
  (:parent-instance g-param-spec)
  (:element-spec (:pointer g-param-spec))
  (:fixed-n-elements :uint))

(defcstruct g-param-spec-g-type
  (:parent-instance g-param-spec)
  (:types-root g-type-designator))

(defcstruct g-param-spec-class
  (:type-class g-type-class)
  (:value-type g-type-designator)
  (:finalize :pointer)
  (:value-set-default :pointer)
  (:value-validate :pointer)
  (:values-cmp :pointer))

(defcstruct g-closure
  (:private-data :uint32)
  (:marshal :pointer)
  (:data :pointer)
  (:notifiers :pointer))



(defcfun g-enum-register-static g-type-designator
  (name :string)
  (static-values (:pointer g-enum-value)))

(defcfun g-flags-register-static g-type-designator
  (name :string)
  (static-values (:pointer g-flags-value)))

(defcfun g-param-spec-boolean (:pointer g-param-spec-boolean)
  (name :string)
  (nick :string)
  (blurb :string)
  (default-value :boolean)
  (flags g-param-flags))

(defcfun g-value-set-boolean :void
  (g-value (:pointer g-value))
  (new-value :boolean))

(defcfun g-value-get-boolean :boolean
  (g-value (:pointer g-value)))

(defcfun g-param-spec-char (:pointer g-param-spec-char)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :int8)
  (maximum :int8)
  (default-value :int8)
  (flags g-param-flags))

(defcfun g-value-set-char :void
  (g-value (:pointer g-value))
  (new-value :char))

(defcfun g-value-get-char :char
  (g-value (:pointer g-value)))

(defcfun g-param-spec-uchar (:pointer g-param-spec-uchar)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :uint8)
  (maximum :uint8)
  (default-value :uint8)
  (flags g-param-flags))

(defcfun g-value-set-uchar :void
  (g-value (:pointer g-value))
  (new-value :uchar))

(defcfun g-value-get-uchar :uchar
  (g-value (:pointer g-value)))

(defcfun g-param-spec-int (:pointer g-param-spec-int)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :int)
  (maximum :int)
  (default-value :int)
  (flags g-param-flags))

(defcfun g-value-set-int :void
  (g-value (:pointer g-value))
  (new-value :int))

(defcfun g-value-get-int :int
  (g-value (:pointer g-value)))

(defcfun g-param-spec-uint (:pointer g-param-spec-uint)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :uint)
  (maximum :uint)
  (default-value :uint)
  (flags g-param-flags))

(defcfun g-value-set-uint :void
  (g-value (:pointer g-value))
  (new-value :uint))

(defcfun g-value-get-uint :uint
  (g-value (:pointer g-value)))

(defcfun g-param-spec-long (:pointer g-param-spec-long)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :long)
  (maximum :long)
  (default-value :long)
  (flags g-param-flags))

(defcfun g-value-set-long :void
  (g-value (:pointer g-value))
  (new-value :long))

(defcfun g-value-get-long :long
  (g-value (:pointer g-value)))

(defcfun g-param-spec-ulong (:pointer g-param-spec-ulong)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :ulong)
  (maximum :ulong)
  (default-value :ulong)
  (flags g-param-flags))

(defcfun g-value-set-ulong :void
  (g-value (:pointer g-value))
  (new-value :ulong))

(defcfun g-value-get-ulong :ulong
  (g-value (:pointer g-value)))

(defcfun g-param-spec-int64 (:pointer g-param-spec-int64)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :int64)
  (maximum :int64)
  (default-value :int64)
  (flags g-param-flags))

(defcfun g-value-set-int64 :void
  (g-value (:pointer g-value))
  (new-value :int64))

(defcfun g-value-get-int64 :int64
  (g-value (:pointer g-value)))

(defcfun g-param-spec-uint64 (:pointer g-param-spec-uint64)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :uint64)
  (maximum :uint64)
  (default-value :uint64)
  (flags g-param-flags))

(defcfun g-value-set-uint64 :void
  (g-value (:pointer g-value))
  (new-value :uint64))

(defcfun g-value-get-uint64 :uint64
  (g-value (:pointer g-value)))

(defcfun g-param-spec-float (:pointer g-param-spec-float)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :float)
  (maximum :float)
  (default-value :float)
  (flags g-param-flags))

(defcfun g-value-set-float :void
  (g-value (:pointer g-value))
  (new-value :float))

(defcfun g-value-get-float :float
  (g-value (:pointer g-value)))

(defcfun g-param-spec-double (:pointer g-param-spec-double)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :double)
  (maximum :double)
  (default-value :double)
  (flags g-param-flags))

(defcfun g-value-set-double :void
  (g-value (:pointer g-value))
  (new-value :double))

(defcfun g-value-get-double :double
  (g-value (:pointer g-value)))

(defcfun g-param-spec-enum (:pointer g-param-spec-enum)
  (name :string)
  (nick :string)
  (blurb :string)
  (enum-type g-type-designator)
  (default-value :int)
  (flags g-param-flags))

(defcfun g-value-set-enum :void
  (g-value (:pointer g-value))
  (new-value :int))

(defcfun g-value-get-enum :int
  (g-value (:pointer g-value)))

(defcfun g-param-spec-flags (:pointer g-param-spec-flags)
  (name :string)
  (nick :string)
  (blurb :string)
  (flags-type g-type-designator)
  (default-value :int)
  (flags g-param-flags))

(defcfun g-value-set-flags :void
  (g-value (:pointer g-value))
  (new-value :int))

(defcfun g-value-get-flags :int
  (g-value (:pointer g-value)))

(defcfun g-param-spec-string (:pointer g-param-spec-string)
  (name :string)
  (nick :string)
  (blurb :string)
  (default-value :string)
  (flags g-param-flags))

(defcfun g-value-set-string :void
  (g-value (:pointer g-value))
  (new-value :string))

(defcfun g-value-get-string (:string :free-from-foreign nil)
  (g-value (:pointer g-value)))

(defcfun g-param-spec-param (:pointer g-param-spec-param)
  (name :string)
  (nick :string)
  (blurb :string)
  (param-type g-type-designator)
  (flags g-param-flags))

(defcfun g-value-set-param :void
  (g-value (:pointer g-value))
  (new-value (:pointer g-param-spec)))

(defcfun g-value-get-param (:pointer g-param-spec)
  (g-value (:pointer g-value)))

(defcfun g-param-spec-boxed (:pointer g-param-spec-boxed)
  (name :string)
  (nick :string)
  (blurb :string)
  (boxed-type g-type-designator)
  (flags g-param-flags))

(defcfun g-value-set-boxed :void
  (g-value (:pointer g-value))
  (new-value :pointer))

(defcfun g-value-take-boxed :void
  (g-value (:pointer g-value))
  (new-value :pointer))

(defcfun g-value-get-boxed :pointer
  (g-value (:pointer g-value)))

(defcfun g-param-spec-pointer (:pointer g-param-spec-pointer)
  (name :string)
  (nick :string)
  (blurb :string)
  (flags g-param-flags))

(defcfun g-value-set-pointer :void
  (g-value (:pointer g-value))
  (new-value :pointer))

(defcfun g-value-get-pointer :pointer
  (g-value (:pointer g-value)))

(defcfun g-param-spec-object (:pointer g-param-spec-object)
  (name :string)
  (nick :string)
  (blurb :string)
  (object-type g-type-designator)
  (flags g-param-flags))

(defcfun g-value-set-object :void
  (g-value (:pointer g-value))
  (new-value :pointer))

(defcfun g-value-get-object :pointer
  (g-value (:pointer g-value)))

(defcfun g-param-spec-value-array (:pointer g-param-spec-value-array)
  (name :string)
  (nick :string)
  (blurb :string)
  (element-spec (:pointer g-param-spec))
  (flags g-param-flags))

(defcfun (g-param-spec-g-type "g_param_spec_gtype") (:pointer g-param-spec-g-type)
  (name :string)
  (nick :string)
  (blurb :string)
  (types-root g-type-designator)
  (flags g-param-flags))

(defcfun (g-value-set-g-type "g_value_set_gtype") :void
  (g-value (:pointer g-value))
  (new-value g-type-designator))

(defcfun (g-value-get-g-type "g_value_get_gtype") g-type-designator
  (g-value (:pointer g-value)))

(defcfun g-param-spec-ref-sink (:pointer g-param-spec)
  (param-spec (:pointer g-param-spec)))

(defcfun g-param-spec-unref :void
  (param-spec (:pointer g-param-spec)))

(defcfun g-param-value-set-default :void
  (param-spec (:pointer g-param-spec))
  (value (:pointer g-value)))

(defcfun g-param-value-defaults :boolean
  (param-spec (:pointer g-param-spec))
  (value (:pointer g-value)))

(defcfun g-param-value-validate :boolean
  (param-spec (:pointer g-param-spec))
  (value (:pointer g-value)))

(defcfun g-param-spec-get-name :string
  (param-spec (:pointer g-param-spec)))

(defcfun g-param-spec-get-nick :string
  (param-spec (:pointer g-param-spec)))

(defcfun g-param-spec-get-blurb :string
  (param-spec (:pointer g-param-spec)))

(defcfun g-value-init (:pointer g-value)
  "Initializes the GValue @code{value} with the default value of @code{type}

@arg[value]{a C pointer to the GValue structure}
@arg[type]{an integer specifying the GType}"
  (value (:pointer g-value))
  (type g-type-designator))

(defcfun g-value-copy :void
  (src-value (:pointer g-value))
  (dst-value (:pointer g-value)))

(defcfun g-value-reset (:pointer g-value)
  (value (:pointer g-value)))

(defcfun g-value-unset (:pointer g-value)
  "Clears the current value in @code{value} and \"unsets\" the type, releasing all resources associated with this GValue. An unset value is the same as an unitialized GValue.

@arg[value]{a C pointer to the GValue structure}"
  (value (:pointer g-value)))

(defcfun g-value-set-instance :void
  (value (:pointer g-value))
  (instance :pointer))

(defcfun g-strdup-value-contents :string
  (value (:pointer g-value)))




(defcfun g-object-add-toggle-ref :void
  (object :pointer)
  (notifty :pointer)
  (data :pointer))

(defcfun g-object-remove-toggle-ref :void
  (object :pointer)
  (notifty :pointer)
  (data :pointer))

(defcfun g-object-notify :void
  (object :pointer)
  (property-name :string))

(defcfun g-object-freeze-notify :void
  (object :pointer))

(defcfun g-object-thaw-notify :void
  (object :pointer))

(defcfun g-object-get-data :pointer
  (object :pointer)
  (key :string))

(defcfun g-object-set-data :void
  (object :pointer)
  (key :string)
  (new-value :pointer))

(defcfun g-object-set-data-full :void
  (object :pointer)
  (key :string)
  (data :pointer)
  (destory :pointer))

(defcfun g-object-steal-data :pointer
  (object :pointer)
  (key :string))

(defcfun g-object-set-property :void
  (object :pointer)
  (property-name :string)
  (value (:pointer g-value)))

(defcfun g-object-get-property :void
  (object :pointer)
  (property-name :string)
  (value (:pointer g-value)))

(defcfun g-signal-connect-closure :ulong
  (instance :pointer)
  (detailed-signal :string)
  (closure (:pointer g-closure))
  (after :boolean))

(defcfun g-signal-emitv :void
  (instance-and-params (:pointer g-value))
  (signal-id :uint)
  (detail g-quark)
  (return-value (:pointer g-value)))

(defcfun g-signal-lookup :uint
  (name :string)
  (type g-type-designator))

(defbitfield g-signal-flags
  :run-first :run-last :run-cleanup :no-recurse :detailed :action :no-hooks)

(defcstruct g-signal-query
  (:signal-id :uint)
  (:signal-name :string)
  (:owner-type g-type-designator)
  (:signal-flags g-signal-flags)
  (:return-type (g-type-designator :mangled-p t))
  (:n-params :uint)
  (:param-types (:pointer (g-type-designator :mangled-p t))))

(defcfun g-signal-query :void
  (signal-id :uint)
  (query (:pointer g-signal-query)))

(defcfun g-signal-list-ids (:pointer :uint)
  (type g-type-designator)
  (n-ids (:pointer :uint)))

(defcstruct g-object-struct
  (:type-instance g-type-instance)
  (:ref-count :uint)
  (:qdata :pointer))
