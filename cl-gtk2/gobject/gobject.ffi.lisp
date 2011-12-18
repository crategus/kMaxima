(in-package :gobject)

(defcfun g-strv-get-type g-type-designator
  "Returns the type designator (see @class{g-type-designator}) for GStrv type. As a side effect, ensures that the type is registered.")

(at-init nil (g-strv-get-type))

(defcfun g-closure-get-type g-type-designator
  "Returns the type designator (see @class{g-type-designator}) for GClosure type. As a side effect, ensure that the type is registered.")

(at-init nil (g-closure-get-type))













(defcstruct g-object-construct-param
  (:param-spec (:pointer g-param-spec))
  (:value (:pointer g-value)))

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



(defbitfield g-signal-flags
  :run-first :run-last :run-cleanup :no-recurse :detailed :action :no-hooks)

