(in-package :gobject)

(defcfun g-strv-get-type g-type-designator
  "Returns the type designator (see @class{g-type-designator}) for GStrv type.
   As a side effect, ensures that the type is registered.")

(at-init nil (g-strv-get-type))

(defcfun g-closure-get-type g-type-designator
  "Returns the type designator (see @class{g-type-designator}) for GClosure
   type. As a side effect, ensure that the type is registered.")

(at-init nil (g-closure-get-type))

(defcstruct g-object-construct-param
  (:param-spec (:pointer g-param-spec))
  (:value (:pointer g-value)))
