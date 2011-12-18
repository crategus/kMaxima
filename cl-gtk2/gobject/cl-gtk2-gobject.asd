(defsystem :cl-gtk2-gobject
  :name :cl-gtk2-gobject
  :version "0.1.1"
  :author "Dr. Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components ((:file "gobject.package")
               (:file "gobject.init")
               
               (:file "gobject.type")      ; Type Information
               (:file "gobject.gvalue")    ; Generic Values
               (:file "gobject.paramspec") ; GParamSpec
                                           
               (:file "gobject.base")      ; The Base Object Type
               
               (:file "gobject.ffi")
               (:file "gobject.foreign")
               
               (:file "gobject.meta")
               
               (:file "gobject.param")
               
               (:file "gobject.object.high")
               (:file "gobject.signals")
               (:file "gobject.closures")
               (:file "gobject.boxed")

               (:file "gobject.type-info.object")
               (:file "gobject.type-info.enum")
               (:file "gobject.type-info.signals")
               
               (:file "gobject.stable-pointer")
               (:file "gobject.object.low")
               
               (:file "gobject.generating")
               (:file "gobject.cffi-callbacks")
               (:file "gobject.foreign-gobject-subclassing")
               
               (:file "gobject.object-function")
               )
  :depends-on (:cl-gtk2-glib
               :cffi
               :trivial-garbage
               :iterate
               :bordeaux-threads
               :iterate
               :closer-mop))
