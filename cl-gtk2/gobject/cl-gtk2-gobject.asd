(defsystem :cl-gtk2-gobject
  :name :cl-gtk2-gobject
  :version "0.1.1"
  :author "Kalyanov Dmitry <Kalyanov.Dmitry@gmail.com>"
  :license "LLGPL"
  :serial t
  :components ((:file "gobject.package")
               (:file "gobject.init")
               (:file "gobject.type")
               
               (:file "gobject.ffi")
               (:file "gobject.object.high")
               
               (:file "gobject.base")
               (:file "gobject.signals")
               (:file "gobject.closures")
               (:file "gobject.boxed")
               
               (:file "gobject.type-info")
               (:file "gobject.type-info.object")
               (:file "gobject.type-info.enum")
               (:file "gobject.type-info.signals")
               
               (:file "gobject.gvalue")
               (:file "gobject.foreign")
               (:file "gobject.stable-pointer")
               (:file "gobject.object.low")
               
               (:file "gobject.meta")
               (:file "gobject.generating")
               (:file "gobject.object-defs")
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
