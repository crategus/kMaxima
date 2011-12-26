(defsystem :cl-gtk2-gobject
  :name :cl-gtk2-gobject
  :version "0.1.1"
  :author "Dr. Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components ((:file "gobject.package")
               (:file "gobject.init")
               (:file "gobject.foreign")
               
               (:file "gobject.type-info")   ; Type Information
               (:file "gobject.gvalue")      ; Generic Values
               (:file "gobject.enumeration") ; Enumeration and flag types
               (:file "gobject.boxed")       ; Boxed Types
               (:file "gobject.paramspec")   ; GParamSpec
               (:file "gobject.param")       ; Paramenters and Values
               (:file "gobject.base")        ; The Base Object Type
               (:file "gobject.closures")    ; Closures
               (:file "gobject.signals")     ; Signals
               
               ;; Varargs Value Collection not implemented
               ;; Value arrays not implemented
               ;; GBinding not implementend
               
               (:file "gobject.type-info.object")
               (:file "gobject.type-info.enum")
               (:file "gobject.type-info.signals")
               (:file "gobject.stable-pointer")
               (:file "gobject.generating")
               (:file "gobject.cffi-callbacks")
               (:file "gobject.foreign-gobject-subclassing")
               (:file "gobject.object-function")
               (:file "gobject.ffi")
               )
  :depends-on (:cl-gtk2-glib
               :cffi
               :trivial-garbage
               :iterate
               :bordeaux-threads
               :iterate
               :closer-mop))
