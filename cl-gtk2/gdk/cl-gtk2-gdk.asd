(defsystem :cl-gtk2-gdk
  :name :cl-gtk2-gdk
  :version "0.1.1"
  :author "Kalyanov Dmitry <Kalyanov.Dmitry@gmail.com>"
  :license "LLGPL"
  :serial t
  :components ((:file "gdk.package")
               (:file "gdk.init")
               
               (:file "gdk.version")      ; Move to gtk?
               (:file "gdk.threads")      ; Functions for using GDK with threads
               
               (:file "gdk.objects")
               (:file "gdk.functions")
               
               (:file "gdk.event-structures") ; Data structures for events
               (:file "gdk.events")           ; Functions for handling events               
               (:file "gdk.general")          ; Library initialization
               
               (:file "gdk.drawable")         ;  Drawing Primitives
               (:file "gdk.windows")
               
               (:file "gdk.manager")
               (:file "gdk.display")
               (:file "gdk.screen")
               (:file "gdk.region")
               (:file "gdk.gc")

               (:file "gdk.bitmaps")
               (:file "gdk.rgb")
               (:file "gdk.images")
               (:file "gdk.pixbufs")
               (:file "gdk.colors")
               (:file "gdk.visual")
               (:file "gdk.cursor")
               
               (:file "gdk.key-values")
               (:file "gdk.selections")
               (:file "gdk.drag-and-drop")
               (:file "gdk.input-devices")
               (:file "gdk.pango")
               )
  :depends-on (:cl-gtk2-glib :cffi :cl-gtk2-pango))
