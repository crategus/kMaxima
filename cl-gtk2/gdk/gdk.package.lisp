(defpackage :gdk
  (:use :cl :gobject :glib :cffi :pango :iter)
  (:export #:gdk-window-events
           #:gdk-atom-as-string))
