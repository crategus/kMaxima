;;;; 2011-01-18 18:50:09

(defpackage #:kmaxima-asd
  (:use :cl :asdf))

(in-package :kmaxima-asd)

(defsystem kmaxima
  :name "kmaxima"
  :version "0.1"
  :serial t
  :components ((:file "defpackage")
               (:file "src/mmacro")
               (:file "src/mutils")
               (:file "src/parser-def")
               (:file "src/parser")
               (:file "src/nformat")
               (:file "src/grind")
               (:file "src/ldisplay")
               (:file "src/msystem")))

