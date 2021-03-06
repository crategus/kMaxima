;;; ----------------------------------------------------------------------------
;;; kmaxima.asd
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;; ----------------------------------------------------------------------------

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
               (:file "src/display")
               (:file "src/msystem")
               (:file "src/float")
               (:file "src/simplify")
               (:file "src/mload")
               (:file "src/ifactor")))

