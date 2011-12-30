;;;; Created on 2011-12-28 21:05:54

(asdf:operate 'asdf:load-op :cl-gtk2-gtk)

(defpackage :example-g-value
  (:use :common-lisp :gobject :cffi)
  (:export #:example-g-value))

(in-package :example-g-value)

;; A transformation from an integer to a string
(defcallback int2string :void ((src-value (:pointer g-value))
                               (dest-value (:pointer g-value)))
  (if (eql (g-value-get-int src-value) 42)
      (g-value-set-string dest-value "An important number")
      (g-value-set-string dest-value "What is that?")))

(defun example-g-value ()
  ;; Declare two variables of type g-value.
  (with-foreign-objects ((value1 'g-value) (value2 'g-value))
    
    ;; Initialization, setting and reading a value of type g-value
    (g-value-init value1 +g-type-string+)
    (g-value-set-string value1 "string")
    (format t "value1 = ~A~%" (g-value-get-string value1))
    (format t "type   = ~A~%" (g-value-type value1))
    (format t "name   = ~A~%~%" (g-value-type-name value1))
    
    ;; The same in one step with the Lisp extension set-g-value
    (set-g-value value2 "a second string" +g-type-string+ :zero-g-value t)
    (format t "value2 = ~A~%" (parse-g-value value2))
    (format t "type   = ~A~%" (g-value-type value2))
    (format t "name   = ~A~%~%" (g-value-type-name value2))
    
    ;; Reuse value1 for an integer value.
    (g-value-unset value1)
    (g-value-init value1 +g-type-int+)
    (g-value-set-int value1 42)
    (format t "value1 = ~A~%" (parse-g-value value1))
    (format t "type   = ~A~%" (g-value-type value1))
    (format t "name   = ~A~%~%" (g-value-type-name value1))
    
    ;; The types integer and string are transformable.
    (assert (g-value-type-transformable +g-type-int+ +g-type-string+))
    
    ;; Transform value1 of type integer into value2 which is a string
    (g-value-transform value1 value2)
    (format t "value1 = ~A~%" (parse-g-value value1))
    (format t "value2 = ~A~%~%" (parse-g-value value2))
    
    ;; Some test functions.
    (assert (g-value-holds value1 +g-type-int+))
    (format t "value-holds is ~A~%" (g-value-holds value1 +g-type-int+))
    (format t "is-value is ~A~%~%" (g-type-is-value +g-type-int+))
                             
    ;; Reuse value2 again for a string.
    (g-value-unset value2)
    (g-value-init value2 +g-type-string+)
    (g-value-set-string value2 "string")
    (format t "value2 = ~A~%" (parse-g-value value2))
    
    ;; Register the transformation int2string
    (g-value-register-transform-func +g-type-int+
                                     +g-type-string+
                                     (callback int2string))
    ;; Try the transformation
    (g-value-transform value1 value2)
    (format t "value2 = ~A~%~%" (parse-g-value value2))))
