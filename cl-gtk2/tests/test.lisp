;;;; Created on 2011-12-27 15:10:36

(asdf:operate 'asdf:load-op :cl-gtk2-gtk)

(defun example ()
  (gtk:within-main-loop
    (let (;; Create a new window.
          (window (make-instance 'gtk:gtk-window
                                 :title "Hello World in GTK"
                                 :type :toplevel
                                 :window-position :center
                                 :default-width  400
                                 :default-height 300
                                 :border-width    10))
          ;; Create a new button with a text.
          (button (make-instance 'gtk:button :label "Hello World")))
      (g:signal-connect window "delete-event"
                        (lambda (window event)
                          (declare (ignore window event))
                          (format t "Delete Event Occured.~%")
                          t))
      (g:signal-connect window "destroy-event"
                        (lambda (window event)
                          (declare (ignore window event))
                          (gtk:main-quit)))
      (g:signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (format t "Hello world.~%")
                          (gtk:widget-destroy window)))
      (gtk:container-add window button)
      (gtk:widget-show button)
      (gtk:widget-show window)
      
      (g:emit-signal window "delete-event" window window)
      
      )))
