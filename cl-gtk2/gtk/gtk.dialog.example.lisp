(in-package :gtk-examples)

(defun test-dialog ()
  (let ((window (make-instance 'gtk-window
                               :type
                               :toplevel
                               :title "Testing dialogs"))
        (v-box (make-instance 'v-box)))
    (g-signal-connect window
                      "destroy"
                      (lambda (w) (declare (ignore w)) (leave-gtk-main)))
    (container-add window v-box)
    (let ((button (make-instance 'button :label "Dialog 1")))
      (box-pack-start v-box button)
      (g-signal-connect button
                        "clicked"
                        (lambda (b)
                          (declare (ignore b))
                          (let ((dialog (make-instance 'dialog)))
                            (dialog-add-button dialog "OK" :ok)
                            (dialog-add-button dialog "Yes" :yes)
                            (dialog-add-button dialog "Cancel" :cancel)
                            (setf (dialog-default-response dialog) :cancel)
                            (dialog-set-alternative-button-order
                              dialog 
                              (list :yes :cancel :ok))
                            (format t "Response was: ~S~%" (dialog-run dialog))
                            (object-destroy dialog)))))
    (let ((button (make-instance 'button :label "About")))
      (box-pack-start v-box button)
      (g-signal-connect button
                        "clicked"
                        (lambda (b)
                          (declare (ignore b))
                          (let ((dialog (make-instance 'about-dialog
                                                       :program-name "Dialogs examples"
                                                       :version "0.01"
                                                       :copyright "(c) Kalyanov Dmitry"
                                                       :website "http://common-lisp.net/project/cl-gtk+"
                                                       :website-label "Project web site"
                                                       :license "LLGPL"
                                                       :authors '("Kalyanov Dmitry")
                                                       :documenters '("Kalyanov Dmitry")
                                                       :artists '("None")
                                                       :logo-icon-name "applications-development"
                                                       :wrap-license t)))
                            (format t "Response was: ~S~%" (dialog-run dialog))
                            (object-destroy dialog)))))
    
    (gtk-widget-show window)
    (ensure-gtk-main)))
