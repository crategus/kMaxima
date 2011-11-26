;;;; Created on 2011-11-19 20:36:07

;;;
;;; guint               gtk_get_major_version               (void);
;;; guint               gtk_get_minor_version               (void);
;;; guint               gtk_get_micro_version               (void);
;;; guint               gtk_get_binary_age                  (void);
;;; guint               gtk_get_interface_age               (void);
;;; const gchar *       gtk_check_version                   (guint required_major,
;;;                                                         guint required_minor,
;;;                                                         guint required_micro);


(in-package :gtk)

(defun get-major-version ()
  gdk:*gtk-major-version*)

(export 'get-major-version)

