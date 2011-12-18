;;;; Created on 2011-11-19 20:54:45

;;; guint               gtk_get_major_version               (void);
;;; guint               gtk_get_minor_version               (void);
;;; guint               gtk_get_micro_version               (void);
;;; guint               gtk_get_binary_age                  (void);
;;; guint               gtk_get_interface_age               (void);
;;; const gchar *       gtk_check_version                   (guint required_major,    

(in-package :gdk)

(defun get-major-version ()
  *gtk-major-version*)

(defun get-minor-version ()
  *gtk-minor-version*)

(defun get-micro-version ()
  *gtk-micro-version*)
  
(defun get-binary-age ()
  *gtk-binary-age*)

(defun get-interface-age ()
  *gtk-interface-age*)

(defcfun (check-version "gtk_check_version") :string
  (required_major :uint)
  (required_minor :uint)
  (required_micro :uint))
