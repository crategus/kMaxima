;;;; Created on 2011-12-22 21:38:08

(defpackage :glib-tests
  (:use :glib :cffi :common-lisp :lisp-unit))

(in-package :glib-tests)

(define-test glib-misc
  (assert-eq :unsigned-long (cffi::canonicalize-foreign-type 'g-size))
  (assert-eq :long (cffi::canonicalize-foreign-type 'g-ssize))
  (assert-eq :UNSIGNED-LONG-LONG (cffi::canonicalize-foreign-type 'g-offset)))

(define-test glib-version
  (assert-equal    2 *glib-major-version*)
  (assert-equal   24 *glib-minor-version*)
  (assert-equal    1 *glib-micro-version*)
  (assert-equal 2401 *glib-binary-age*)
  (assert-equal    1 *glib-interface-age*)
  (assert-false (glib-check-version 2 0 0))
  (assert-equal "GLib version too old (micro mismatch)"
                (glib-check-version 2 28 0)))

(define-test glib-threads
;  (assert-true (g-thread-supported))
;  (assert-true (g-thread-initialized))
  (assert-true (g-thread-self)))

(defvar started nil)

(define-test glib-utils
  (let ((env))
    (if started
        (progn
          (assert-equal "glib-AppName" (glib:g-get-application-name))
          (assert-equal "glib-Prgname" (glib:g-get-prgname)))
        (progn
          (setq started t)
          (assert-equal "<unknown>" (glib:g-get-application-name))
          (glib:g-set-application-name "glib-AppName")
          (assert-equal "glib-AppName" (glib:g-get-application-name))
          (assert-equal "<unknown>" (glib:g-get-prgname))
          (glib:g-set-prgname "glib-Prgname")
          (assert-equal "glib-Prgname" (glib:g-get-prgname))))
    (assert-equal 40 (length (setq env (glib:g-listenv))))
    (assert-equal "ORBIT_SOCKETDIR" (first env))
    (assert-equal "/tmp/orbit-dieter" (glib:g-getenv (first env)))
    (assert-equal "dieter" (glib:g-get-user-name))
    (assert-equal "Dieter Kaiser" (glib:g-get-real-name))
    (assert-equal "/home/dieter/.cache" (glib:g-get-user-cache-dir))
    (assert-equal "/home/dieter/.local/share" (glib:g-get-user-data-dir))
    (assert-equal "/home/dieter/.config" (glib:g-get-user-config-dir))
    (assert-equal "/home/dieter/Desktop"
                  (glib:g-get-user-special-dir :g-user-directory-desktop))
    (assert-equal "/home/dieter/Dokumente"
                  (glib:g-get-user-special-dir :g-user-directory-documents))
    (assert-equal "/home/dieter/Downloads"
                  (glib:g-get-user-special-dir :g-user-directory-download))
    (assert-equal "/home/dieter/Musik"
                  (glib:g-get-user-special-dir :g-user-directory-music))
    (assert-equal "/home/dieter/Bilder"
                  (glib:g-get-user-special-dir :g-user-directory-pictures))
    (assert-equal "/home/dieter/Öffentlich"
                  (glib:g-get-user-special-dir :g-user-directory-public-share))
    (assert-equal "/home/dieter/Vorlagen"
                  (glib:g-get-user-special-dir :g-user-directory-templates))
    (assert-equal "/home/dieter/Videos"
                  (glib:g-get-user-special-dir :g-user-directory-videos))
    (if (not started)
        (progn
          (assert-equal '("/usr/share/gnome" "/usr/local/share/" "/usr/share/")
                        (glib:g-get-system-data-dirs))
          (assert-equal '("/etc/xdg/xdg-gnome" "/etc/xdg")
                        (glib:g-get-system-config-dirs))))
    (assert-equal "dieter" (glib:g-get-host-name))
    (assert-equal "/home/dieter" (glib:g-get-home-dir))
    (assert-equal "/tmp" (glib:g-get-tmp-dir))
    (assert-equal "/home/dieter/Lisp/kMaxima/cl-gtk2/tests"
                  (glib:g-get-current-dir))
    (assert-equal t (glib:g-path-is-absolute "/home/dieter/Lisp"))
    (assert-equal nil (glib:g-path-is-absolute "Lisp"))))
