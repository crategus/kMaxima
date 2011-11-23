
(in-package :glib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *initializers-table* (make-hash-table :test 'equalp))
  (defvar *initializers* nil)
  (defun register-initializer (key fn)
    (unless (gethash key *initializers-table*)
      (setf (gethash key *initializers-table*) t
            *initializers* (nconc *initializers* (list fn)))))
  (defvar *finalizers-table* (make-hash-table :test 'equalp))
  (defvar *finalizers* nil)
  (defun register-finalizer (key fn)
    (unless (gethash key *finalizers-table*)
      (setf (gethash key *finalizers-table*) t
            *finalizers* (nconc *finalizers* (list fn))))))

(defun run-initializers ()
  (iter (for fn in *initializers*)
        (funcall fn)))

(defun run-finalizers ()
  (iter (for fn in *finalizers*)
        (funcall fn)))

#+sbcl
(pushnew 'run-initializers sb-ext:*init-hooks*)
#+openmcl
(pushnew 'run-initializers ccl:*restore-lisp-functions*)

#+sbcl
(pushnew 'run-finalizers sb-ext:*save-hooks*)
#+openmcl
(pushnew 'run-finalizers ccl:*save-exit-functions*)

(defmacro at-init ((&rest keys) &body body)
  "
@arg[keys]{list of expression}
@arg[body]{the code}
Runs the code normally but also schedules the code to be run at image load time.
It is used to reinitialize the libraries when the dumped image is loaded. (Works only on SBCL for now).

At-init form may be called multiple times. The same code from should not be run multiple times at initialization time (in best case, this will only slow down initialization, in worst case, the code may crash). To ensure this, every @code{at-init} expression is added to hash-table with the @code{body} and @code{keys} as a composite key. This ensures that the same code is only executed once (once on the same set of parameters).

Example:
@begin{pre}
\(defmethod initialize-instance :after ((class gobject-class) &key &allow-other-keys)
  (register-object-type (gobject-class-g-type-name class) (class-name class))
  (at-init (class) (initialize-gobject-class-g-type class)))
@end{pre}

In this example, for every @code{class}, @code{(initialize-gobject-class-g-type class)} will be called only once.
"
  `(progn (register-initializer (list ,@keys ',body) (lambda () ,@body))
          ,@body))

(defmacro at-finalize ((&rest keys) &body body)
  `(register-finalizer (list ,@keys ',body) (lambda () ,@body)))

(at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library glib
      ((:and :unix (:not :darwin)) (:or "libglib-2.0.so.0" "libglib-2.0.so"))
      (:darwin (:or "libglib-2.0.0.dylib" "libglib-2.0.dylib"))
      (:windows "libglib-2.0-0.dll")
      (t (:default "libglib-2.0"))))
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library gthread
      ((:and :unix (:not :darwin))  (:or "libgthread-2.0.so.0"  "libgthread-2.0.so"))
      (:darwin (:or "libgthread-2.0.0.dylib"  "libgthread-2.0.dylib"))
      (:windows "libgthread-2.0-0.dll")
      (t "libgthread-2.0")))

  (use-foreign-library glib)
  (use-foreign-library gthread))

(defmacro push-library-version-features (library-name major-version-var minor-version-var &body versions)
  `(eval-when (:load-toplevel :execute)
     ,@(iter (for (major minor) on versions by #'cddr)
             (collect
                 `(when (or (and (= ,major-version-var ,major) (>= ,minor-version-var ,minor))
                            (> ,major-version-var ,major))
                    (pushnew ,(intern (format nil "~A-~A.~A" (string library-name) major minor) (find-package :keyword)) *features*))))))

(define-condition foreign-library-minimum-version-mismatch (error)
  ((library :initarg :library :reader .library)
   (minimum-version :initarg :minimum-version :reader .minimum-version)
   (actual-version :initarg :actual-version :reader .actual-version))
  (:report (lambda (c s)
             (format s "Library ~A has too old version: it is ~A but required to be at least ~A"
                     (.library c)
                     (.actual-version c)
                     (.minimum-version c)))))

(defun require-library-version (library min-major-version min-minor-version major-version minor-version)
  (unless (or (> major-version min-major-version)
              (and (= major-version min-major-version)
                   (>= minor-version min-minor-version)))
    (restart-case
        (error 'foreign-library-minimum-version-mismatch
               :library library
               :minimum-version (format nil "~A.~A" min-major-version min-minor-version)
               :actual-version (format nil "~A.~A" major-version minor-version))
      (ignore () :report "Ignore version requirement" nil))))

;;
;; Glib Fundamentals
;;

;;
;; Fundamentals - Basic types
;;


;; TODO: not sure about these: for amd64 they are ok
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cond
    ((cffi-features:cffi-feature-p :x86-64) (defctype gsize :uint64))
    ((cffi-features:cffi-feature-p :x86) (defctype gsize :ulong))
    ((cffi-features:cffi-feature-p :ppc32) (defctype gsize :uint32))
    ((cffi-features:cffi-feature-p :ppc64) (defctype gsize :uint64))
    (t (error "Can not define 'gsize', unknown CPU architecture (known are x86 and x86-64)"))))

(defctype gssize :long)

(defctype goffset :uint64)


;;
;; Fundamentals - Version information
;;

(defcvar (*glib-major-version* "glib_major_version" :read-only t :library glib) :uint)
(defcvar (*glib-minor-version* "glib_minor_version" :read-only t :library glib) :uint)
(defcvar (*glib-micro-version* "glib_micro_version" :read-only t :library glib) :uint)
(defcvar (*glib-binary-age* "glib_binary_age" :read-only t :library glib) :uint)
(defcvar (*glib-interface-age* "glib_interface_age" :read-only t :library glib) :uint)

(push-library-version-features glib *glib-major-version* *glib-micro-version*
  2 2
  2 4
  2 6
  2 8
  2 10
  2 12
  2 14
  2 16
  2 18
  2 20
  2 22)

(require-library-version "Glib" 2 20 *glib-major-version* *glib-minor-version*)

;;
;; Omitted:
;; Limits of Basic Types, Standard Macros, Type Conversion Macros, Byte Order Macros, 
;; Numerical Definitions, Miscellaneous Macros, Atomic operations
;;

;; Core Application Support - The Main Event Loop

(defcstruct g-main-loop)

(defcstruct g-main-context)

(defcstruct g-source)

(defcstruct g-source-funcs
  (prepare :pointer)
  (check :pointer)
  (dispatch :pointer)
  (finalize :pointer)
  (closure-callback :pointer)
  (closure-marshal :pointer))

(defcstruct g-source-callback-funcs
  (ref :pointer)
  (unref :pointer)
  (get :pointer))

(defcstruct g-cond)

(defcstruct g-mutex)

(defcstruct g-poll-fd
  (fd :int) ;; TODO: #if defined (G_OS_WIN32) && GLIB_SIZEOF_VOID_P == 8
  (events :ushort)
  (revent :ushort))

(defcstruct g-time-val
  (seconds :long)
  (microseconds :long))

(defcstruct g-thread)



;(defctype g-pid :int) ;;TODO: might work on amd64 linux, but on others

;; Omitted GPid, g_child_add_watch, g_child_add_watch_full


;;
;; Core Application Support - Threads
;;



;omitted: struct GThreadFunctions








;omitted: g_thread_create, g_thread_create_full, g_thread_yield, g_thread_exit, g_thread_foreach


;;;; TODO: Commented g_mutex_*, g_cond* because they are not functions, but called through dispatch table

;; (defcfun (g-mutex-new "g_mutex_new" :library glib) (:pointer g-mutex))

;; (defcfun (g-mutex-lock "g_mutex_lock" :library glib) :void
;;   (mutex (:pointer g-mutex)))

;; (defcfun (g-mutex-try-lock "g_mutex_trylock" :library glib) :boolean
;;   (mutex (:pointer g-mutex)))

;; (defcfun (g-mutex-free "g_mutex_free" :library glib) :void
;;   (mutex (:pointer g-mutex)))

;omitted: GStaticMutex, GStaticRWLock stuff

;; (defcfun (g-cond-new "g_cond_new" :library glib) (:pointer g-cond))

;; (defcfun (g-cond-signal "g_cond_signal" :library glib) :void
;;   (cond (:pointer g-cond)))

;; (defcfun (g-cond-broadcast "g_cond_broadcast" :library glib) :void
;;   (cond (:pointer g-cond)))

;; (defcfun (g-cond-wait "g_cond_wait" :library glib) :void
;;   (cond (:pointer g-cond))
;;   (mutex (:pointer g-mutex)))

;; (defcfun (g-cond-timed-wait "g_cond_timed_wait" :library glib) :boolean
;;   (cond (:pointer g-cond))
;;   (mutex (:pointer g-mutex))
;;   (abs-time (:pointer g-time-val)))

;; (defcfun (g-cond-free "g_cond_free" :library glib) :void
;;   (cond (:pointer g-cond)))

;omitted: GPrivate, GOnce stuff

;omitted: Thread pools, Asynchronous queues, Dynamic Loading of Modules,
; Memory Allocation, IO Channels, Error Reporting, Message Output and Debugging  Functions, Message Logging

(defcfun g-free :void
  "@arg[ptr]{pointer previously obtained with @fun{g-malloc} or with g_malloc C function}
Frees the pointer by calling g_free on it."
  (ptr :pointer))

(defcfun (g-malloc "g_malloc0") :pointer
  "@arg[n-bytes]{an integer}
@return{pointer to beginning of allocated memory}
Allocates the specified number of bytes in memory. Calls g_malloc.
@see{g-free}"
  (n-bytes gsize))

(defcfun g-strdup :pointer
  "@arg[str]{a @class{string}}
@return{foreign pointer to new string}
Allocates a new string that is equal to @code{str}. Use @fun{g-free} to free it."
  (str (:string :free-to-foreign t)))

;omitted all GLib Utilites

(defbitfield g-spawn-flags
  :leave-descriptors-open :do-not-reap-child :search-path :stdout-to-dev-null :stderr-to-dev-null
  :child-inherits-stdin :file-and-argv-zero)

;TODO: omitted Date and Time Functions
