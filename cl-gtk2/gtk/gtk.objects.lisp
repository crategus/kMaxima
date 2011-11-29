(in-package :gtk)

(define-g-boxed-cstruct border "GtkBorder"
  (left :int :initform 0)
  (right :int :initform 0)
  (top :int :initform 0)
  (bottom :int :initform 0))

(at-init () (foreign-funcall "gtk_border_get_type" :int))
(at-init () (foreign-funcall "gtk_ui_manager_get_type" :int))

(export (boxed-related-symbols 'border))

(define-foreign-type pointer-as-integer-foreign-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser pointer-as-integer))

(defmethod translate-to-foreign (value (type pointer-as-integer-foreign-type))
  (make-pointer value))

(defmethod translate-from-foreign (value (type pointer-as-integer-foreign-type))
  (pointer-address value))

(define-g-boxed-cstruct tree-iter "GtkTreeIter"
  (stamp :int :initform 0)
  (user-data pointer-as-integer :initform 0)
  (user-data-2 pointer-as-integer :initform 0)
  (user-data-3 pointer-as-integer :initform 0))

(export 'tree-iter)
(export 'tree-iter-stamp)
(export 'tree-iter-user-data)

(defctype tree-path :pointer)

(defcfun gtk-tree-path-new :pointer)
(defcfun gtk-tree-path-free :void (path :pointer))

(define-g-boxed-opaque tree-path "GtkTreePath"
  :alloc (gtk-tree-path-new))


(defun tree-path-indices (path)
  (tree-path-get-indices path))

(defun (setf tree-path-indices) (new-value path)
  (tree-path-set-indices new-value path))

(defun tree-path-get-indices (path)
  (setf path (pointer path))
  (let ((n (%gtk-tree-path-get-depth path))
        (indices (%gtk-tree-path-get-indices path)))
    (loop
       for i from 0 below n
       collect (mem-aref indices :int i))))

(defun tree-path-set-indices (indices path)
  (setf path (pointer path))
  (loop 
     repeat (%gtk-tree-path-get-depth path)
     do (foreign-funcall "gtk_tree_path_up" :pointer path :boolean))
  (loop
     for index in indices
     do(foreign-funcall "gtk_tree_path_append_index" :pointer path :int index :void)))

(export 'tree-path)
(export 'tree-path-indices)


(at-init () (gobject::type-initializer-call "gtk_tree_row_reference_get_type"))

(define-g-boxed-opaque tree-row-reference "GtkTreeRowReference" :alloc (lambda () (error "")))

(export 'tree-row-reference)

(defcfun (tree-row-reference-new "gtk_tree_row_reference_new") (g-boxed-foreign tree-row-reference :return)
  (model (g-object tree-model))
  (path (g-boxed-foreign tree-path)))

(export 'tree-row-reference)

(define-boxed-opaque-accessor tree-row-reference tree-row-reference-model
  :reader "gtk_tree_row_reference_get_model" :type (g-object tree-model))

(define-boxed-opaque-accessor tree-row-reference tree-row-reference-path
  :reader "gtk_tree_row_reference_get_path" :type (g-boxed-foreign tree-path :return))

(define-boxed-opaque-accessor tree-row-reference tree-row-reference-valid
  :reader "gtk_tree_row_reference_valid" :type :boolean)

(export '(tree-row-reference-model tree-row-reference-path tree-row-reference-valid))

(defcfun (adjustment-clamp-page "gtk_adjustment_clamp_page") :void
  (adjustment (g-object adjustment))
  (lower :double)
  (upper :double))

(export 'adjustment-clamp-page)

(define-g-boxed-cstruct requisition "GtkRequisition"
  (width :int :initform 0)
  (height :int :initform 0))

(export (boxed-related-symbols 'requisition))

(define-g-boxed-cstruct allocation "GtkAllocation"
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))

(export (boxed-related-symbols 'allocation))
