;;;; Created on 2011-11-19 19:01:54

(in-package :gtk)

(define-g-object-class "GtkGrid" grid
                       (:superclass container
                        :export t
                        :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_box_get_type")
                       ((column-homogeneous grid-column-homogeneous 
                                            "homogeneous"
                                            "gboolean" t t)
                        (column-spacing grid-column-spacing
                                        "column-homogeneous"
                                        "gint" t t)
                        (row-homogeneous grid-row-homogeneous
                                         "row-homogeneous"
                                         "gboolean" t t)
                        (row-spacing grid-row-spacing
                                     "row-spacing"
                                     "gint" t t)))

(defcfun gtk-grid-attach :void
  (grid   (g-object grid))
  (child  (g-object widget))
  (left   :int)
  (top    :int)
  (width  :int)
  (height :int))

(defun grid-attach (grid child &key (left 0) (top 0) (width 0) (height 0))
  (getk-grid-attach grid child left top width height))

(export #grid-attach)
  


(defcfun gtk-box-pack-start :void
  (box (g-object box))
  (child (g-object widget))
  (expand :boolean)
  (fill :boolean)
  (padding :uint))

(defun box-pack-start (box child &key (expand t) (fill t) (padding 0))
  (gtk-box-pack-start box child expand fill padding))

(export 'box-pack-start)

(defcfun gtk-box-pack-end :void
  (box (g-object box))
  (child (g-object widget))
  (expand :boolean)
  (fill :boolean)
  (padding :uint))

(defun box-pack-end (box child &key (expand t) (fill t) (padding 0))
  (gtk-box-pack-end box child expand fill padding))

(export 'box-pack-end)

(defcfun (box-reorder-child "gtk_box_reorder_child") :void
  (box g-object)
  (child g-object)
  (position :int))

(export 'box-reorder-child)