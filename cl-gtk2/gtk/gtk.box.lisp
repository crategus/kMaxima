;;; ----------------------------------------------------------------------------
;;; gtk.box.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2 from
;;; http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 2.2.2 Reference Manual
;;; See http://www.gtk.org.
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
;;;
;;; GtkBox
;;; 
;;; A container box
;;; 	
;;; Synopsis
;;; 
;;; struct      GtkBox;
;;; GtkWidget * gtk_box_new                 (GtkOrientation orientation,
;;;                                          gint spacing);
;;; void        gtk_box_pack_start          (GtkBox *box,
;;;                                          GtkWidget *child,
;;;                                          gboolean expand,
;;;                                          gboolean fill,
;;;                                          guint padding);
;;; void        gtk_box_pack_end            (GtkBox *box,
;;;                                          GtkWidget *child,
;;;                                          gboolean expand,
;;;                                          gboolean fill,
;;;                                          guint padding);
;;; gboolean    gtk_box_get_homogeneous     (GtkBox *box);
;;; void        gtk_box_set_homogeneous     (GtkBox *box,
;;;                                          gboolean homogeneous);
;;; gint        gtk_box_get_spacing         (GtkBox *box);
;;; void        gtk_box_set_spacing         (GtkBox *box,
;;;                                          gint spacing);
;;; void        gtk_box_reorder_child       (GtkBox *box,
;;;                                          GtkWidget *child,
;;;                                          gint position);
;;; void        gtk_box_query_child_packing (GtkBox *box,
;;;                                          GtkWidget *child,
;;;                                          gboolean *expand,
;;;                                          gboolean *fill,
;;;                                          guint *padding,
;;;                                          GtkPackType *pack_type);
;;; void        gtk_box_set_child_packing   (GtkBox *box,
;;;                                          GtkWidget *child,
;;;                                          gboolean expand,
;;;                                          gboolean fill,
;;;                                          guint padding,
;;;                                          GtkPackType pack_type);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBox
;;;                            +----GtkAppChooserWidget
;;;                            +----GtkButtonBox
;;;                            +----GtkColorSelection
;;;                            +----GtkFileChooserButton
;;;                            +----GtkFileChooserWidget
;;;                            +----GtkFontChooserWidget
;;;                            +----GtkFontSelection
;;;                            +----GtkHBox
;;;                            +----GtkInfoBar
;;;                            +----GtkRecentChooserWidget
;;;                            +----GtkStatusbar
;;;                            +----GtkVBox
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkBox implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; Properties
;;; 
;;;   "homogeneous"              gboolean              : Read / Write
;;;   "spacing"                  gint                  : Read / Write
;;; 
;;; Child Properties
;;; 
;;;   "expand"                   gboolean              : Read / Write
;;;   "fill"                     gboolean              : Read / Write
;;;   "pack-type"                GtkPackType           : Read / Write
;;;   "padding"                  guint                 : Read / Write
;;;   "position"                 gint                  : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkBox widget organizes child widgets into a rectangular area.
;;; 
;;; The rectangular area of a GtkBox is organized into either a single row or a
;;; single column of child widgets depending upon the orientation. Thus, all
;;; children of a GtkBox are allocated one dimension in common, which is the
;;; height of a row, or the width of a column.
;;; 
;;; GtkBox uses a notion of packing. Packing refers to adding widgets with
;;; reference to a particular position in a GtkContainer. For a GtkBox, there
;;; are two reference positions: the start and the end of the box. For a
;;; vertical GtkBox, the start is defined as the top of the box and the end is
;;; defined as the bottom. For a horizontal GtkBox the start is defined as the
;;; left side and the end is defined as the right side.
;;; 
;;; Use repeated calls to gtk_box_pack_start() to pack widgets into a GtkBox
;;; from start to end. Use gtk_box_pack_end() to add widgets from end to start.
;;; You may intersperse these calls and add widgets from both ends of the same
;;; GtkBox.
;;; 
;;; Because GtkBox is a GtkContainer, you may also use gtk_container_add() to
;;; insert widgets into the box, and they will be packed with the default values
;;; for "expand" and "fill". Use gtk_container_remove() to remove widgets from
;;; the GtkBox.
;;; 
;;; Use gtk_box_set_homogeneous() to specify whether or not all children of the
;;; GtkBox are forced to get the same amount of space.
;;; 
;;; Use gtk_box_set_spacing() to determine how much space will be minimally
;;; placed between all children in the GtkBox. Note that spacing is added
;;; between the children, while padding added by gtk_box_pack_start() or
;;; gtk_box_pack_end() is added on either side of the widget it belongs to.
;;; 
;;; Use gtk_box_reorder_child() to move a GtkBox child to a different place in
;;; the box.
;;; 
;;; Use gtk_box_set_child_packing() to reset the "expand", "fill" and "padding"
;;; child properties. Use gtk_box_query_child_packing() to query these fields.
;;; 
;;; Note
;;; 
;;; Note that a single-row or single-column GtkGrid provides exactly the same
;;; functionality as GtkBox.
;;; ---------------------------------------------------------------------------- 

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkBox
;;; 
;;; struct GtkBox;
;;; 
;;; gtk_box_new ()
;;; 
;;; GtkWidget * gtk_box_new (GtkOrientation orientation, gint spacing)
;;; 
;;; Creates a new GtkBox.
;;; 
;;; orientation :
;;; 	the box's orientation.
;;; 
;;; spacing :
;;; 	the number of pixels to place by default between children.
;;; 
;;; Returns :
;;; 	a new GtkBox.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_box_pack_start ()
;;; 
;;; void gtk_box_pack_start (GtkBox *box,
;;;                          GtkWidget *child,
;;;                          gboolean expand,
;;;                          gboolean fill,
;;;                          guint padding)
;;; 
;;; Adds child to box, packed with reference to the start of box. The child is
;;; packed after any other child packed with reference to the start of box.
;;; 
;;; box :
;;; 	a GtkBox
;;; 
;;; child :
;;; 	the GtkWidget to be added to box
;;; 
;;; expand :
;;; 	TRUE if the new child is to be given extra space allocated to box. The
;;;     extra space will be divided evenly between all children that use this
;;;     option
;;; 
;;; fill :
;;; 	TRUE if space given to child by the expand option is actually allocated
;;;     to child, rather than just padding it. This parameter has no effect if
;;;     expand is set to FALSE. A child is always allocated the full height of
;;;     a horizontal GtkBox and the full width of a vertical GtkBox. This option
;;;     affects the other dimension
;;; 
;;; padding :
;;; 	extra space in pixels to put between this child and its neighbors, over
;;;     and above the global amount specified by "spacing" property. If child is
;;;     a widget at one of the reference ends of box, then padding pixels are
;;;     also put between child and the reference edge of box
;;; ----------------------------------------------------------------------------

(defcfun gtk-box-pack-start :void
  (box (g-object box))
  (child (g-object widget))
  (expand :boolean)
  (fill :boolean)
  (padding :uint))

(defun box-pack-start (box child &key (expand t) (fill t) (padding 0))
  (gtk-box-pack-start box child expand fill padding))

(export 'box-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_box_pack_end ()
;;; 
;;; void gtk_box_pack_end (GtkBox *box,
;;;                        GtkWidget *child,
;;;                        gboolean expand,
;;;                        gboolean fill,
;;;                        guint padding)
;;; 
;;; Adds child to box, packed with reference to the end of box. The child is
;;; packed after (away from end of) any other child packed with reference to
;;; the end of box.
;;; 
;;; box :
;;; 	a GtkBox
;;; 
;;; child :
;;; 	the GtkWidget to be added to box
;;; 
;;; expand :
;;; 	TRUE if the new child is to be given extra space allocated to box. The
;;;     extra space will be divided evenly between all children of box that use
;;;     this option
;;; 
;;; fill :
;;; 	TRUE if space given to child by the expand option is actually allocated
;;;     to child, rather than just padding it. This parameter has no effect if
;;;     expand is set to FALSE. A child is always allocated the full height of
;;;     a horizontal GtkBox and the full width of a vertical GtkBox. This option
;;;     affects the other dimension
;;; 
;;; padding :
;;; 	extra space in pixels to put between this child and its neighbors, over
;;;     and above the global amount specified by "spacing" property. If child is
;;;     a widget at one of the reference ends of box, then padding pixels are
;;;     also put between child and the reference edge of box
;;; ----------------------------------------------------------------------------

(defcfun gtk-box-pack-end :void
  (box (g-object box))
  (child (g-object widget))
  (expand :boolean)
  (fill :boolean)
  (padding :uint))

(defun box-pack-end (box child &key (expand t) (fill t) (padding 0))
  (gtk-box-pack-end box child expand fill padding))

(export 'box-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_box_get_homogeneous ()
;;; 
;;; gboolean gtk_box_get_homogeneous (GtkBox *box)
;;; 
;;; Returns whether the box is homogeneous (all children are the same size).
;;; See gtk_box_set_homogeneous().
;;; 
;;; box :
;;; 	a GtkBox
;;; 
;;; Returns :
;;; 	TRUE if the box is homogeneous.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_box_set_homogeneous ()
;;; 
;;; void gtk_box_set_homogeneous (GtkBox *box, gboolean homogeneous)
;;; 
;;; Sets the "homogeneous" property of box, controlling whether or not all
;;; children of box are given equal space in the box.
;;; 
;;; box :
;;; 	a GtkBox
;;; 
;;; homogeneous :
;;; 	a boolean value, TRUE to create equal allotments, FALSE for variable
;;;     allotments
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_box_get_spacing ()
;;; 
;;; gint gtk_box_get_spacing (GtkBox *box)
;;; 
;;; Gets the value set by gtk_box_set_spacing().
;;; 
;;; box :
;;; 	a GtkBox
;;; 
;;; Returns :
;;; 	spacing between children
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_box_set_spacing ()
;;; 
;;; void gtk_box_set_spacing (GtkBox *box, gint spacing)
;;; 
;;; Sets the "spacing" property of box, which is the number of pixels to place
;;; between children of box.
;;; 
;;; box :
;;; 	a GtkBox
;;; 
;;; spacing :
;;; 	the number of pixels to put between children
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_box_reorder_child ()
;;; 
;;; void gtk_box_reorder_child (GtkBox *box, GtkWidget *child, gint position)
;;; 
;;; Moves child to a new position in the list of box children. The list is the
;;; children field of GtkBox, and contains both widgets packed GTK_PACK_START
;;; as well as widgets packed GTK_PACK_END, in the order that these widgets
;;; were added to box.
;;; 
;;; A widget's position in the box children list determines where the widget is
;;; packed into box. A child widget at some position in the list will be packed
;;; just after all other widgets of the same packing type that appear earlier
;;; in the list.
;;; 
;;; box :
;;; 	a GtkBox
;;; 
;;; child :
;;; 	the GtkWidget to move
;;; 
;;; position :
;;; 	the new position for child in the list of children of box, starting
;;;     from 0. If negative, indicates the end of the list
;;; ----------------------------------------------------------------------------

(defcfun (box-reorder-child "gtk_box_reorder_child") :void
  (box g-object)
  (child g-object)
  (position :int))

(export 'box-reorder-child)

;;; ----------------------------------------------------------------------------
;;; gtk_box_query_child_packing ()
;;; 
;;; void gtk_box_query_child_packing (GtkBox *box,
;;;                                   GtkWidget *child,
;;;                                   gboolean *expand,
;;;                                   gboolean *fill,
;;;                                   guint *padding,
;;;                                   GtkPackType *pack_type)
;;; 
;;; Obtains information about how child is packed into box.
;;; 
;;; box :
;;; 	a GtkBox
;;; 
;;; child :
;;; 	the GtkWidget of the child to query
;;; 
;;; expand :
;;; 	pointer to return location for "expand" child property. [out]
;;; 
;;; fill :
;;; 	pointer to return location for "fill" child property. [out]
;;; 
;;; padding :
;;; 	pointer to return location for "padding" child property. [out]
;;; 
;;; pack_type :
;;; 	pointer to return location for "pack-type" child property. [out]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_box_set_child_packing ()
;;; 
;;; void                gtk_box_set_child_packing           (GtkBox *box,
;;;                                                          GtkWidget *child,
;;;                                                          gboolean expand,
;;;                                                          gboolean fill,
;;;                                                          guint padding,
;;;                                                          GtkPackType pack_type);
;;; 
;;; Sets the way child is packed into box.
;;; 
;;; box :
;;; 	a GtkBox
;;; 
;;; child :
;;; 	the GtkWidget of the child to set
;;; 
;;; expand :
;;; 	the new value of the "expand" child property
;;; 
;;; fill :
;;; 	the new value of the "fill" child property
;;; 
;;; padding :
;;; 	the new value of the "padding" child property
;;; 
;;; pack_type :
;;; 	the new value of the "pack-type" child property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Property Details
;;;
;;; The "homogeneous" property
;;; 
;;;   "homogeneous"              gboolean              : Read / Write
;;; 
;;; Whether the children should all be the same size.
;;; 
;;; Default value: FALSE
;;; The "spacing" property
;;; 
;;;   "spacing"                  gint                  : Read / Write
;;; 
;;; The amount of space between children.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Child Property Details
;;;
;;; The "expand" child property
;;; 
;;;   "expand"                   gboolean              : Read / Write
;;; 
;;; Whether the child should receive extra space when the parent grows.
;;; 
;;; Note that the default value for this property is FALSE for GtkBox, but
;;; GtkHBox, GtkVBox and other subclasses use the old default of TRUE.
;;; 
;;; Note that the "halign", "valign", "hexpand" and "vexpand" properties are
;;; the preferred way to influence child size allocation in containers.
;;; 
;;; Default value: FALSE
;;;
;;; The "fill" child property
;;; 
;;;   "fill"                     gboolean              : Read / Write
;;; 
;;; Whether the child should receive extra space when the parent grows.
;;; 
;;; Note that the "halign", "valign", "hexpand" and "vexpand" properties are
;;; the preferred way to influence child size allocation in containers.
;;; 
;;; Default value: TRUE
;;; The "pack-type" child property
;;; 
;;;   "pack-type"                GtkPackType           : Read / Write
;;; 
;;; A GtkPackType indicating whether the child is packed with reference to the
;;; start or end of the parent.
;;; 
;;; Default value: GTK_PACK_START
;;;
;;; The "padding" child property
;;; 
;;;   "padding"                  guint                 : Read / Write
;;; 
;;; Extra space to put between the child and its neighbors, in pixels.
;;; 
;;; Allowed values: <= G_MAXINT
;;; 
;;; Default value: 0
;;; The "position" child property
;;; 
;;;   "position"                 gint                  : Read / Write
;;; 
;;; The index of the child in the parent.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: 0
;;; ----------------------------------------------------------------------------



