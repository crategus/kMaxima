;;; ----------------------------------------------------------------------------
;;; gtk.text-mark.lisp
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
;;; GtkTextMark
;;; 
;;; GtkTextMark — A position in the buffer preserved across buffer modifications
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;; struct              GtkTextMark;
;;; GtkTextMark *       gtk_text_mark_new                   (const gchar *name,
;;;                                                          gboolean left_gravity);
;;; void                gtk_text_mark_set_visible           (GtkTextMark *mark,
;;;                                                          gboolean setting);
;;; gboolean            gtk_text_mark_get_visible           (GtkTextMark *mark);
;;; gboolean            gtk_text_mark_get_deleted           (GtkTextMark *mark);
;;; const gchar *       gtk_text_mark_get_name              (GtkTextMark *mark);
;;; GtkTextBuffer *     gtk_text_mark_get_buffer            (GtkTextMark *mark);
;;; gboolean            gtk_text_mark_get_left_gravity      (GtkTextMark *mark);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkTextMark
;;; 
;;; Properties
;;; 
;;;   "left-gravity"             gboolean              : Read / Write / Construct Only
;;;   "name"                     gchar*                : Read / Write / Construct Only
;;; 
;;; Description
;;; 
;;; You may wish to begin by reading the text widget conceptual overview which gives an overview of all the objects and data types related to the text widget and how they work together.
;;; 
;;; A GtkTextMark is like a bookmark in a text buffer; it preserves a position in the text. You can convert the mark to an iterator using gtk_text_buffer_get_iter_at_mark(). Unlike iterators, marks remain valid across buffer mutations, because their behavior is defined when text is inserted or deleted. When text containing a mark is deleted, the mark remains in the position originally occupied by the deleted text. When text is inserted at a mark, a mark with left gravity will be moved to the beginning of the newly-inserted text, and a mark with right gravity will be moved to the end.
;;; 
;;; [3]
;;; 
;;; Marks are reference counted, but the reference count only controls the validity of the memory; marks can be deleted from the buffer at any time with gtk_text_buffer_delete_mark(). Once deleted from the buffer, a mark is essentially useless.
;;; 
;;; Marks optionally have names; these can be convenient to avoid passing the GtkTextMark object around.
;;; 
;;; Marks are typically created using the gtk_text_buffer_create_mark() function.
;;; Details
;;; struct GtkTextMark
;;; 
;;; struct GtkTextMark;
;;; 
;;; gtk_text_mark_new ()
;;; 
;;; GtkTextMark *       gtk_text_mark_new                   (const gchar *name,
;;;                                                          gboolean left_gravity);
;;; 
;;; Creates a text mark. Add it to a buffer using gtk_text_buffer_add_mark(). If name is NULL, the mark is anonymous; otherwise, the mark can be retrieved by name using gtk_text_buffer_get_mark(). If a mark has left gravity, and text is inserted at the mark's current location, the mark will be moved to the left of the newly-inserted text. If the mark has right gravity (left_gravity = FALSE), the mark will end up on the right of newly-inserted text. The standard left-to-right cursor is a mark with right gravity (when you type, the cursor stays on the right side of the text you're typing).
;;; 
;;; name :
;;; 	mark name or NULL. [allow-none]
;;; 
;;; left_gravity :
;;; 	whether the mark should have left gravity
;;; 
;;; Returns :
;;; 	new GtkTextMark
;;; 
;;; Since 2.12
;;; gtk_text_mark_set_visible ()
;;; 
;;; void                gtk_text_mark_set_visible           (GtkTextMark *mark,
;;;                                                          gboolean setting);
;;; 
;;; Sets the visibility of mark; the insertion point is normally visible, i.e. you can see it as a vertical bar. Also, the text widget uses a visible mark to indicate where a drop will occur when dragging-and-dropping text. Most other marks are not visible. Marks are not visible by default.
;;; 
;;; mark :
;;; 	a GtkTextMark
;;; 
;;; setting :
;;; 	visibility of mark
;;; gtk_text_mark_get_visible ()
;;; 
;;; gboolean            gtk_text_mark_get_visible           (GtkTextMark *mark);
;;; 
;;; Returns TRUE if the mark is visible (i.e. a cursor is displayed for it).
;;; 
;;; mark :
;;; 	a GtkTextMark
;;; 
;;; Returns :
;;; 	TRUE if visible
;;; gtk_text_mark_get_deleted ()
;;; 
;;; gboolean            gtk_text_mark_get_deleted           (GtkTextMark *mark);
;;; 
;;; Returns TRUE if the mark has been removed from its buffer with gtk_text_buffer_delete_mark(). See gtk_text_buffer_add_mark() for a way to add it to a buffer again.
;;; 
;;; mark :
;;; 	a GtkTextMark
;;; 
;;; Returns :
;;; 	whether the mark is deleted
;;; gtk_text_mark_get_name ()
;;; 
;;; const gchar *       gtk_text_mark_get_name              (GtkTextMark *mark);
;;; 
;;; Returns the mark name; returns NULL for anonymous marks.
;;; 
;;; mark :
;;; 	a GtkTextMark
;;; 
;;; Returns :
;;; 	mark name
;;; gtk_text_mark_get_buffer ()
;;; 
;;; GtkTextBuffer *     gtk_text_mark_get_buffer            (GtkTextMark *mark);
;;; 
;;; Gets the buffer this mark is located inside, or NULL if the mark is deleted.
;;; 
;;; mark :
;;; 	a GtkTextMark
;;; 
;;; Returns :
;;; 	the mark's GtkTextBuffer. [transfer none]
;;; gtk_text_mark_get_left_gravity ()
;;; 
;;; gboolean            gtk_text_mark_get_left_gravity      (GtkTextMark *mark);
;;; 
;;; Determines whether the mark has left gravity.
;;; 
;;; mark :
;;; 	a GtkTextMark
;;; 
;;; Returns :
;;; 	TRUE if the mark has left gravity, FALSE otherwise
;;; Property Details
;;; The "left-gravity" property
;;; 
;;;   "left-gravity"             gboolean              : Read / Write / Construct Only
;;; 
;;; Whether the mark has left gravity.
;;; 
;;; Default value: FALSE
;;; The "name" property
;;; 
;;;   "name"                     gchar*                : Read / Write / Construct Only
;;; 
;;; Mark name.
;;; 
;;; Default value: NULL
;;; 
;;; 
;;; 
