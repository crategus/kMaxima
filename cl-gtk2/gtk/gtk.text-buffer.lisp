;;; ----------------------------------------------------------------------------
;;; gtk.text-buffer.lisp
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
;;; GtkTextBuffer
;;; 
;;; GtkTextBuffer — Stores attributed text for display in a GtkTextView
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;;                     GtkTextBuffer;
;;; GtkTextBuffer *     gtk_text_buffer_new                 (GtkTextTagTable *table);
;;; gint                gtk_text_buffer_get_line_count      (GtkTextBuffer *buffer);
;;; gint                gtk_text_buffer_get_char_count      (GtkTextBuffer *buffer);
;;; GtkTextTagTable *   gtk_text_buffer_get_tag_table       (GtkTextBuffer *buffer);
;;; void                gtk_text_buffer_insert              (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          const gchar *text,
;;;                                                          gint len);
;;; void                gtk_text_buffer_insert_at_cursor    (GtkTextBuffer *buffer,
;;;                                                          const gchar *text,
;;;                                                          gint len);
;;; gboolean            gtk_text_buffer_insert_interactive  (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          const gchar *text,
;;;                                                          gint len,
;;;                                                          gboolean default_editable);
;;; gboolean            gtk_text_buffer_insert_interactive_at_cursor
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          const gchar *text,
;;;                                                          gint len,
;;;                                                          gboolean default_editable);
;;; void                gtk_text_buffer_insert_range        (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; gboolean            gtk_text_buffer_insert_range_interactive
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end,
;;;                                                          gboolean default_editable);
;;; void                gtk_text_buffer_insert_with_tags    (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          const gchar *text,
;;;                                                          gint len,
;;;                                                          GtkTextTag *first_tag,
;;;                                                          ...);
;;; void                gtk_text_buffer_insert_with_tags_by_name
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          const gchar *text,
;;;                                                          gint len,
;;;                                                          const gchar *first_tag_name,
;;;                                                          ...);
;;; void                gtk_text_buffer_delete              (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *start,
;;;                                                          GtkTextIter *end);
;;; gboolean            gtk_text_buffer_delete_interactive  (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *start_iter,
;;;                                                          GtkTextIter *end_iter,
;;;                                                          gboolean default_editable);
;;; gboolean            gtk_text_buffer_backspace           (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          gboolean interactive,
;;;                                                          gboolean default_editable);
;;; void                gtk_text_buffer_set_text            (GtkTextBuffer *buffer,
;;;                                                          const gchar *text,
;;;                                                          gint len);
;;; gchar *             gtk_text_buffer_get_text            (GtkTextBuffer *buffer,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end,
;;;                                                          gboolean include_hidden_chars);
;;; gchar *             gtk_text_buffer_get_slice           (GtkTextBuffer *buffer,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end,
;;;                                                          gboolean include_hidden_chars);
;;; void                gtk_text_buffer_insert_pixbuf       (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          GdkPixbuf *pixbuf);
;;; void                gtk_text_buffer_insert_child_anchor (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          GtkTextChildAnchor *anchor);
;;; GtkTextChildAnchor * gtk_text_buffer_create_child_anchor
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter);
;;; GtkTextMark *       gtk_text_buffer_create_mark         (GtkTextBuffer *buffer,
;;;                                                          const gchar *mark_name,
;;;                                                          const GtkTextIter *where,
;;;                                                          gboolean left_gravity);
;;; void                gtk_text_buffer_move_mark           (GtkTextBuffer *buffer,
;;;                                                          GtkTextMark *mark,
;;;                                                          const GtkTextIter *where);
;;; void                gtk_text_buffer_move_mark_by_name   (GtkTextBuffer *buffer,
;;;                                                          const gchar *name,
;;;                                                          const GtkTextIter *where);
;;; void                gtk_text_buffer_add_mark            (GtkTextBuffer *buffer,
;;;                                                          GtkTextMark *mark,
;;;                                                          const GtkTextIter *where);
;;; void                gtk_text_buffer_delete_mark         (GtkTextBuffer *buffer,
;;;                                                          GtkTextMark *mark);
;;; void                gtk_text_buffer_delete_mark_by_name (GtkTextBuffer *buffer,
;;;                                                          const gchar *name);
;;; GtkTextMark *       gtk_text_buffer_get_mark            (GtkTextBuffer *buffer,
;;;                                                          const gchar *name);
;;; GtkTextMark *       gtk_text_buffer_get_insert          (GtkTextBuffer *buffer);
;;; GtkTextMark *       gtk_text_buffer_get_selection_bound (GtkTextBuffer *buffer);
;;; gboolean            gtk_text_buffer_get_has_selection   (GtkTextBuffer *buffer);
;;; void                gtk_text_buffer_place_cursor        (GtkTextBuffer *buffer,
;;;                                                          const GtkTextIter *where);
;;; void                gtk_text_buffer_select_range        (GtkTextBuffer *buffer,
;;;                                                          const GtkTextIter *ins,
;;;                                                          const GtkTextIter *bound);
;;; void                gtk_text_buffer_apply_tag           (GtkTextBuffer *buffer,
;;;                                                          GtkTextTag *tag,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; void                gtk_text_buffer_remove_tag          (GtkTextBuffer *buffer,
;;;                                                          GtkTextTag *tag,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; void                gtk_text_buffer_apply_tag_by_name   (GtkTextBuffer *buffer,
;;;                                                          const gchar *name,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; void                gtk_text_buffer_remove_tag_by_name  (GtkTextBuffer *buffer,
;;;                                                          const gchar *name,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; void                gtk_text_buffer_remove_all_tags     (GtkTextBuffer *buffer,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; GtkTextTag *        gtk_text_buffer_create_tag          (GtkTextBuffer *buffer,
;;;                                                          const gchar *tag_name,
;;;                                                          const gchar *first_property_name,
;;;                                                          ...);
;;; void                gtk_text_buffer_get_iter_at_line_offset
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          gint line_number,
;;;                                                          gint char_offset);
;;; void                gtk_text_buffer_get_iter_at_offset  (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          gint char_offset);
;;; void                gtk_text_buffer_get_iter_at_line    (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          gint line_number);
;;; void                gtk_text_buffer_get_iter_at_line_index
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          gint line_number,
;;;                                                          gint byte_index);
;;; void                gtk_text_buffer_get_iter_at_mark    (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          GtkTextMark *mark);
;;; void                gtk_text_buffer_get_iter_at_child_anchor
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          GtkTextChildAnchor *anchor);
;;; void                gtk_text_buffer_get_start_iter      (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter);
;;; void                gtk_text_buffer_get_end_iter        (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter);
;;; void                gtk_text_buffer_get_bounds          (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *start,
;;;                                                          GtkTextIter *end);
;;; gboolean            gtk_text_buffer_get_modified        (GtkTextBuffer *buffer);
;;; void                gtk_text_buffer_set_modified        (GtkTextBuffer *buffer,
;;;                                                          gboolean setting);
;;; gboolean            gtk_text_buffer_delete_selection    (GtkTextBuffer *buffer,
;;;                                                          gboolean interactive,
;;;                                                          gboolean default_editable);
;;; void                gtk_text_buffer_paste_clipboard     (GtkTextBuffer *buffer,
;;;                                                          GtkClipboard *clipboard,
;;;                                                          GtkTextIter *override_location,
;;;                                                          gboolean default_editable);
;;; void                gtk_text_buffer_copy_clipboard      (GtkTextBuffer *buffer,
;;;                                                          GtkClipboard *clipboard);
;;; void                gtk_text_buffer_cut_clipboard       (GtkTextBuffer *buffer,
;;;                                                          GtkClipboard *clipboard,
;;;                                                          gboolean default_editable);
;;; gboolean            gtk_text_buffer_get_selection_bounds
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *start,
;;;                                                          GtkTextIter *end);
;;; void                gtk_text_buffer_begin_user_action   (GtkTextBuffer *buffer);
;;; void                gtk_text_buffer_end_user_action     (GtkTextBuffer *buffer);
;;; void                gtk_text_buffer_add_selection_clipboard
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkClipboard *clipboard);
;;; void                gtk_text_buffer_remove_selection_clipboard
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkClipboard *clipboard);
;;; 
;;; enum                GtkTextBufferTargetInfo;
;;; gboolean            (*GtkTextBufferDeserializeFunc)     (GtkTextBuffer *register_buffer,
;;;                                                          GtkTextBuffer *content_buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          const guint8 *data,
;;;                                                          gsize length,
;;;                                                          gboolean create_tags,
;;;                                                          gpointer user_data,
;;;                                                          GError **error);
;;; gboolean            gtk_text_buffer_deserialize         (GtkTextBuffer *register_buffer,
;;;                                                          GtkTextBuffer *content_buffer,
;;;                                                          GdkAtom format,
;;;                                                          GtkTextIter *iter,
;;;                                                          const guint8 *data,
;;;                                                          gsize length,
;;;                                                          GError **error);
;;; gboolean            gtk_text_buffer_deserialize_get_can_create_tags
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GdkAtom format);
;;; void                gtk_text_buffer_deserialize_set_can_create_tags
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GdkAtom format,
;;;                                                          gboolean can_create_tags);
;;; GtkTargetList *     gtk_text_buffer_get_copy_target_list
;;;                                                         (GtkTextBuffer *buffer);
;;; GdkAtom *           gtk_text_buffer_get_deserialize_formats
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          gint *n_formats);
;;; GtkTargetList *     gtk_text_buffer_get_paste_target_list
;;;                                                         (GtkTextBuffer *buffer);
;;; GdkAtom *           gtk_text_buffer_get_serialize_formats
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          gint *n_formats);
;;; GdkAtom             gtk_text_buffer_register_deserialize_format
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          const gchar *mime_type,
;;;                                                          GtkTextBufferDeserializeFunc function,
;;;                                                          gpointer user_data,
;;;                                                          GDestroyNotify user_data_destroy);
;;; GdkAtom             gtk_text_buffer_register_deserialize_tagset
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          const gchar *tagset_name);
;;; GdkAtom             gtk_text_buffer_register_serialize_format
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          const gchar *mime_type,
;;;                                                          GtkTextBufferSerializeFunc function,
;;;                                                          gpointer user_data,
;;;                                                          GDestroyNotify user_data_destroy);
;;; GdkAtom             gtk_text_buffer_register_serialize_tagset
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          const gchar *tagset_name);
;;; guint8 *            (*GtkTextBufferSerializeFunc)       (GtkTextBuffer *register_buffer,
;;;                                                          GtkTextBuffer *content_buffer,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end,
;;;                                                          gsize *length,
;;;                                                          gpointer user_data);
;;; guint8 *            gtk_text_buffer_serialize           (GtkTextBuffer *register_buffer,
;;;                                                          GtkTextBuffer *content_buffer,
;;;                                                          GdkAtom format,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end,
;;;                                                          gsize *length);
;;; void                gtk_text_buffer_unregister_deserialize_format
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GdkAtom format);
;;; void                gtk_text_buffer_unregister_serialize_format
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GdkAtom format);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkTextBuffer
;;; 
;;; Properties
;;; 
;;;   "copy-target-list"         GtkTargetList*        : Read
;;;   "cursor-position"          gint                  : Read
;;;   "has-selection"            gboolean              : Read
;;;   "paste-target-list"        GtkTargetList*        : Read
;;;   "tag-table"                GtkTextTagTable*      : Read / Write / Construct Only
;;;   "text"                     gchar*                : Read / Write
;;; 
;;; Signals
;;; 
;;;   "apply-tag"                                      : Run Last
;;;   "begin-user-action"                              : Run Last
;;;   "changed"                                        : Run Last
;;;   "delete-range"                                   : Run Last
;;;   "end-user-action"                                : Run Last
;;;   "insert-child-anchor"                            : Run Last
;;;   "insert-pixbuf"                                  : Run Last
;;;   "insert-text"                                    : Run Last
;;;   "mark-deleted"                                   : Run Last
;;;   "mark-set"                                       : Run Last
;;;   "modified-changed"                               : Run Last
;;;   "paste-done"                                     : Run Last
;;;   "remove-tag"                                     : Run Last
;;; 
;;; Description
;;; 
;;; You may wish to begin by reading the text widget conceptual overview which gives an overview of all the objects and data types related to the text widget and how they work together.
;;; Details
;;; GtkTextBuffer
;;; 
;;; typedef struct _GtkTextBuffer GtkTextBuffer;
;;; 
;;; gtk_text_buffer_new ()
;;; 
;;; GtkTextBuffer *     gtk_text_buffer_new                 (GtkTextTagTable *table);
;;; 
;;; Creates a new text buffer.
;;; 
;;; table :
;;; 	a tag table, or NULL to create a new one. [allow-none]
;;; 
;;; Returns :
;;; 	a new text buffer
;;; gtk_text_buffer_get_line_count ()
;;; 
;;; gint                gtk_text_buffer_get_line_count      (GtkTextBuffer *buffer);
;;; 
;;; Obtains the number of lines in the buffer. This value is cached, so the function is very fast.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; Returns :
;;; 	number of lines in the buffer
;;; gtk_text_buffer_get_char_count ()
;;; 
;;; gint                gtk_text_buffer_get_char_count      (GtkTextBuffer *buffer);
;;; 
;;; Gets the number of characters in the buffer; note that characters and bytes are not the same, you can't e.g. expect the contents of the buffer in string form to be this many bytes long. The character count is cached, so this function is very fast.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; Returns :
;;; 	number of characters in the buffer
;;; gtk_text_buffer_get_tag_table ()
;;; 
;;; GtkTextTagTable *   gtk_text_buffer_get_tag_table       (GtkTextBuffer *buffer);
;;; 
;;; Get the GtkTextTagTable associated with this buffer.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; Returns :
;;; 	the buffer's tag table. [transfer none]
;;; gtk_text_buffer_insert ()
;;; 
;;; void                gtk_text_buffer_insert              (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          const gchar *text,
;;;                                                          gint len);
;;; 
;;; Inserts len bytes of text at position iter. If len is -1, text must be nul-terminated and will be inserted in its entirety. Emits the "insert-text" signal; insertion actually occurs in the default handler for the signal. iter is invalidated when insertion occurs (because the buffer contents change), but the default signal handler revalidates it to point to the end of the inserted text.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	a position in the buffer
;;; 
;;; text :
;;; 	text in UTF-8 format
;;; 
;;; len :
;;; 	length of text in bytes, or -1
;;; gtk_text_buffer_insert_at_cursor ()
;;; 
;;; void                gtk_text_buffer_insert_at_cursor    (GtkTextBuffer *buffer,
;;;                                                          const gchar *text,
;;;                                                          gint len);
;;; 
;;; Simply calls gtk_text_buffer_insert(), using the current cursor position as the insertion point.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; text :
;;; 	text in UTF-8 format
;;; 
;;; len :
;;; 	length of text, in bytes
;;; gtk_text_buffer_insert_interactive ()
;;; 
;;; gboolean            gtk_text_buffer_insert_interactive  (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          const gchar *text,
;;;                                                          gint len,
;;;                                                          gboolean default_editable);
;;; 
;;; Like gtk_text_buffer_insert(), but the insertion will not occur if iter is at a non-editable location in the buffer. Usually you want to prevent insertions at ineditable locations if the insertion results from a user action (is interactive).
;;; 
;;; default_editable indicates the editability of text that doesn't have a tag affecting editability applied to it. Typically the result of gtk_text_view_get_editable() is appropriate here.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	a position in buffer
;;; 
;;; text :
;;; 	some UTF-8 text
;;; 
;;; len :
;;; 	length of text in bytes, or -1
;;; 
;;; default_editable :
;;; 	default editability of buffer
;;; 
;;; Returns :
;;; 	whether text was actually inserted
;;; gtk_text_buffer_insert_interactive_at_cursor ()
;;; 
;;; gboolean            gtk_text_buffer_insert_interactive_at_cursor
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          const gchar *text,
;;;                                                          gint len,
;;;                                                          gboolean default_editable);
;;; 
;;; Calls gtk_text_buffer_insert_interactive() at the cursor position.
;;; 
;;; default_editable indicates the editability of text that doesn't have a tag affecting editability applied to it. Typically the result of gtk_text_view_get_editable() is appropriate here.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; text :
;;; 	text in UTF-8 format
;;; 
;;; len :
;;; 	length of text in bytes, or -1
;;; 
;;; default_editable :
;;; 	default editability of buffer
;;; 
;;; Returns :
;;; 	whether text was actually inserted
;;; gtk_text_buffer_insert_range ()
;;; 
;;; void                gtk_text_buffer_insert_range        (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; 
;;; Copies text, tags, and pixbufs between start and end (the order of start and end doesn't matter) and inserts the copy at iter. Used instead of simply getting/inserting text because it preserves images and tags. If start and end are in a different buffer from buffer, the two buffers must share the same tag table.
;;; 
;;; Implemented via emissions of the insert_text and apply_tag signals, so expect those.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	a position in buffer
;;; 
;;; start :
;;; 	a position in a GtkTextBuffer
;;; 
;;; end :
;;; 	another position in the same buffer as start
;;; gtk_text_buffer_insert_range_interactive ()
;;; 
;;; gboolean            gtk_text_buffer_insert_range_interactive
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end,
;;;                                                          gboolean default_editable);
;;; 
;;; Same as gtk_text_buffer_insert_range(), but does nothing if the insertion point isn't editable. The default_editable parameter indicates whether the text is editable at iter if no tags enclosing iter affect editability. Typically the result of gtk_text_view_get_editable() is appropriate here.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	a position in buffer
;;; 
;;; start :
;;; 	a position in a GtkTextBuffer
;;; 
;;; end :
;;; 	another position in the same buffer as start
;;; 
;;; default_editable :
;;; 	default editability of the buffer
;;; 
;;; Returns :
;;; 	whether an insertion was possible at iter
;;; gtk_text_buffer_insert_with_tags ()
;;; 
;;; void                gtk_text_buffer_insert_with_tags    (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          const gchar *text,
;;;                                                          gint len,
;;;                                                          GtkTextTag *first_tag,
;;;                                                          ...);
;;; 
;;; Inserts text into buffer at iter, applying the list of tags to the newly-inserted text. The last tag specified must be NULL to terminate the list. Equivalent to calling gtk_text_buffer_insert(), then gtk_text_buffer_apply_tag() on the inserted text; gtk_text_buffer_insert_with_tags() is just a convenience function.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	an iterator in buffer
;;; 
;;; text :
;;; 	UTF-8 text
;;; 
;;; len :
;;; 	length of text, or -1
;;; 
;;; first_tag :
;;; 	first tag to apply to text
;;; 
;;; ... :
;;; 	NULL-terminated list of tags to apply
;;; gtk_text_buffer_insert_with_tags_by_name ()
;;; 
;;; void                gtk_text_buffer_insert_with_tags_by_name
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          const gchar *text,
;;;                                                          gint len,
;;;                                                          const gchar *first_tag_name,
;;;                                                          ...);
;;; 
;;; Same as gtk_text_buffer_insert_with_tags(), but allows you to pass in tag names instead of tag objects.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	position in buffer
;;; 
;;; text :
;;; 	UTF-8 text
;;; 
;;; len :
;;; 	length of text, or -1
;;; 
;;; first_tag_name :
;;; 	name of a tag to apply to text
;;; 
;;; ... :
;;; 	more tag names
;;; gtk_text_buffer_delete ()
;;; 
;;; void                gtk_text_buffer_delete              (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *start,
;;;                                                          GtkTextIter *end);
;;; 
;;; Deletes text between start and end. The order of start and end is not actually relevant; gtk_text_buffer_delete() will reorder them. This function actually emits the "delete-range" signal, and the default handler of that signal deletes the text. Because the buffer is modified, all outstanding iterators become invalid after calling this function; however, the start and end will be re-initialized to point to the location where text was deleted.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; start :
;;; 	a position in buffer
;;; 
;;; end :
;;; 	another position in buffer
;;; gtk_text_buffer_delete_interactive ()
;;; 
;;; gboolean            gtk_text_buffer_delete_interactive  (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *start_iter,
;;;                                                          GtkTextIter *end_iter,
;;;                                                          gboolean default_editable);
;;; 
;;; Deletes all editable text in the given range. Calls gtk_text_buffer_delete() for each editable sub-range of [start,end). start and end are revalidated to point to the location of the last deleted range, or left untouched if no text was deleted.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; start_iter :
;;; 	start of range to delete
;;; 
;;; end_iter :
;;; 	end of range
;;; 
;;; default_editable :
;;; 	whether the buffer is editable by default
;;; 
;;; Returns :
;;; 	whether some text was actually deleted
;;; gtk_text_buffer_backspace ()
;;; 
;;; gboolean            gtk_text_buffer_backspace           (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          gboolean interactive,
;;;                                                          gboolean default_editable);
;;; 
;;; Performs the appropriate action as if the user hit the delete key with the cursor at the position specified by iter. In the normal case a single character will be deleted, but when combining accents are involved, more than one character can be deleted, and when precomposed character and accent combinations are involved, less than one character will be deleted.
;;; 
;;; Because the buffer is modified, all outstanding iterators become invalid after calling this function; however, the iter will be re-initialized to point to the location where text was deleted.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	a position in buffer
;;; 
;;; interactive :
;;; 	whether the deletion is caused by user interaction
;;; 
;;; default_editable :
;;; 	whether the buffer is editable by default
;;; 
;;; Returns :
;;; 	TRUE if the buffer was modified
;;; 
;;; Since 2.6
;;; gtk_text_buffer_set_text ()
;;; 
;;; void                gtk_text_buffer_set_text            (GtkTextBuffer *buffer,
;;;                                                          const gchar *text,
;;;                                                          gint len);
;;; 
;;; Deletes current contents of buffer, and inserts text instead. If len is -1, text must be nul-terminated. text must be valid UTF-8.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; text :
;;; 	UTF-8 text to insert
;;; 
;;; len :
;;; 	length of text in bytes
;;; gtk_text_buffer_get_text ()
;;; 
;;; gchar *             gtk_text_buffer_get_text            (GtkTextBuffer *buffer,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end,
;;;                                                          gboolean include_hidden_chars);
;;; 
;;; Returns the text in the range [start,end). Excludes undisplayed text (text marked with tags that set the invisibility attribute) if include_hidden_chars is FALSE. Does not include characters representing embedded images, so byte and character indexes into the returned string do not correspond to byte and character indexes into the buffer. Contrast with gtk_text_buffer_get_slice().
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; start :
;;; 	start of a range
;;; 
;;; end :
;;; 	end of a range
;;; 
;;; include_hidden_chars :
;;; 	whether to include invisible text
;;; 
;;; Returns :
;;; 	an allocated UTF-8 string
;;; gtk_text_buffer_get_slice ()
;;; 
;;; gchar *             gtk_text_buffer_get_slice           (GtkTextBuffer *buffer,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end,
;;;                                                          gboolean include_hidden_chars);
;;; 
;;; Returns the text in the range [start,end). Excludes undisplayed text (text marked with tags that set the invisibility attribute) if include_hidden_chars is FALSE. The returned string includes a 0xFFFC character whenever the buffer contains embedded images, so byte and character indexes into the returned string do correspond to byte and character indexes into the buffer. Contrast with gtk_text_buffer_get_text(). Note that 0xFFFC can occur in normal text as well, so it is not a reliable indicator that a pixbuf or widget is in the buffer.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; start :
;;; 	start of a range
;;; 
;;; end :
;;; 	end of a range
;;; 
;;; include_hidden_chars :
;;; 	whether to include invisible text
;;; 
;;; Returns :
;;; 	an allocated UTF-8 string
;;; gtk_text_buffer_insert_pixbuf ()
;;; 
;;; void                gtk_text_buffer_insert_pixbuf       (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          GdkPixbuf *pixbuf);
;;; 
;;; Inserts an image into the text buffer at iter. The image will be counted as one character in character counts, and when obtaining the buffer contents as a string, will be represented by the Unicode "object replacement character" 0xFFFC. Note that the "slice" variants for obtaining portions of the buffer as a string include this character for pixbufs, but the "text" variants do not. e.g. see gtk_text_buffer_get_slice() and gtk_text_buffer_get_text().
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	location to insert the pixbuf
;;; 
;;; pixbuf :
;;; 	a GdkPixbuf
;;; gtk_text_buffer_insert_child_anchor ()
;;; 
;;; void                gtk_text_buffer_insert_child_anchor (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          GtkTextChildAnchor *anchor);
;;; 
;;; Inserts a child widget anchor into the text buffer at iter. The anchor will be counted as one character in character counts, and when obtaining the buffer contents as a string, will be represented by the Unicode "object replacement character" 0xFFFC. Note that the "slice" variants for obtaining portions of the buffer as a string include this character for child anchors, but the "text" variants do not. E.g. see gtk_text_buffer_get_slice() and gtk_text_buffer_get_text(). Consider gtk_text_buffer_create_child_anchor() as a more convenient alternative to this function. The buffer will add a reference to the anchor, so you can unref it after insertion.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	location to insert the anchor
;;; 
;;; anchor :
;;; 	a GtkTextChildAnchor
;;; gtk_text_buffer_create_child_anchor ()
;;; 
;;; GtkTextChildAnchor * gtk_text_buffer_create_child_anchor
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter);
;;; 
;;; This is a convenience function which simply creates a child anchor with gtk_text_child_anchor_new() and inserts it into the buffer with gtk_text_buffer_insert_child_anchor(). The new anchor is owned by the buffer; no reference count is returned to the caller of gtk_text_buffer_create_child_anchor().
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	location in the buffer
;;; 
;;; Returns :
;;; 	the created child anchor. [transfer none]
;;; gtk_text_buffer_create_mark ()
;;; 
;;; GtkTextMark *       gtk_text_buffer_create_mark         (GtkTextBuffer *buffer,
;;;                                                          const gchar *mark_name,
;;;                                                          const GtkTextIter *where,
;;;                                                          gboolean left_gravity);
;;; 
;;; Creates a mark at position where. If mark_name is NULL, the mark is anonymous; otherwise, the mark can be retrieved by name using gtk_text_buffer_get_mark(). If a mark has left gravity, and text is inserted at the mark's current location, the mark will be moved to the left of the newly-inserted text. If the mark has right gravity (left_gravity = FALSE), the mark will end up on the right of newly-inserted text. The standard left-to-right cursor is a mark with right gravity (when you type, the cursor stays on the right side of the text you're typing).
;;; 
;;; The caller of this function does not own a reference to the returned GtkTextMark, so you can ignore the return value if you like. Marks are owned by the buffer and go away when the buffer does.
;;; 
;;; Emits the "mark-set" signal as notification of the mark's initial placement.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; mark_name :
;;; 	name for mark, or NULL. [allow-none]
;;; 
;;; where :
;;; 	location to place mark
;;; 
;;; left_gravity :
;;; 	whether the mark has left gravity
;;; 
;;; Returns :
;;; 	the new GtkTextMark object. [transfer none]
;;; gtk_text_buffer_move_mark ()
;;; 
;;; void                gtk_text_buffer_move_mark           (GtkTextBuffer *buffer,
;;;                                                          GtkTextMark *mark,
;;;                                                          const GtkTextIter *where);
;;; 
;;; Moves mark to the new location where. Emits the "mark-set" signal as notification of the move.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; mark :
;;; 	a GtkTextMark
;;; 
;;; where :
;;; 	new location for mark in buffer
;;; gtk_text_buffer_move_mark_by_name ()
;;; 
;;; void                gtk_text_buffer_move_mark_by_name   (GtkTextBuffer *buffer,
;;;                                                          const gchar *name,
;;;                                                          const GtkTextIter *where);
;;; 
;;; Moves the mark named name (which must exist) to location where. See gtk_text_buffer_move_mark() for details.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; name :
;;; 	name of a mark
;;; 
;;; where :
;;; 	new location for mark
;;; gtk_text_buffer_add_mark ()
;;; 
;;; void                gtk_text_buffer_add_mark            (GtkTextBuffer *buffer,
;;;                                                          GtkTextMark *mark,
;;;                                                          const GtkTextIter *where);
;;; 
;;; Adds the mark at position where. The mark must not be added to another buffer, and if its name is not NULL then there must not be another mark in the buffer with the same name.
;;; 
;;; Emits the "mark-set" signal as notification of the mark's initial placement.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; mark :
;;; 	the mark to add
;;; 
;;; where :
;;; 	location to place mark
;;; 
;;; Since 2.12
;;; gtk_text_buffer_delete_mark ()
;;; 
;;; void                gtk_text_buffer_delete_mark         (GtkTextBuffer *buffer,
;;;                                                          GtkTextMark *mark);
;;; 
;;; Deletes mark, so that it's no longer located anywhere in the buffer. Removes the reference the buffer holds to the mark, so if you haven't called g_object_ref() on the mark, it will be freed. Even if the mark isn't freed, most operations on mark become invalid, until it gets added to a buffer again with gtk_text_buffer_add_mark(). Use gtk_text_mark_get_deleted() to find out if a mark has been removed from its buffer. The "mark-deleted" signal will be emitted as notification after the mark is deleted.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; mark :
;;; 	a GtkTextMark in buffer
;;; gtk_text_buffer_delete_mark_by_name ()
;;; 
;;; void                gtk_text_buffer_delete_mark_by_name (GtkTextBuffer *buffer,
;;;                                                          const gchar *name);
;;; 
;;; Deletes the mark named name; the mark must exist. See gtk_text_buffer_delete_mark() for details.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; name :
;;; 	name of a mark in buffer
;;; gtk_text_buffer_get_mark ()
;;; 
;;; GtkTextMark *       gtk_text_buffer_get_mark            (GtkTextBuffer *buffer,
;;;                                                          const gchar *name);
;;; 
;;; Returns the mark named name in buffer buffer, or NULL if no such mark exists in the buffer.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; name :
;;; 	a mark name
;;; 
;;; Returns :
;;; 	a GtkTextMark, or NULL. [transfer none]
;;; gtk_text_buffer_get_insert ()
;;; 
;;; GtkTextMark *       gtk_text_buffer_get_insert          (GtkTextBuffer *buffer);
;;; 
;;; Returns the mark that represents the cursor (insertion point). Equivalent to calling gtk_text_buffer_get_mark() to get the mark named "insert", but very slightly more efficient, and involves less typing.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; Returns :
;;; 	insertion point mark. [transfer none]
;;; gtk_text_buffer_get_selection_bound ()
;;; 
;;; GtkTextMark *       gtk_text_buffer_get_selection_bound (GtkTextBuffer *buffer);
;;; 
;;; Returns the mark that represents the selection bound. Equivalent to calling gtk_text_buffer_get_mark() to get the mark named "selection_bound", but very slightly more efficient, and involves less typing.
;;; 
;;; The currently-selected text in buffer is the region between the "selection_bound" and "insert" marks. If "selection_bound" and "insert" are in the same place, then there is no current selection. gtk_text_buffer_get_selection_bounds() is another convenient function for handling the selection, if you just want to know whether there's a selection and what its bounds are.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; Returns :
;;; 	selection bound mark. [transfer none]
;;; gtk_text_buffer_get_has_selection ()
;;; 
;;; gboolean            gtk_text_buffer_get_has_selection   (GtkTextBuffer *buffer);
;;; 
;;; Indicates whether the buffer has some text currently selected.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; Returns :
;;; 	TRUE if the there is text selected
;;; 
;;; Since 2.10
;;; gtk_text_buffer_place_cursor ()
;;; 
;;; void                gtk_text_buffer_place_cursor        (GtkTextBuffer *buffer,
;;;                                                          const GtkTextIter *where);
;;; 
;;; This function moves the "insert" and "selection_bound" marks simultaneously. If you move them to the same place in two steps with gtk_text_buffer_move_mark(), you will temporarily select a region in between their old and new locations, which can be pretty inefficient since the temporarily-selected region will force stuff to be recalculated. This function moves them as a unit, which can be optimized.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; where :
;;; 	where to put the cursor
;;; gtk_text_buffer_select_range ()
;;; 
;;; void                gtk_text_buffer_select_range        (GtkTextBuffer *buffer,
;;;                                                          const GtkTextIter *ins,
;;;                                                          const GtkTextIter *bound);
;;; 
;;; This function moves the "insert" and "selection_bound" marks simultaneously. If you move them in two steps with gtk_text_buffer_move_mark(), you will temporarily select a region in between their old and new locations, which can be pretty inefficient since the temporarily-selected region will force stuff to be recalculated. This function moves them as a unit, which can be optimized.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; ins :
;;; 	where to put the "insert" mark
;;; 
;;; bound :
;;; 	where to put the "selection_bound" mark
;;; 
;;; Since 2.4
;;; gtk_text_buffer_apply_tag ()
;;; 
;;; void                gtk_text_buffer_apply_tag           (GtkTextBuffer *buffer,
;;;                                                          GtkTextTag *tag,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; 
;;; Emits the "apply-tag" signal on buffer. The default handler for the signal applies tag to the given range. start and end do not have to be in order.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; tag :
;;; 	a GtkTextTag
;;; 
;;; start :
;;; 	one bound of range to be tagged
;;; 
;;; end :
;;; 	other bound of range to be tagged
;;; gtk_text_buffer_remove_tag ()
;;; 
;;; void                gtk_text_buffer_remove_tag          (GtkTextBuffer *buffer,
;;;                                                          GtkTextTag *tag,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; 
;;; Emits the "remove-tag" signal. The default handler for the signal removes all occurrences of tag from the given range. start and end don't have to be in order.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; tag :
;;; 	a GtkTextTag
;;; 
;;; start :
;;; 	one bound of range to be untagged
;;; 
;;; end :
;;; 	other bound of range to be untagged
;;; gtk_text_buffer_apply_tag_by_name ()
;;; 
;;; void                gtk_text_buffer_apply_tag_by_name   (GtkTextBuffer *buffer,
;;;                                                          const gchar *name,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; 
;;; Calls gtk_text_tag_table_lookup() on the buffer's tag table to get a GtkTextTag, then calls gtk_text_buffer_apply_tag().
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; name :
;;; 	name of a named GtkTextTag
;;; 
;;; start :
;;; 	one bound of range to be tagged
;;; 
;;; end :
;;; 	other bound of range to be tagged
;;; gtk_text_buffer_remove_tag_by_name ()
;;; 
;;; void                gtk_text_buffer_remove_tag_by_name  (GtkTextBuffer *buffer,
;;;                                                          const gchar *name,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; 
;;; Calls gtk_text_tag_table_lookup() on the buffer's tag table to get a GtkTextTag, then calls gtk_text_buffer_remove_tag().
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; name :
;;; 	name of a GtkTextTag
;;; 
;;; start :
;;; 	one bound of range to be untagged
;;; 
;;; end :
;;; 	other bound of range to be untagged
;;; gtk_text_buffer_remove_all_tags ()
;;; 
;;; void                gtk_text_buffer_remove_all_tags     (GtkTextBuffer *buffer,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; 
;;; Removes all tags in the range between start and end. Be careful with this function; it could remove tags added in code unrelated to the code you're currently writing. That is, using this function is probably a bad idea if you have two or more unrelated code sections that add tags.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; start :
;;; 	one bound of range to be untagged
;;; 
;;; end :
;;; 	other bound of range to be untagged
;;; gtk_text_buffer_create_tag ()
;;; 
;;; GtkTextTag *        gtk_text_buffer_create_tag          (GtkTextBuffer *buffer,
;;;                                                          const gchar *tag_name,
;;;                                                          const gchar *first_property_name,
;;;                                                          ...);
;;; 
;;; Creates a tag and adds it to the tag table for buffer. Equivalent to calling gtk_text_tag_new() and then adding the tag to the buffer's tag table. The returned tag is owned by the buffer's tag table, so the ref count will be equal to one.
;;; 
;;; If tag_name is NULL, the tag is anonymous.
;;; 
;;; If tag_name is non-NULL, a tag called tag_name must not already exist in the tag table for this buffer.
;;; 
;;; The first_property_name argument and subsequent arguments are a list of properties to set on the tag, as with g_object_set().
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; tag_name :
;;; 	name of the new tag, or NULL. [allow-none]
;;; 
;;; first_property_name :
;;; 	name of first property to set, or NULL. [allow-none]
;;; 
;;; ... :
;;; 	NULL-terminated list of property names and values
;;; 
;;; Returns :
;;; 	a new tag. [transfer none]
;;; gtk_text_buffer_get_iter_at_line_offset ()
;;; 
;;; void                gtk_text_buffer_get_iter_at_line_offset
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          gint line_number,
;;;                                                          gint char_offset);
;;; 
;;; Obtains an iterator pointing to char_offset within the given line. The char_offset must exist, offsets off the end of the line are not allowed. Note characters, not bytes; UTF-8 may encode one character as multiple bytes.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	iterator to initialize. [out]
;;; 
;;; line_number :
;;; 	line number counting from 0
;;; 
;;; char_offset :
;;; 	char offset from start of line
;;; gtk_text_buffer_get_iter_at_offset ()
;;; 
;;; void                gtk_text_buffer_get_iter_at_offset  (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          gint char_offset);
;;; 
;;; Initializes iter to a position char_offset chars from the start of the entire buffer. If char_offset is -1 or greater than the number of characters in the buffer, iter is initialized to the end iterator, the iterator one past the last valid character in the buffer.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	iterator to initialize. [out]
;;; 
;;; char_offset :
;;; 	char offset from start of buffer, counting from 0, or -1
;;; gtk_text_buffer_get_iter_at_line ()
;;; 
;;; void                gtk_text_buffer_get_iter_at_line    (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          gint line_number);
;;; 
;;; Initializes iter to the start of the given line.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	iterator to initialize. [out]
;;; 
;;; line_number :
;;; 	line number counting from 0
;;; gtk_text_buffer_get_iter_at_line_index ()
;;; 
;;; void                gtk_text_buffer_get_iter_at_line_index
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          gint line_number,
;;;                                                          gint byte_index);
;;; 
;;; Obtains an iterator pointing to byte_index within the given line. byte_index must be the start of a UTF-8 character, and must not be beyond the end of the line. Note bytes, not characters; UTF-8 may encode one character as multiple bytes.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	iterator to initialize. [out]
;;; 
;;; line_number :
;;; 	line number counting from 0
;;; 
;;; byte_index :
;;; 	byte index from start of line
;;; gtk_text_buffer_get_iter_at_mark ()
;;; 
;;; void                gtk_text_buffer_get_iter_at_mark    (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          GtkTextMark *mark);
;;; 
;;; Initializes iter with the current position of mark.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	iterator to initialize. [out]
;;; 
;;; mark :
;;; 	a GtkTextMark in buffer
;;; gtk_text_buffer_get_iter_at_child_anchor ()
;;; 
;;; void                gtk_text_buffer_get_iter_at_child_anchor
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          GtkTextChildAnchor *anchor);
;;; 
;;; Obtains the location of anchor within buffer.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	an iterator to be initialized. [out]
;;; 
;;; anchor :
;;; 	a child anchor that appears in buffer
;;; gtk_text_buffer_get_start_iter ()
;;; 
;;; void                gtk_text_buffer_get_start_iter      (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter);
;;; 
;;; Initialized iter with the first position in the text buffer. This is the same as using gtk_text_buffer_get_iter_at_offset() to get the iter at character offset 0.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	iterator to initialize. [out]
;;; gtk_text_buffer_get_end_iter ()
;;; 
;;; void                gtk_text_buffer_get_end_iter        (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *iter);
;;; 
;;; Initializes iter with the "end iterator," one past the last valid character in the text buffer. If dereferenced with gtk_text_iter_get_char(), the end iterator has a character value of 0. The entire buffer lies in the range from the first position in the buffer (call gtk_text_buffer_get_start_iter() to get character position 0) to the end iterator.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; iter :
;;; 	iterator to initialize. [out]
;;; gtk_text_buffer_get_bounds ()
;;; 
;;; void                gtk_text_buffer_get_bounds          (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *start,
;;;                                                          GtkTextIter *end);
;;; 
;;; Retrieves the first and last iterators in the buffer, i.e. the entire buffer lies within the range [start,end).
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; start :
;;; 	iterator to initialize with first position in the buffer. [out]
;;; 
;;; end :
;;; 	iterator to initialize with the end iterator. [out]
;;; gtk_text_buffer_get_modified ()
;;; 
;;; gboolean            gtk_text_buffer_get_modified        (GtkTextBuffer *buffer);
;;; 
;;; Indicates whether the buffer has been modified since the last call to gtk_text_buffer_set_modified() set the modification flag to FALSE. Used for example to enable a "save" function in a text editor.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; Returns :
;;; 	TRUE if the buffer has been modified
;;; gtk_text_buffer_set_modified ()
;;; 
;;; void                gtk_text_buffer_set_modified        (GtkTextBuffer *buffer,
;;;                                                          gboolean setting);
;;; 
;;; Used to keep track of whether the buffer has been modified since the last time it was saved. Whenever the buffer is saved to disk, call gtk_text_buffer_set_modified (buffer, FALSE). When the buffer is modified, it will automatically toggled on the modified bit again. When the modified bit flips, the buffer emits a "modified-changed" signal.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; setting :
;;; 	modification flag setting
;;; gtk_text_buffer_delete_selection ()
;;; 
;;; gboolean            gtk_text_buffer_delete_selection    (GtkTextBuffer *buffer,
;;;                                                          gboolean interactive,
;;;                                                          gboolean default_editable);
;;; 
;;; Deletes the range between the "insert" and "selection_bound" marks, that is, the currently-selected text. If interactive is TRUE, the editability of the selection will be considered (users can't delete uneditable text).
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; interactive :
;;; 	whether the deletion is caused by user interaction
;;; 
;;; default_editable :
;;; 	whether the buffer is editable by default
;;; 
;;; Returns :
;;; 	whether there was a non-empty selection to delete
;;; gtk_text_buffer_paste_clipboard ()
;;; 
;;; void                gtk_text_buffer_paste_clipboard     (GtkTextBuffer *buffer,
;;;                                                          GtkClipboard *clipboard,
;;;                                                          GtkTextIter *override_location,
;;;                                                          gboolean default_editable);
;;; 
;;; Pastes the contents of a clipboard at the insertion point, or at override_location. (Note: pasting is asynchronous, that is, we'll ask for the paste data and return, and at some point later after the main loop runs, the paste data will be inserted.)
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; clipboard :
;;; 	the GtkClipboard to paste from
;;; 
;;; override_location :
;;; 	location to insert pasted text, or NULL for at the cursor. [allow-none]
;;; 
;;; default_editable :
;;; 	whether the buffer is editable by default
;;; gtk_text_buffer_copy_clipboard ()
;;; 
;;; void                gtk_text_buffer_copy_clipboard      (GtkTextBuffer *buffer,
;;;                                                          GtkClipboard *clipboard);
;;; 
;;; Copies the currently-selected text to a clipboard.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; clipboard :
;;; 	the GtkClipboard object to copy to
;;; gtk_text_buffer_cut_clipboard ()
;;; 
;;; void                gtk_text_buffer_cut_clipboard       (GtkTextBuffer *buffer,
;;;                                                          GtkClipboard *clipboard,
;;;                                                          gboolean default_editable);
;;; 
;;; Copies the currently-selected text to a clipboard, then deletes said text if it's editable.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; clipboard :
;;; 	the GtkClipboard object to cut to
;;; 
;;; default_editable :
;;; 	default editability of the buffer
;;; gtk_text_buffer_get_selection_bounds ()
;;; 
;;; gboolean            gtk_text_buffer_get_selection_bounds
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkTextIter *start,
;;;                                                          GtkTextIter *end);
;;; 
;;; Returns TRUE if some text is selected; places the bounds of the selection in start and end (if the selection has length 0, then start and end are filled in with the same value). start and end will be in ascending order. If start and end are NULL, then they are not filled in, but the return value still indicates whether text is selected.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer a GtkTextBuffer
;;; 
;;; start :
;;; 	iterator to initialize with selection start. [out]
;;; 
;;; end :
;;; 	iterator to initialize with selection end. [out]
;;; 
;;; Returns :
;;; 	whether the selection has nonzero length
;;; gtk_text_buffer_begin_user_action ()
;;; 
;;; void                gtk_text_buffer_begin_user_action   (GtkTextBuffer *buffer);
;;; 
;;; Called to indicate that the buffer operations between here and a call to gtk_text_buffer_end_user_action() are part of a single user-visible operation. The operations between gtk_text_buffer_begin_user_action() and gtk_text_buffer_end_user_action() can then be grouped when creating an undo stack. GtkTextBuffer maintains a count of calls to gtk_text_buffer_begin_user_action() that have not been closed with a call to gtk_text_buffer_end_user_action(), and emits the "begin-user-action" and "end-user-action" signals only for the outermost pair of calls. This allows you to build user actions from other user actions.
;;; 
;;; The "interactive" buffer mutation functions, such as gtk_text_buffer_insert_interactive(), automatically call begin/end user action around the buffer operations they perform, so there's no need to add extra calls if you user action consists solely of a single call to one of those functions.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; gtk_text_buffer_end_user_action ()
;;; 
;;; void                gtk_text_buffer_end_user_action     (GtkTextBuffer *buffer);
;;; 
;;; Should be paired with a call to gtk_text_buffer_begin_user_action(). See that function for a full explanation.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; gtk_text_buffer_add_selection_clipboard ()
;;; 
;;; void                gtk_text_buffer_add_selection_clipboard
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkClipboard *clipboard);
;;; 
;;; Adds clipboard to the list of clipboards in which the selection contents of buffer are available. In most cases, clipboard will be the GtkClipboard of type GDK_SELECTION_PRIMARY for a view of buffer.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; clipboard :
;;; 	a GtkClipboard
;;; gtk_text_buffer_remove_selection_clipboard ()
;;; 
;;; void                gtk_text_buffer_remove_selection_clipboard
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GtkClipboard *clipboard);
;;; 
;;; Removes a GtkClipboard added with gtk_text_buffer_add_selection_clipboard().
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; clipboard :
;;; 	a GtkClipboard added to buffer by gtk_text_buffer_add_selection_clipboard()
;;; enum GtkTextBufferTargetInfo
;;; 
;;; typedef enum {
;;;   GTK_TEXT_BUFFER_TARGET_INFO_BUFFER_CONTENTS = - 1,
;;;   GTK_TEXT_BUFFER_TARGET_INFO_RICH_TEXT       = - 2,
;;;   GTK_TEXT_BUFFER_TARGET_INFO_TEXT            = - 3
;;; } GtkTextBufferTargetInfo;
;;; 
;;; GtkTextBufferDeserializeFunc ()
;;; 
;;; gboolean            (*GtkTextBufferDeserializeFunc)     (GtkTextBuffer *register_buffer,
;;;                                                          GtkTextBuffer *content_buffer,
;;;                                                          GtkTextIter *iter,
;;;                                                          const guint8 *data,
;;;                                                          gsize length,
;;;                                                          gboolean create_tags,
;;;                                                          gpointer user_data,
;;;                                                          GError **error);
;;; 
;;; A function that is called to deserialize rich text that has been serialized with gtk_text_buffer_serialize(), and insert it at iter.
;;; 
;;; register_buffer :
;;; 	the GtkTextBuffer the format is registered with
;;; 
;;; content_buffer :
;;; 	the GtkTextBuffer to deserialize into
;;; 
;;; iter :
;;; 	insertion point for the deserialized text
;;; 
;;; data :
;;; 	data to deserialize
;;; 
;;; length :
;;; 	length of data
;;; 
;;; create_tags :
;;; 	TRUE if deserializing may create tags
;;; 
;;; user_data :
;;; 	user data that was specified when registering the format
;;; 
;;; error :
;;; 	return location for a GError
;;; 
;;; Returns :
;;; 	TRUE on success, FALSE otherwise
;;; gtk_text_buffer_deserialize ()
;;; 
;;; gboolean            gtk_text_buffer_deserialize         (GtkTextBuffer *register_buffer,
;;;                                                          GtkTextBuffer *content_buffer,
;;;                                                          GdkAtom format,
;;;                                                          GtkTextIter *iter,
;;;                                                          const guint8 *data,
;;;                                                          gsize length,
;;;                                                          GError **error);
;;; 
;;; This function deserializes rich text in format format and inserts it at iter.
;;; 
;;; formats to be used must be registered using gtk_text_buffer_register_deserialize_format() or gtk_text_buffer_register_deserialize_tagset() beforehand.
;;; 
;;; register_buffer :
;;; 	the GtkTextBuffer format is registered with
;;; 
;;; content_buffer :
;;; 	the GtkTextBuffer to deserialize into
;;; 
;;; format :
;;; 	the rich text format to use for deserializing
;;; 
;;; iter :
;;; 	insertion point for the deserialized text
;;; 
;;; data :
;;; 	data to deserialize. [array length=length]
;;; 
;;; length :
;;; 	length of data
;;; 
;;; error :
;;; 	return location for a GError
;;; 
;;; Returns :
;;; 	TRUE on success, FALSE otherwise.
;;; 
;;; Since 2.10
;;; gtk_text_buffer_deserialize_get_can_create_tags ()
;;; 
;;; gboolean            gtk_text_buffer_deserialize_get_can_create_tags
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GdkAtom format);
;;; 
;;; This functions returns the value set with gtk_text_buffer_deserialize_set_can_create_tags()
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; format :
;;; 	a GdkAtom representing a registered rich text format
;;; 
;;; Returns :
;;; 	whether deserializing this format may create tags
;;; 
;;; Since 2.10
;;; gtk_text_buffer_deserialize_set_can_create_tags ()
;;; 
;;; void                gtk_text_buffer_deserialize_set_can_create_tags
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GdkAtom format,
;;;                                                          gboolean can_create_tags);
;;; 
;;; Use this function to allow a rich text deserialization function to create new tags in the receiving buffer. Note that using this function is almost always a bad idea, because the rich text functions you register should know how to map the rich text format they handler to your text buffers set of tags.
;;; 
;;; The ability of creating new (arbitrary!) tags in the receiving buffer is meant for special rich text formats like the internal one that is registered using gtk_text_buffer_register_deserialize_tagset(), because that format is essentially a dump of the internal structure of the source buffer, including its tag names.
;;; 
;;; You should allow creation of tags only if you know what you are doing, e.g. if you defined a tagset name for your application suite's text buffers and you know that it's fine to receive new tags from these buffers, because you know that your application can handle the newly created tags.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; format :
;;; 	a GdkAtom representing a registered rich text format
;;; 
;;; can_create_tags :
;;; 	whether deserializing this format may create tags
;;; 
;;; Since 2.10
;;; gtk_text_buffer_get_copy_target_list ()
;;; 
;;; GtkTargetList *     gtk_text_buffer_get_copy_target_list
;;;                                                         (GtkTextBuffer *buffer);
;;; 
;;; This function returns the list of targets this text buffer can provide for copying and as DND source. The targets in the list are added with info values from the GtkTextBufferTargetInfo enum, using gtk_target_list_add_rich_text_targets() and gtk_target_list_add_text_targets().
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; Returns :
;;; 	the GtkTargetList. [transfer none]
;;; 
;;; Since 2.10
;;; gtk_text_buffer_get_deserialize_formats ()
;;; 
;;; GdkAtom *           gtk_text_buffer_get_deserialize_formats
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          gint *n_formats);
;;; 
;;; This function returns the rich text deserialize formats registered with buffer using gtk_text_buffer_register_deserialize_format() or gtk_text_buffer_register_deserialize_tagset()
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; n_formats :
;;; 	return location for the number of formats
;;; 
;;; Returns :
;;; 	an array of GdkAtoms representing the registered formats. [array length=n_formats][transfer container]
;;; 
;;; Since 2.10
;;; gtk_text_buffer_get_paste_target_list ()
;;; 
;;; GtkTargetList *     gtk_text_buffer_get_paste_target_list
;;;                                                         (GtkTextBuffer *buffer);
;;; 
;;; This function returns the list of targets this text buffer supports for pasting and as DND destination. The targets in the list are added with info values from the GtkTextBufferTargetInfo enum, using gtk_target_list_add_rich_text_targets() and gtk_target_list_add_text_targets().
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; Returns :
;;; 	the GtkTargetList. [transfer none]
;;; 
;;; Since 2.10
;;; gtk_text_buffer_get_serialize_formats ()
;;; 
;;; GdkAtom *           gtk_text_buffer_get_serialize_formats
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          gint *n_formats);
;;; 
;;; This function returns the rich text serialize formats registered with buffer using gtk_text_buffer_register_serialize_format() or gtk_text_buffer_register_serialize_tagset()
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; n_formats :
;;; 	return location for the number of formats
;;; 
;;; Returns :
;;; 	an array of GdkAtoms representing the registered formats. [array length=n_formats][transfer container]
;;; 
;;; Since 2.10
;;; gtk_text_buffer_register_deserialize_format ()
;;; 
;;; GdkAtom             gtk_text_buffer_register_deserialize_format
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          const gchar *mime_type,
;;;                                                          GtkTextBufferDeserializeFunc function,
;;;                                                          gpointer user_data,
;;;                                                          GDestroyNotify user_data_destroy);
;;; 
;;; This function registers a rich text deserialization function along with its mime_type with the passed buffer.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; mime_type :
;;; 	the format's mime-type
;;; 
;;; function :
;;; 	the deserialize function to register
;;; 
;;; user_data :
;;; 	function's user_data
;;; 
;;; user_data_destroy :
;;; 	a function to call when user_data is no longer needed
;;; 
;;; Returns :
;;; 	the GdkAtom that corresponds to the newly registered format's mime-type. [transfer none]
;;; 
;;; Since 2.10
;;; gtk_text_buffer_register_deserialize_tagset ()
;;; 
;;; GdkAtom             gtk_text_buffer_register_deserialize_tagset
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          const gchar *tagset_name);
;;; 
;;; This function registers GTK+'s internal rich text serialization format with the passed buffer. See gtk_text_buffer_register_serialize_tagset() for details.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; tagset_name :
;;; 	an optional tagset name, on NULL. [allow-none]
;;; 
;;; Returns :
;;; 	the GdkAtom that corresponds to the newly registered format's mime-type. [transfer none]
;;; 
;;; Since 2.10
;;; gtk_text_buffer_register_serialize_format ()
;;; 
;;; GdkAtom             gtk_text_buffer_register_serialize_format
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          const gchar *mime_type,
;;;                                                          GtkTextBufferSerializeFunc function,
;;;                                                          gpointer user_data,
;;;                                                          GDestroyNotify user_data_destroy);
;;; 
;;; This function registers a rich text serialization function along with its mime_type with the passed buffer.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; mime_type :
;;; 	the format's mime-type
;;; 
;;; function :
;;; 	the serialize function to register
;;; 
;;; user_data :
;;; 	function's user_data
;;; 
;;; user_data_destroy :
;;; 	a function to call when user_data is no longer needed
;;; 
;;; Returns :
;;; 	the GdkAtom that corresponds to the newly registered format's mime-type. [transfer none]
;;; 
;;; Since 2.10
;;; gtk_text_buffer_register_serialize_tagset ()
;;; 
;;; GdkAtom             gtk_text_buffer_register_serialize_tagset
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          const gchar *tagset_name);
;;; 
;;; This function registers GTK+'s internal rich text serialization format with the passed buffer. The internal format does not comply to any standard rich text format and only works between GtkTextBuffer instances. It is capable of serializing all of a text buffer's tags and embedded pixbufs.
;;; 
;;; This function is just a wrapper around gtk_text_buffer_register_serialize_format(). The mime type used for registering is "application/x-gtk-text-buffer-rich-text", or "application/x-gtk-text-buffer-rich-text;format=tagset_name" if a tagset_name was passed.
;;; 
;;; The tagset_name can be used to restrict the transfer of rich text to buffers with compatible sets of tags, in order to avoid unknown tags from being pasted. It is probably the common case to pass an identifier != NULL here, since the NULL tagset requires the receiving buffer to deal with with pasting of arbitrary tags.
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; tagset_name :
;;; 	an optional tagset name, on NULL. [allow-none]
;;; 
;;; Returns :
;;; 	the GdkAtom that corresponds to the newly registered format's mime-type. [transfer none]
;;; 
;;; Since 2.10
;;; GtkTextBufferSerializeFunc ()
;;; 
;;; guint8 *            (*GtkTextBufferSerializeFunc)       (GtkTextBuffer *register_buffer,
;;;                                                          GtkTextBuffer *content_buffer,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end,
;;;                                                          gsize *length,
;;;                                                          gpointer user_data);
;;; 
;;; A function that is called to serialize the content of a text buffer. It must return the serialized form of the content.
;;; 
;;; register_buffer :
;;; 	the GtkTextBuffer for which the format is registered
;;; 
;;; content_buffer :
;;; 	the GtkTextBuffer to serialize
;;; 
;;; start :
;;; 	start of the block of text to serialize
;;; 
;;; end :
;;; 	end of the block of text to serialize
;;; 
;;; length :
;;; 	Return location for the length of the serialized data
;;; 
;;; user_data :
;;; 	user data that was specified when registering the format
;;; 
;;; Returns :
;;; 	a newly-allocated array of guint8 which contains the serialized data, or NULL if an error occurred
;;; gtk_text_buffer_serialize ()
;;; 
;;; guint8 *            gtk_text_buffer_serialize           (GtkTextBuffer *register_buffer,
;;;                                                          GtkTextBuffer *content_buffer,
;;;                                                          GdkAtom format,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end,
;;;                                                          gsize *length);
;;; 
;;; This function serializes the portion of text between start and end in the rich text format represented by format.
;;; 
;;; formats to be used must be registered using gtk_text_buffer_register_serialize_format() or gtk_text_buffer_register_serialize_tagset() beforehand.
;;; 
;;; register_buffer :
;;; 	the GtkTextBuffer format is registered with
;;; 
;;; content_buffer :
;;; 	the GtkTextBuffer to serialize
;;; 
;;; format :
;;; 	the rich text format to use for serializing
;;; 
;;; start :
;;; 	start of block of text to serialize
;;; 
;;; end :
;;; 	end of block of test to serialize
;;; 
;;; length :
;;; 	return location for the length of the serialized data
;;; 
;;; Returns :
;;; 	the serialized data, encoded as format. [array length=length][transfer full]
;;; 
;;; Since 2.10
;;; gtk_text_buffer_unregister_deserialize_format ()
;;; 
;;; void                gtk_text_buffer_unregister_deserialize_format
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GdkAtom format);
;;; 
;;; This function unregisters a rich text format that was previously registered using gtk_text_buffer_register_deserialize_format() or gtk_text_buffer_register_deserialize_tagset().
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; format :
;;; 	a GdkAtom representing a registered rich text format.
;;; 
;;; Since 2.10
;;; gtk_text_buffer_unregister_serialize_format ()
;;; 
;;; void                gtk_text_buffer_unregister_serialize_format
;;;                                                         (GtkTextBuffer *buffer,
;;;                                                          GdkAtom format);
;;; 
;;; This function unregisters a rich text format that was previously registered using gtk_text_buffer_register_serialize_format() or gtk_text_buffer_register_serialize_tagset()
;;; 
;;; buffer :
;;; 	a GtkTextBuffer
;;; 
;;; format :
;;; 	a GdkAtom representing a registered rich text format.
;;; 
;;; Since 2.10
;;; Property Details
;;; The "copy-target-list" property
;;; 
;;;   "copy-target-list"         GtkTargetList*        : Read
;;; 
;;; The list of targets this buffer supports for clipboard copying and as DND source.
;;; 
;;; Since 2.10
;;; The "cursor-position" property
;;; 
;;;   "cursor-position"          gint                  : Read
;;; 
;;; The position of the insert mark (as offset from the beginning of the buffer). It is useful for getting notified when the cursor moves.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;; 
;;; Since 2.10
;;; The "has-selection" property
;;; 
;;;   "has-selection"            gboolean              : Read
;;; 
;;; Whether the buffer has some text currently selected.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;; The "paste-target-list" property
;;; 
;;;   "paste-target-list"        GtkTargetList*        : Read
;;; 
;;; The list of targets this buffer supports for clipboard pasting and as DND destination.
;;; 
;;; Since 2.10
;;; The "tag-table" property
;;; 
;;;   "tag-table"                GtkTextTagTable*      : Read / Write / Construct Only
;;; 
;;; Text Tag Table.
;;; The "text" property
;;; 
;;;   "text"                     gchar*                : Read / Write
;;; 
;;; The text content of the buffer. Without child widgets and images, see gtk_text_buffer_get_text() for more information.
;;; 
;;; Default value: ""
;;; 
;;; Since 2.8
;;; Signal Details
;;; The "apply-tag" signal
;;; 
;;; void                user_function                      (GtkTextBuffer *textbuffer,
;;;                                                         GtkTextTag    *tag,
;;;                                                         GtkTextIter   *start,
;;;                                                         GtkTextIter   *end,
;;;                                                         gpointer       user_data)       : Run Last
;;; 
;;; The ::apply-tag signal is emitted to apply a tag to a range of text in a GtkTextBuffer. Applying actually occurs in the default handler.
;;; 
;;; Note that if your handler runs before the default handler it must not invalidate the start and end iters (or has to revalidate them).
;;; 
;;; See also: gtk_text_buffer_apply_tag(), gtk_text_buffer_insert_with_tags(), gtk_text_buffer_insert_range().
;;; 
;;; textbuffer :
;;; 	the object which received the signal
;;; 
;;; tag :
;;; 	the applied tag
;;; 
;;; start :
;;; 	the start of the range the tag is applied to
;;; 
;;; end :
;;; 	the end of the range the tag is applied to
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; The "begin-user-action" signal
;;; 
;;; void                user_function                      (GtkTextBuffer *textbuffer,
;;;                                                         gpointer       user_data)       : Run Last
;;; 
;;; The ::begin-user-action signal is emitted at the beginning of a single user-visible operation on a GtkTextBuffer.
;;; 
;;; See also: gtk_text_buffer_begin_user_action(), gtk_text_buffer_insert_interactive(), gtk_text_buffer_insert_range_interactive(), gtk_text_buffer_delete_interactive(), gtk_text_buffer_backspace(), gtk_text_buffer_delete_selection().
;;; 
;;; textbuffer :
;;; 	the object which received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; The "changed" signal
;;; 
;;; void                user_function                      (GtkTextBuffer *textbuffer,
;;;                                                         gpointer       user_data)       : Run Last
;;; 
;;; The ::changed signal is emitted when the content of a GtkTextBuffer has changed.
;;; 
;;; textbuffer :
;;; 	the object which received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; The "delete-range" signal
;;; 
;;; void                user_function                      (GtkTextBuffer *textbuffer,
;;;                                                         GtkTextIter   *start,
;;;                                                         GtkTextIter   *end,
;;;                                                         gpointer       user_data)       : Run Last
;;; 
;;; The ::delete-range signal is emitted to delete a range from a GtkTextBuffer.
;;; 
;;; Note that if your handler runs before the default handler it must not invalidate the start and end iters (or has to revalidate them). The default signal handler revalidates the start and end iters to both point point to the location where text was deleted. Handlers which run after the default handler (see g_signal_connect_after()) do not have access to the deleted text.
;;; 
;;; See also: gtk_text_buffer_delete().
;;; 
;;; textbuffer :
;;; 	the object which received the signal
;;; 
;;; start :
;;; 	the start of the range to be deleted
;;; 
;;; end :
;;; 	the end of the range to be deleted
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; The "end-user-action" signal
;;; 
;;; void                user_function                      (GtkTextBuffer *textbuffer,
;;;                                                         gpointer       user_data)       : Run Last
;;; 
;;; The ::end-user-action signal is emitted at the end of a single user-visible operation on the GtkTextBuffer.
;;; 
;;; See also: gtk_text_buffer_end_user_action(), gtk_text_buffer_insert_interactive(), gtk_text_buffer_insert_range_interactive(), gtk_text_buffer_delete_interactive(), gtk_text_buffer_backspace(), gtk_text_buffer_delete_selection(), gtk_text_buffer_backspace().
;;; 
;;; textbuffer :
;;; 	the object which received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; The "insert-child-anchor" signal
;;; 
;;; void                user_function                      (GtkTextBuffer      *textbuffer,
;;;                                                         GtkTextIter        *location,
;;;                                                         GtkTextChildAnchor *anchor,
;;;                                                         gpointer            user_data)       : Run Last
;;; 
;;; The ::insert-child-anchor signal is emitted to insert a GtkTextChildAnchor in a GtkTextBuffer. Insertion actually occurs in the default handler.
;;; 
;;; Note that if your handler runs before the default handler it must not invalidate the location iter (or has to revalidate it). The default signal handler revalidates it to be placed after the inserted anchor.
;;; 
;;; See also: gtk_text_buffer_insert_child_anchor().
;;; 
;;; textbuffer :
;;; 	the object which received the signal
;;; 
;;; location :
;;; 	position to insert anchor in textbuffer
;;; 
;;; anchor :
;;; 	the GtkTextChildAnchor to be inserted
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; The "insert-pixbuf" signal
;;; 
;;; void                user_function                      (GtkTextBuffer *textbuffer,
;;;                                                         GtkTextIter   *location,
;;;                                                         GdkPixbuf     *pixbuf,
;;;                                                         gpointer       user_data)       : Run Last
;;; 
;;; The ::insert-pixbuf signal is emitted to insert a GdkPixbuf in a GtkTextBuffer. Insertion actually occurs in the default handler.
;;; 
;;; Note that if your handler runs before the default handler it must not invalidate the location iter (or has to revalidate it). The default signal handler revalidates it to be placed after the inserted pixbuf.
;;; 
;;; See also: gtk_text_buffer_insert_pixbuf().
;;; 
;;; textbuffer :
;;; 	the object which received the signal
;;; 
;;; location :
;;; 	position to insert pixbuf in textbuffer
;;; 
;;; pixbuf :
;;; 	the GdkPixbuf to be inserted
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; The "insert-text" signal
;;; 
;;; void                user_function                      (GtkTextBuffer *textbuffer,
;;;                                                         GtkTextIter   *location,
;;;                                                         gchar         *text,
;;;                                                         gint           len,
;;;                                                         gpointer       user_data)       : Run Last
;;; 
;;; The ::insert-text signal is emitted to insert text in a GtkTextBuffer. Insertion actually occurs in the default handler.
;;; 
;;; Note that if your handler runs before the default handler it must not invalidate the location iter (or has to revalidate it). The default signal handler revalidates it to point to the end of the inserted text.
;;; 
;;; See also: gtk_text_buffer_insert(), gtk_text_buffer_insert_range().
;;; 
;;; textbuffer :
;;; 	the object which received the signal
;;; 
;;; location :
;;; 	position to insert text in textbuffer
;;; 
;;; text :
;;; 	the UTF-8 text to be inserted
;;; 
;;; len :
;;; 	length of the inserted text in bytes
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; The "mark-deleted" signal
;;; 
;;; void                user_function                      (GtkTextBuffer *textbuffer,
;;;                                                         GtkTextMark   *mark,
;;;                                                         gpointer       user_data)       : Run Last
;;; 
;;; The ::mark-deleted signal is emitted as notification after a GtkTextMark is deleted.
;;; 
;;; See also: gtk_text_buffer_delete_mark().
;;; 
;;; textbuffer :
;;; 	the object which received the signal
;;; 
;;; mark :
;;; 	The mark that was deleted
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; The "mark-set" signal
;;; 
;;; void                user_function                      (GtkTextBuffer *textbuffer,
;;;                                                         GtkTextIter   *location,
;;;                                                         GtkTextMark   *mark,
;;;                                                         gpointer       user_data)       : Run Last
;;; 
;;; The ::mark-set signal is emitted as notification after a GtkTextMark is set.
;;; 
;;; See also: gtk_text_buffer_create_mark(), gtk_text_buffer_move_mark().
;;; 
;;; textbuffer :
;;; 	the object which received the signal
;;; 
;;; location :
;;; 	The location of mark in textbuffer
;;; 
;;; mark :
;;; 	The mark that is set
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; The "modified-changed" signal
;;; 
;;; void                user_function                      (GtkTextBuffer *textbuffer,
;;;                                                         gpointer       user_data)       : Run Last
;;; 
;;; The ::modified-changed signal is emitted when the modified bit of a GtkTextBuffer flips.
;;; 
;;; See also: gtk_text_buffer_set_modified().
;;; 
;;; textbuffer :
;;; 	the object which received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; The "paste-done" signal
;;; 
;;; void                user_function                      (GtkTextBuffer *textbuffer,
;;;                                                         GtkClipboard  *arg1,
;;;                                                         gpointer       user_data)       : Run Last
;;; 
;;; The paste-done signal is emitted after paste operation has been completed. This is useful to properly scroll the view to the end of the pasted text. See gtk_text_buffer_paste_clipboard() for more details.
;;; 
;;; textbuffer :
;;; 	the object which received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Since 2.16
;;; The "remove-tag" signal
;;; 
;;; void                user_function                      (GtkTextBuffer *textbuffer,
;;;                                                         GtkTextTag    *tag,
;;;                                                         GtkTextIter   *start,
;;;                                                         GtkTextIter   *end,
;;;                                                         gpointer       user_data)       : Run Last
;;; 
;;; The ::remove-tag signal is emitted to remove all occurrences of tag from a range of text in a GtkTextBuffer. Removal actually occurs in the default handler.
;;; 
;;; Note that if your handler runs before the default handler it must not invalidate the start and end iters (or has to revalidate them).
;;; 
;;; See also: gtk_text_buffer_remove_tag().
;;; 
;;; textbuffer :
;;; 	the object which received the signal
;;; 
;;; tag :
;;; 	the tag to be removed
;;; 
;;; start :
;;; 	the start of the range the tag is removed from
;;; 
;;; end :
;;; 	the end of the range the tag is removed from
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; See Also
;;; GtkTextView, GtkTextIter, GtkTextMark
;;; 
;;; 
