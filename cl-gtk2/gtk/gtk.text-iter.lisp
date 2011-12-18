;;; ----------------------------------------------------------------------------
;;; gtk.text-iter.lisp
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
;;; GtkTextIter
;;; 
;;; Text buffer iterator
;;; 	
;;; Synopsis
;;; 
;;;                     GtkTextIter;
;;; GtkTextBuffer *     gtk_text_iter_get_buffer            (const GtkTextIter *iter);
;;; GtkTextIter *       gtk_text_iter_copy                  (const GtkTextIter *iter);
;;; void                gtk_text_iter_assign                (GtkTextIter *iter,
;;;                                                          const GtkTextIter *other);
;;; void                gtk_text_iter_free                  (GtkTextIter *iter);
;;; gint                gtk_text_iter_get_offset            (const GtkTextIter *iter);
;;; gint                gtk_text_iter_get_line              (const GtkTextIter *iter);
;;; gint                gtk_text_iter_get_line_offset       (const GtkTextIter *iter);
;;; gint                gtk_text_iter_get_line_index        (const GtkTextIter *iter);
;;; gint                gtk_text_iter_get_visible_line_index
;;;                                                         (const GtkTextIter *iter);
;;; gint                gtk_text_iter_get_visible_line_offset
;;;                                                         (const GtkTextIter *iter);
;;; gunichar            gtk_text_iter_get_char              (const GtkTextIter *iter);
;;; gchar *             gtk_text_iter_get_slice             (const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; gchar *             gtk_text_iter_get_text              (const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; gchar *             gtk_text_iter_get_visible_slice     (const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; gchar *             gtk_text_iter_get_visible_text      (const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; GdkPixbuf *         gtk_text_iter_get_pixbuf            (const GtkTextIter *iter);
;;; GSList *            gtk_text_iter_get_marks             (const GtkTextIter *iter);
;;; GSList *            gtk_text_iter_get_toggled_tags      (const GtkTextIter *iter,
;;;                                                          gboolean toggled_on);
;;; GtkTextChildAnchor * gtk_text_iter_get_child_anchor     (const GtkTextIter *iter);
;;; gboolean            gtk_text_iter_begins_tag            (const GtkTextIter *iter,
;;;                                                          GtkTextTag *tag);
;;; gboolean            gtk_text_iter_ends_tag              (const GtkTextIter *iter,
;;;                                                          GtkTextTag *tag);
;;; gboolean            gtk_text_iter_toggles_tag           (const GtkTextIter *iter,
;;;                                                          GtkTextTag *tag);
;;; gboolean            gtk_text_iter_has_tag               (const GtkTextIter *iter,
;;;                                                          GtkTextTag *tag);
;;; GSList *            gtk_text_iter_get_tags              (const GtkTextIter *iter);
;;; gboolean            gtk_text_iter_editable              (const GtkTextIter *iter,
;;;                                                          gboolean default_setting);
;;; gboolean            gtk_text_iter_can_insert            (const GtkTextIter *iter,
;;;                                                          gboolean default_editability);
;;; gboolean            gtk_text_iter_starts_word           (const GtkTextIter *iter);
;;; gboolean            gtk_text_iter_ends_word             (const GtkTextIter *iter);
;;; gboolean            gtk_text_iter_inside_word           (const GtkTextIter *iter);
;;; gboolean            gtk_text_iter_starts_line           (const GtkTextIter *iter);
;;; gboolean            gtk_text_iter_ends_line             (const GtkTextIter *iter);
;;; gboolean            gtk_text_iter_starts_sentence       (const GtkTextIter *iter);
;;; gboolean            gtk_text_iter_ends_sentence         (const GtkTextIter *iter);
;;; gboolean            gtk_text_iter_inside_sentence       (const GtkTextIter *iter);
;;; gboolean            gtk_text_iter_is_cursor_position    (const GtkTextIter *iter);
;;; gint                gtk_text_iter_get_chars_in_line     (const GtkTextIter *iter);
;;; gint                gtk_text_iter_get_bytes_in_line     (const GtkTextIter *iter);
;;; gboolean            gtk_text_iter_get_attributes        (const GtkTextIter *iter,
;;;                                                          GtkTextAttributes *values);
;;; PangoLanguage *     gtk_text_iter_get_language          (const GtkTextIter *iter);
;;; gboolean            gtk_text_iter_is_end                (const GtkTextIter *iter);
;;; gboolean            gtk_text_iter_is_start              (const GtkTextIter *iter);
;;; gboolean            gtk_text_iter_forward_char          (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_backward_char         (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_forward_chars         (GtkTextIter *iter,
;;;                                                          gint count);
;;; gboolean            gtk_text_iter_backward_chars        (GtkTextIter *iter,
;;;                                                          gint count);
;;; gboolean            gtk_text_iter_forward_line          (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_backward_line         (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_forward_lines         (GtkTextIter *iter,
;;;                                                          gint count);
;;; gboolean            gtk_text_iter_backward_lines        (GtkTextIter *iter,
;;;                                                          gint count);
;;; gboolean            gtk_text_iter_forward_word_ends     (GtkTextIter *iter,
;;;                                                          gint count);
;;; gboolean            gtk_text_iter_backward_word_starts  (GtkTextIter *iter,
;;;                                                          gint count);
;;; gboolean            gtk_text_iter_forward_word_end      (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_backward_word_start   (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_forward_cursor_position
;;;                                                         (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_backward_cursor_position
;;;                                                         (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_forward_cursor_positions
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; gboolean            gtk_text_iter_backward_cursor_positions
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; gboolean            gtk_text_iter_backward_sentence_start
;;;                                                         (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_backward_sentence_starts
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; gboolean            gtk_text_iter_forward_sentence_end  (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_forward_sentence_ends (GtkTextIter *iter,
;;;                                                          gint count);
;;; gboolean            gtk_text_iter_forward_visible_word_ends
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; gboolean            gtk_text_iter_backward_visible_word_starts
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; gboolean            gtk_text_iter_forward_visible_word_end
;;;                                                         (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_backward_visible_word_start
;;;                                                         (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_forward_visible_cursor_position
;;;                                                         (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_backward_visible_cursor_position
;;;                                                         (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_forward_visible_cursor_positions
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; gboolean            gtk_text_iter_backward_visible_cursor_positions
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; gboolean            gtk_text_iter_forward_visible_line  (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_backward_visible_line (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_forward_visible_lines (GtkTextIter *iter,
;;;                                                          gint count);
;;; gboolean            gtk_text_iter_backward_visible_lines
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; void                gtk_text_iter_set_offset            (GtkTextIter *iter,
;;;                                                          gint char_offset);
;;; void                gtk_text_iter_set_line              (GtkTextIter *iter,
;;;                                                          gint line_number);
;;; void                gtk_text_iter_set_line_offset       (GtkTextIter *iter,
;;;                                                          gint char_on_line);
;;; void                gtk_text_iter_set_line_index        (GtkTextIter *iter,
;;;                                                          gint byte_on_line);
;;; void                gtk_text_iter_set_visible_line_index
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint byte_on_line);
;;; void                gtk_text_iter_set_visible_line_offset
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint char_on_line);
;;; void                gtk_text_iter_forward_to_end        (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_forward_to_line_end   (GtkTextIter *iter);
;;; gboolean            gtk_text_iter_forward_to_tag_toggle (GtkTextIter *iter,
;;;                                                          GtkTextTag *tag);
;;; gboolean            gtk_text_iter_backward_to_tag_toggle
;;;                                                         (GtkTextIter *iter,
;;;                                                          GtkTextTag *tag);
;;; gboolean            (*GtkTextCharPredicate)             (gunichar ch,
;;;                                                          gpointer user_data);
;;; gboolean            gtk_text_iter_forward_find_char     (GtkTextIter *iter,
;;;                                                          GtkTextCharPredicate pred,
;;;                                                          gpointer user_data,
;;;                                                          const GtkTextIter *limit);
;;; gboolean            gtk_text_iter_backward_find_char    (GtkTextIter *iter,
;;;                                                          GtkTextCharPredicate pred,
;;;                                                          gpointer user_data,
;;;                                                          const GtkTextIter *limit);
;;; enum                GtkTextSearchFlags;
;;; gboolean            gtk_text_iter_forward_search        (const GtkTextIter *iter,
;;;                                                          const gchar *str,
;;;                                                          GtkTextSearchFlags flags,
;;;                                                          GtkTextIter *match_start,
;;;                                                          GtkTextIter *match_end,
;;;                                                          const GtkTextIter *limit);
;;; gboolean            gtk_text_iter_backward_search       (const GtkTextIter *iter,
;;;                                                          const gchar *str,
;;;                                                          GtkTextSearchFlags flags,
;;;                                                          GtkTextIter *match_start,
;;;                                                          GtkTextIter *match_end,
;;;                                                          const GtkTextIter *limit);
;;; gboolean            gtk_text_iter_equal                 (const GtkTextIter *lhs,
;;;                                                          const GtkTextIter *rhs);
;;; gint                gtk_text_iter_compare               (const GtkTextIter *lhs,
;;;                                                          const GtkTextIter *rhs);
;;; gboolean            gtk_text_iter_in_range              (const GtkTextIter *iter,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; void                gtk_text_iter_order                 (GtkTextIter *first,
;;;                                                          GtkTextIter *second);
;;; 
;;; Object Hierarchy
;;; 
;;;   GBoxed
;;;    +----GtkTextIter
;;; 
;;; Description
;;; 
;;; You may wish to begin by reading the text widget conceptual overview which gives an overview of all the objects and data types related to the text widget and how they work together.
;;; Details
;;; GtkTextIter
;;; 
;;; typedef struct {
;;;   /* GtkTextIter is an opaque datatype; ignore all these fields.
;;;    * Initialize the iter with gtk_text_buffer_get_iter_*
;;;    * functions
;;;    */
;;; } GtkTextIter;
;;; 
;;; gtk_text_iter_get_buffer ()
;;; 
;;; GtkTextBuffer *     gtk_text_iter_get_buffer            (const GtkTextIter *iter);
;;; 
;;; Returns the GtkTextBuffer this iterator is associated with.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	the buffer. [transfer none]
;;; gtk_text_iter_copy ()
;;; 
;;; GtkTextIter *       gtk_text_iter_copy                  (const GtkTextIter *iter);
;;; 
;;; Creates a dynamically-allocated copy of an iterator. This function is not useful in applications, because iterators can be copied with a simple assignment (GtkTextIter i = j;). The function is used by language bindings.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	a copy of the iter, free with gtk_text_iter_free()
;;; gtk_text_iter_assign ()
;;; 
;;; void                gtk_text_iter_assign                (GtkTextIter *iter,
;;;                                                          const GtkTextIter *other);
;;; 
;;; Assigns the value of other to iter. This function is not useful in applications, because iterators can be assigned with GtkTextIter i = j;. The function is used by language bindings.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; other :
;;; 	another GtkTextIter
;;; 
;;; Since 3.2
;;; gtk_text_iter_free ()
;;; 
;;; void                gtk_text_iter_free                  (GtkTextIter *iter);
;;; 
;;; Free an iterator allocated on the heap. This function is intended for use in language bindings, and is not especially useful for applications, because iterators can simply be allocated on the stack.
;;; 
;;; iter :
;;; 	a dynamically-allocated iterator
;;; gtk_text_iter_get_offset ()
;;; 
;;; gint                gtk_text_iter_get_offset            (const GtkTextIter *iter);
;;; 
;;; Returns the character offset of an iterator. Each character in a GtkTextBuffer has an offset, starting with 0 for the first character in the buffer. Use gtk_text_buffer_get_iter_at_offset() to convert an offset back into an iterator.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	a character offset
;;; gtk_text_iter_get_line ()
;;; 
;;; gint                gtk_text_iter_get_line              (const GtkTextIter *iter);
;;; 
;;; Returns the line number containing the iterator. Lines in a GtkTextBuffer are numbered beginning with 0 for the first line in the buffer.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	a line number
;;; gtk_text_iter_get_line_offset ()
;;; 
;;; gint                gtk_text_iter_get_line_offset       (const GtkTextIter *iter);
;;; 
;;; Returns the character offset of the iterator, counting from the start of a newline-terminated line. The first character on the line has offset 0.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	offset from start of line
;;; gtk_text_iter_get_line_index ()
;;; 
;;; gint                gtk_text_iter_get_line_index        (const GtkTextIter *iter);
;;; 
;;; Returns the byte index of the iterator, counting from the start of a newline-terminated line. Remember that GtkTextBuffer encodes text in UTF-8, and that characters can require a variable number of bytes to represent.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	distance from start of line, in bytes
;;; gtk_text_iter_get_visible_line_index ()
;;; 
;;; gint                gtk_text_iter_get_visible_line_index
;;;                                                         (const GtkTextIter *iter);
;;; 
;;; Returns the number of bytes from the start of the line to the given iter, not counting bytes that are invisible due to tags with the "invisible" flag toggled on.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	byte index of iter with respect to the start of the line
;;; gtk_text_iter_get_visible_line_offset ()
;;; 
;;; gint                gtk_text_iter_get_visible_line_offset
;;;                                                         (const GtkTextIter *iter);
;;; 
;;; Returns the offset in characters from the start of the line to the given iter, not counting characters that are invisible due to tags with the "invisible" flag toggled on.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	offset in visible characters from the start of the line
;;; gtk_text_iter_get_char ()
;;; 
;;; gunichar            gtk_text_iter_get_char              (const GtkTextIter *iter);
;;; 
;;; returns 0.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	a Unicode character, or 0 if iter is not dereferenceable
;;; gtk_text_iter_get_slice ()
;;; 
;;; gchar *             gtk_text_iter_get_slice             (const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; 
;;; Returns the text in the given range. A "slice" is an array of characters encoded in UTF-8 format, including the Unicode "unknown" character 0xFFFC for iterable non-character elements in the buffer, such as images. Because images are encoded in the slice, byte and character offsets in the returned array will correspond to byte offsets in the text buffer. Note that 0xFFFC can occur in normal text as well, so it is not a reliable indicator that a pixbuf or widget is in the buffer.
;;; 
;;; start :
;;; 	iterator at start of a range
;;; 
;;; end :
;;; 	iterator at end of a range
;;; 
;;; Returns :
;;; 	slice of text from the buffer
;;; gtk_text_iter_get_text ()
;;; 
;;; gchar *             gtk_text_iter_get_text              (const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; 
;;; Returns text in the given range. If the range contains non-text elements such as images, the character and byte offsets in the returned string will not correspond to character and byte offsets in the buffer. If you want offsets to correspond, see gtk_text_iter_get_slice().
;;; 
;;; start :
;;; 	iterator at start of a range
;;; 
;;; end :
;;; 	iterator at end of a range
;;; 
;;; Returns :
;;; 	array of characters from the buffer
;;; gtk_text_iter_get_visible_slice ()
;;; 
;;; gchar *             gtk_text_iter_get_visible_slice     (const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; 
;;; Like gtk_text_iter_get_slice(), but invisible text is not included. Invisible text is usually invisible because a GtkTextTag with the "invisible" attribute turned on has been applied to it.
;;; 
;;; start :
;;; 	iterator at start of range
;;; 
;;; end :
;;; 	iterator at end of range
;;; 
;;; Returns :
;;; 	slice of text from the buffer
;;; gtk_text_iter_get_visible_text ()
;;; 
;;; gchar *             gtk_text_iter_get_visible_text      (const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; 
;;; Like gtk_text_iter_get_text(), but invisible text is not included. Invisible text is usually invisible because a GtkTextTag with the "invisible" attribute turned on has been applied to it.
;;; 
;;; start :
;;; 	iterator at start of range
;;; 
;;; end :
;;; 	iterator at end of range
;;; 
;;; Returns :
;;; 	string containing visible text in the range
;;; gtk_text_iter_get_pixbuf ()
;;; 
;;; GdkPixbuf *         gtk_text_iter_get_pixbuf            (const GtkTextIter *iter);
;;; 
;;; If the element at iter is a pixbuf, the pixbuf is returned (with no new reference count added). Otherwise, NULL is returned.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	the pixbuf at iter. [transfer none]
;;; gtk_text_iter_get_marks ()
;;; 
;;; GSList *            gtk_text_iter_get_marks             (const GtkTextIter *iter);
;;; 
;;; Returns a list of all GtkTextMark at this location. Because marks are not iterable (they don't take up any "space" in the buffer, they are just marks in between iterable locations), multiple marks can exist in the same place. The returned list is not in any meaningful order.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	list of GtkTextMark. [element-type GtkTextMark][transfer container]
;;; gtk_text_iter_get_toggled_tags ()
;;; 
;;; GSList *            gtk_text_iter_get_toggled_tags      (const GtkTextIter *iter,
;;;                                                          gboolean toggled_on);
;;; 
;;; Returns a list of GtkTextTag that are toggled on or off at this point. (If toggled_on is TRUE, the list contains tags that are toggled on.) If a tag is toggled on at iter, then some non-empty range of characters following iter has that tag applied to it. If a tag is toggled off, then some non-empty range following iter does not have the tag applied to it.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; toggled_on :
;;; 	TRUE to get toggled-on tags
;;; 
;;; Returns :
;;; 	tags toggled at this point. [element-type GtkTextTag][transfer container]
;;; gtk_text_iter_get_child_anchor ()
;;; 
;;; GtkTextChildAnchor * gtk_text_iter_get_child_anchor     (const GtkTextIter *iter);
;;; 
;;; If the location at iter contains a child anchor, the anchor is returned (with no new reference count added). Otherwise, NULL is returned.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	the anchor at iter. [transfer none]
;;; gtk_text_iter_begins_tag ()
;;; 
;;; gboolean            gtk_text_iter_begins_tag            (const GtkTextIter *iter,
;;;                                                          GtkTextTag *tag);
;;; 
;;; Returns TRUE if tag is toggled on at exactly this point. If tag is NULL, returns TRUE if any tag is toggled on at this point. Note that the gtk_text_iter_begins_tag() returns TRUE if iter is the start of the tagged range; gtk_text_iter_has_tag() tells you whether an iterator is within a tagged range.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; tag :
;;; 	a GtkTextTag, or NULL. [allow-none]
;;; 
;;; Returns :
;;; 	whether iter is the start of a range tagged with tag
;;; gtk_text_iter_ends_tag ()
;;; 
;;; gboolean            gtk_text_iter_ends_tag              (const GtkTextIter *iter,
;;;                                                          GtkTextTag *tag);
;;; 
;;; Returns TRUE if tag is toggled off at exactly this point. If tag is NULL, returns TRUE if any tag is toggled off at this point. Note that the gtk_text_iter_ends_tag() returns TRUE if iter is the end of the tagged range; gtk_text_iter_has_tag() tells you whether an iterator is within a tagged range.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; tag :
;;; 	a GtkTextTag, or NULL. [allow-none]
;;; 
;;; Returns :
;;; 	whether iter is the end of a range tagged with tag
;;; gtk_text_iter_toggles_tag ()
;;; 
;;; gboolean            gtk_text_iter_toggles_tag           (const GtkTextIter *iter,
;;;                                                          GtkTextTag *tag);
;;; 
;;; This is equivalent to (gtk_text_iter_begins_tag() || gtk_text_iter_ends_tag()), i.e. it tells you whether a range with tag applied to it begins or ends at iter.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; tag :
;;; 	a GtkTextTag, or NULL. [allow-none]
;;; 
;;; Returns :
;;; 	whether tag is toggled on or off at iter
;;; gtk_text_iter_has_tag ()
;;; 
;;; gboolean            gtk_text_iter_has_tag               (const GtkTextIter *iter,
;;;                                                          GtkTextTag *tag);
;;; 
;;; Returns TRUE if iter is within a range tagged with tag.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; tag :
;;; 	a GtkTextTag
;;; 
;;; Returns :
;;; 	whether iter is tagged with tag
;;; gtk_text_iter_get_tags ()
;;; 
;;; GSList *            gtk_text_iter_get_tags              (const GtkTextIter *iter);
;;; 
;;; Returns a list of tags that apply to iter, in ascending order of priority (highest-priority tags are last). The GtkTextTag in the list don't have a reference added, but you have to free the list itself.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	list of GtkTextTag. [element-type GtkTextTag][transfer container]
;;; gtk_text_iter_editable ()
;;; 
;;; gboolean            gtk_text_iter_editable              (const GtkTextIter *iter,
;;;                                                          gboolean default_setting);
;;; 
;;; Returns whether the character at iter is within an editable region of text. Non-editable text is "locked" and can't be changed by the user via GtkTextView. This function is simply a convenience wrapper around gtk_text_iter_get_attributes(). If no tags applied to this text affect editability, default_setting will be returned.
;;; 
;;; You don't want to use this function to decide whether text can be inserted at iter, because for insertion you don't want to know whether the char at iter is inside an editable range, you want to know whether a new character inserted at iter would be inside an editable range. Use gtk_text_iter_can_insert() to handle this case.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; default_setting :
;;; 	TRUE if text is editable by default
;;; 
;;; Returns :
;;; 	whether iter is inside an editable range
;;; gtk_text_iter_can_insert ()
;;; 
;;; gboolean            gtk_text_iter_can_insert            (const GtkTextIter *iter,
;;;                                                          gboolean default_editability);
;;; 
;;; Considering the default editability of the buffer, and tags that affect editability, determines whether text inserted at iter would be editable. If text inserted at iter would be editable then the user should be allowed to insert text at iter. gtk_text_buffer_insert_interactive() uses this function to decide whether insertions are allowed at a given position.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; default_editability :
;;; 	TRUE if text is editable by default
;;; 
;;; Returns :
;;; 	whether text inserted at iter would be editable
;;; gtk_text_iter_starts_word ()
;;; 
;;; gboolean            gtk_text_iter_starts_word           (const GtkTextIter *iter);
;;; 
;;; Determines whether iter begins a natural-language word. Word breaks are determined by Pango and should be correct for nearly any language (if not, the correct fix would be to the Pango word break algorithms).
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if iter is at the start of a word
;;; gtk_text_iter_ends_word ()
;;; 
;;; gboolean            gtk_text_iter_ends_word             (const GtkTextIter *iter);
;;; 
;;; Determines whether iter ends a natural-language word. Word breaks are determined by Pango and should be correct for nearly any language (if not, the correct fix would be to the Pango word break algorithms).
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if iter is at the end of a word
;;; gtk_text_iter_inside_word ()
;;; 
;;; gboolean            gtk_text_iter_inside_word           (const GtkTextIter *iter);
;;; 
;;; Determines whether iter is inside a natural-language word (as opposed to say inside some whitespace). Word breaks are determined by Pango and should be correct for nearly any language (if not, the correct fix would be to the Pango word break algorithms).
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if iter is inside a word
;;; gtk_text_iter_starts_line ()
;;; 
;;; gboolean            gtk_text_iter_starts_line           (const GtkTextIter *iter);
;;; 
;;; Returns TRUE if iter begins a paragraph, i.e. if gtk_text_iter_get_line_offset() would return 0. However this function is potentially more efficient than gtk_text_iter_get_line_offset() because it doesn't have to compute the offset, it just has to see whether it's 0.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	whether iter begins a line
;;; gtk_text_iter_ends_line ()
;;; 
;;; gboolean            gtk_text_iter_ends_line             (const GtkTextIter *iter);
;;; 
;;; Returns TRUE if iter points to the start of the paragraph delimiter characters for a line (delimiters will be either a newline, a carriage return, a carriage return followed by a newline, or a Unicode paragraph separator character). Note that an iterator pointing to the \n of a \r\n pair will not be counted as the end of a line, the line ends before the \r. The end iterator is considered to be at the end of a line, even though there are no paragraph delimiter chars there.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	whether iter is at the end of a line
;;; gtk_text_iter_starts_sentence ()
;;; 
;;; gboolean            gtk_text_iter_starts_sentence       (const GtkTextIter *iter);
;;; 
;;; Determines whether iter begins a sentence. Sentence boundaries are determined by Pango and should be correct for nearly any language (if not, the correct fix would be to the Pango text boundary algorithms).
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if iter is at the start of a sentence.
;;; gtk_text_iter_ends_sentence ()
;;; 
;;; gboolean            gtk_text_iter_ends_sentence         (const GtkTextIter *iter);
;;; 
;;; Determines whether iter ends a sentence. Sentence boundaries are determined by Pango and should be correct for nearly any language (if not, the correct fix would be to the Pango text boundary algorithms).
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if iter is at the end of a sentence.
;;; gtk_text_iter_inside_sentence ()
;;; 
;;; gboolean            gtk_text_iter_inside_sentence       (const GtkTextIter *iter);
;;; 
;;; Determines whether iter is inside a sentence (as opposed to in between two sentences, e.g. after a period and before the first letter of the next sentence). Sentence boundaries are determined by Pango and should be correct for nearly any language (if not, the correct fix would be to the Pango text boundary algorithms).
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if iter is inside a sentence.
;;; gtk_text_iter_is_cursor_position ()
;;; 
;;; gboolean            gtk_text_iter_is_cursor_position    (const GtkTextIter *iter);
;;; 
;;; See gtk_text_iter_forward_cursor_position() or PangoLogAttr or pango_break() for details on what a cursor position is.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if the cursor can be placed at iter
;;; gtk_text_iter_get_chars_in_line ()
;;; 
;;; gint                gtk_text_iter_get_chars_in_line     (const GtkTextIter *iter);
;;; 
;;; Returns the number of characters in the line containing iter, including the paragraph delimiters.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	number of characters in the line
;;; gtk_text_iter_get_bytes_in_line ()
;;; 
;;; gint                gtk_text_iter_get_bytes_in_line     (const GtkTextIter *iter);
;;; 
;;; Returns the number of bytes in the line containing iter, including the paragraph delimiters.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	number of bytes in the line
;;; gtk_text_iter_get_attributes ()
;;; 
;;; gboolean            gtk_text_iter_get_attributes        (const GtkTextIter *iter,
;;;                                                          GtkTextAttributes *values);
;;; 
;;; Computes the effect of any tags applied to this spot in the text. The values parameter should be initialized to the default settings you wish to use if no tags are in effect. You'd typically obtain the defaults from gtk_text_view_get_default_attributes().
;;; 
;;; gtk_text_iter_get_attributes() will modify values, applying the effects of any tags present at iter. If any tags affected values, the function returns TRUE.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; values :
;;; 	a GtkTextAttributes to be filled in. [out]
;;; 
;;; Returns :
;;; 	TRUE if values was modified
;;; gtk_text_iter_get_language ()
;;; 
;;; PangoLanguage *     gtk_text_iter_get_language          (const GtkTextIter *iter);
;;; 
;;; A convenience wrapper around gtk_text_iter_get_attributes(), which returns the language in effect at iter. If no tags affecting language apply to iter, the return value is identical to that of gtk_get_default_language().
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	language in effect at iter
;;; gtk_text_iter_is_end ()
;;; 
;;; gboolean            gtk_text_iter_is_end                (const GtkTextIter *iter);
;;; 
;;; Returns TRUE if iter is the end iterator, i.e. one past the last dereferenceable iterator in the buffer. gtk_text_iter_is_end() is the most efficient way to check whether an iterator is the end iterator.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	whether iter is the end iterator
;;; gtk_text_iter_is_start ()
;;; 
;;; gboolean            gtk_text_iter_is_start              (const GtkTextIter *iter);
;;; 
;;; Returns TRUE if iter is the first iterator in the buffer, that is if iter has a character offset of 0.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	whether iter is the first in the buffer
;;; gtk_text_iter_forward_char ()
;;; 
;;; gboolean            gtk_text_iter_forward_char          (GtkTextIter *iter);
;;; 
;;; Moves iter forward by one character offset. Note that images embedded in the buffer occupy 1 character slot, so gtk_text_iter_forward_char() may actually move onto an image instead of a character, if you have images in your buffer. If iter is the end iterator or one character before it, iter will now point at the end iterator, and gtk_text_iter_forward_char() returns FALSE for convenience when writing loops.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	whether iter moved and is dereferenceable
;;; gtk_text_iter_backward_char ()
;;; 
;;; gboolean            gtk_text_iter_backward_char         (GtkTextIter *iter);
;;; 
;;; Moves backward by one character offset. Returns TRUE if movement was possible; if iter was the first in the buffer (character offset 0), gtk_text_iter_backward_char() returns FALSE for convenience when writing loops.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	whether movement was possible
;;; gtk_text_iter_forward_chars ()
;;; 
;;; gboolean            gtk_text_iter_forward_chars         (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Moves count characters if possible (if count would move past the start or end of the buffer, moves to the start or end of the buffer). The return value indicates whether the new position of iter is different from its original position, and dereferenceable (the last iterator in the buffer is not dereferenceable). If count is 0, the function does nothing and returns FALSE.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; count :
;;; 	number of characters to move, may be negative
;;; 
;;; Returns :
;;; 	whether iter moved and is dereferenceable
;;; gtk_text_iter_backward_chars ()
;;; 
;;; gboolean            gtk_text_iter_backward_chars        (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Moves count characters backward, if possible (if count would move past the start or end of the buffer, moves to the start or end of the buffer). The return value indicates whether the iterator moved onto a dereferenceable position; if the iterator didn't move, or moved onto the end iterator, then FALSE is returned. If count is 0, the function does nothing and returns FALSE.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; count :
;;; 	number of characters to move
;;; 
;;; Returns :
;;; 	whether iter moved and is dereferenceable
;;; gtk_text_iter_forward_line ()
;;; 
;;; gboolean            gtk_text_iter_forward_line          (GtkTextIter *iter);
;;; 
;;; Moves iter to the start of the next line. If the iter is already on the last line of the buffer, moves the iter to the end of the current line. If after the operation, the iter is at the end of the buffer and not dereferencable, returns FALSE. Otherwise, returns TRUE.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	whether iter can be dereferenced
;;; gtk_text_iter_backward_line ()
;;; 
;;; gboolean            gtk_text_iter_backward_line         (GtkTextIter *iter);
;;; 
;;; Moves iter to the start of the previous line. Returns TRUE if iter could be moved; i.e. if iter was at character offset 0, this function returns FALSE. Therefore if iter was already on line 0, but not at the start of the line, iter is snapped to the start of the line and the function returns TRUE. (Note that this implies that in a loop calling this function, the line number may not change on every iteration, if your first iteration is on line 0.)
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	whether iter moved
;;; gtk_text_iter_forward_lines ()
;;; 
;;; gboolean            gtk_text_iter_forward_lines         (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Moves count lines forward, if possible (if count would move past the start or end of the buffer, moves to the start or end of the buffer). The return value indicates whether the iterator moved onto a dereferenceable position; if the iterator didn't move, or moved onto the end iterator, then FALSE is returned. If count is 0, the function does nothing and returns FALSE. If count is negative, moves backward by 0 - count lines.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; count :
;;; 	number of lines to move forward
;;; 
;;; Returns :
;;; 	whether iter moved and is dereferenceable
;;; gtk_text_iter_backward_lines ()
;;; 
;;; gboolean            gtk_text_iter_backward_lines        (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Moves count lines backward, if possible (if count would move past the start or end of the buffer, moves to the start or end of the buffer). The return value indicates whether the iterator moved onto a dereferenceable position; if the iterator didn't move, or moved onto the end iterator, then FALSE is returned. If count is 0, the function does nothing and returns FALSE. If count is negative, moves forward by 0 - count lines.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; count :
;;; 	number of lines to move backward
;;; 
;;; Returns :
;;; 	whether iter moved and is dereferenceable
;;; gtk_text_iter_forward_word_ends ()
;;; 
;;; gboolean            gtk_text_iter_forward_word_ends     (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Calls gtk_text_iter_forward_word_end() up to count times.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; count :
;;; 	number of times to move
;;; 
;;; Returns :
;;; 	TRUE if iter moved and is not the end iterator
;;; gtk_text_iter_backward_word_starts ()
;;; 
;;; gboolean            gtk_text_iter_backward_word_starts  (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Calls gtk_text_iter_backward_word_start() up to count times.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; count :
;;; 	number of times to move
;;; 
;;; Returns :
;;; 	TRUE if iter moved and is not the end iterator
;;; gtk_text_iter_forward_word_end ()
;;; 
;;; gboolean            gtk_text_iter_forward_word_end      (GtkTextIter *iter);
;;; 
;;; Moves forward to the next word end. (If iter is currently on a word end, moves forward to the next one after that.) Word breaks are determined by Pango and should be correct for nearly any language (if not, the correct fix would be to the Pango word break algorithms).
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if iter moved and is not the end iterator
;;; gtk_text_iter_backward_word_start ()
;;; 
;;; gboolean            gtk_text_iter_backward_word_start   (GtkTextIter *iter);
;;; 
;;; Moves backward to the previous word start. (If iter is currently on a word start, moves backward to the next one after that.) Word breaks are determined by Pango and should be correct for nearly any language (if not, the correct fix would be to the Pango word break algorithms).
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if iter moved and is not the end iterator
;;; gtk_text_iter_forward_cursor_position ()
;;; 
;;; gboolean            gtk_text_iter_forward_cursor_position
;;;                                                         (GtkTextIter *iter);
;;; 
;;; Moves iter forward by a single cursor position. Cursor positions are (unsurprisingly) positions where the cursor can appear. Perhaps surprisingly, there may not be a cursor position between all characters. The most common example for European languages would be a carriage return/newline sequence. For some Unicode characters, the equivalent of say the letter "a" with an accent mark will be represented as two characters, first the letter then a "combining mark" that causes the accent to be rendered; so the cursor can't go between those two characters. See also the PangoLogAttr structure and pango_break() function.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if we moved and the new position is dereferenceable
;;; gtk_text_iter_backward_cursor_position ()
;;; 
;;; gboolean            gtk_text_iter_backward_cursor_position
;;;                                                         (GtkTextIter *iter);
;;; 
;;; Like gtk_text_iter_forward_cursor_position(), but moves backward.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if we moved
;;; gtk_text_iter_forward_cursor_positions ()
;;; 
;;; gboolean            gtk_text_iter_forward_cursor_positions
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Moves up to count cursor positions. See gtk_text_iter_forward_cursor_position() for details.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; count :
;;; 	number of positions to move
;;; 
;;; Returns :
;;; 	TRUE if we moved and the new position is dereferenceable
;;; gtk_text_iter_backward_cursor_positions ()
;;; 
;;; gboolean            gtk_text_iter_backward_cursor_positions
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Moves up to count cursor positions. See gtk_text_iter_forward_cursor_position() for details.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; count :
;;; 	number of positions to move
;;; 
;;; Returns :
;;; 	TRUE if we moved and the new position is dereferenceable
;;; gtk_text_iter_backward_sentence_start ()
;;; 
;;; gboolean            gtk_text_iter_backward_sentence_start
;;;                                                         (GtkTextIter *iter);
;;; 
;;; Moves backward to the previous sentence start; if iter is already at the start of a sentence, moves backward to the next one. Sentence boundaries are determined by Pango and should be correct for nearly any language (if not, the correct fix would be to the Pango text boundary algorithms).
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if iter moved and is not the end iterator
;;; gtk_text_iter_backward_sentence_starts ()
;;; 
;;; gboolean            gtk_text_iter_backward_sentence_starts
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Calls gtk_text_iter_backward_sentence_start() up to count times, or until it returns FALSE. If count is negative, moves forward instead of backward.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; count :
;;; 	number of sentences to move
;;; 
;;; Returns :
;;; 	TRUE if iter moved and is not the end iterator
;;; gtk_text_iter_forward_sentence_end ()
;;; 
;;; gboolean            gtk_text_iter_forward_sentence_end  (GtkTextIter *iter);
;;; 
;;; Moves forward to the next sentence end. (If iter is at the end of a sentence, moves to the next end of sentence.) Sentence boundaries are determined by Pango and should be correct for nearly any language (if not, the correct fix would be to the Pango text boundary algorithms).
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if iter moved and is not the end iterator
;;; gtk_text_iter_forward_sentence_ends ()
;;; 
;;; gboolean            gtk_text_iter_forward_sentence_ends (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Calls gtk_text_iter_forward_sentence_end() count times (or until gtk_text_iter_forward_sentence_end() returns FALSE). If count is negative, moves backward instead of forward.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; count :
;;; 	number of sentences to move
;;; 
;;; Returns :
;;; 	TRUE if iter moved and is not the end iterator
;;; gtk_text_iter_forward_visible_word_ends ()
;;; 
;;; gboolean            gtk_text_iter_forward_visible_word_ends
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Calls gtk_text_iter_forward_visible_word_end() up to count times.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; count :
;;; 	number of times to move
;;; 
;;; Returns :
;;; 	TRUE if iter moved and is not the end iterator
;;; 
;;; Since 2.4
;;; gtk_text_iter_backward_visible_word_starts ()
;;; 
;;; gboolean            gtk_text_iter_backward_visible_word_starts
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Calls gtk_text_iter_backward_visible_word_start() up to count times.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; count :
;;; 	number of times to move
;;; 
;;; Returns :
;;; 	TRUE if iter moved and is not the end iterator
;;; 
;;; Since 2.4
;;; gtk_text_iter_forward_visible_word_end ()
;;; 
;;; gboolean            gtk_text_iter_forward_visible_word_end
;;;                                                         (GtkTextIter *iter);
;;; 
;;; Moves forward to the next visible word end. (If iter is currently on a word end, moves forward to the next one after that.) Word breaks are determined by Pango and should be correct for nearly any language (if not, the correct fix would be to the Pango word break algorithms).
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if iter moved and is not the end iterator
;;; 
;;; Since 2.4
;;; gtk_text_iter_backward_visible_word_start ()
;;; 
;;; gboolean            gtk_text_iter_backward_visible_word_start
;;;                                                         (GtkTextIter *iter);
;;; 
;;; Moves backward to the previous visible word start. (If iter is currently on a word start, moves backward to the next one after that.) Word breaks are determined by Pango and should be correct for nearly any language (if not, the correct fix would be to the Pango word break algorithms).
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if iter moved and is not the end iterator
;;; 
;;; Since 2.4
;;; gtk_text_iter_forward_visible_cursor_position ()
;;; 
;;; gboolean            gtk_text_iter_forward_visible_cursor_position
;;;                                                         (GtkTextIter *iter);
;;; 
;;; Moves iter forward to the next visible cursor position. See gtk_text_iter_forward_cursor_position() for details.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if we moved and the new position is dereferenceable
;;; 
;;; Since 2.4
;;; gtk_text_iter_backward_visible_cursor_position ()
;;; 
;;; gboolean            gtk_text_iter_backward_visible_cursor_position
;;;                                                         (GtkTextIter *iter);
;;; 
;;; Moves iter forward to the previous visible cursor position. See gtk_text_iter_backward_cursor_position() for details.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if we moved and the new position is dereferenceable
;;; 
;;; Since 2.4
;;; gtk_text_iter_forward_visible_cursor_positions ()
;;; 
;;; gboolean            gtk_text_iter_forward_visible_cursor_positions
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Moves up to count visible cursor positions. See gtk_text_iter_forward_cursor_position() for details.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; count :
;;; 	number of positions to move
;;; 
;;; Returns :
;;; 	TRUE if we moved and the new position is dereferenceable
;;; 
;;; Since 2.4
;;; gtk_text_iter_backward_visible_cursor_positions ()
;;; 
;;; gboolean            gtk_text_iter_backward_visible_cursor_positions
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Moves up to count visible cursor positions. See gtk_text_iter_backward_cursor_position() for details.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; count :
;;; 	number of positions to move
;;; 
;;; Returns :
;;; 	TRUE if we moved and the new position is dereferenceable
;;; 
;;; Since 2.4
;;; gtk_text_iter_forward_visible_line ()
;;; 
;;; gboolean            gtk_text_iter_forward_visible_line  (GtkTextIter *iter);
;;; 
;;; Moves iter to the start of the next visible line. Returns TRUE if there was a next line to move to, and FALSE if iter was simply moved to the end of the buffer and is now not dereferenceable, or if iter was already at the end of the buffer.
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	whether iter can be dereferenced
;;; 
;;; Since 2.8
;;; gtk_text_iter_backward_visible_line ()
;;; 
;;; gboolean            gtk_text_iter_backward_visible_line (GtkTextIter *iter);
;;; 
;;; Moves iter to the start of the previous visible line. Returns TRUE if iter could be moved; i.e. if iter was at character offset 0, this function returns FALSE. Therefore if iter was already on line 0, but not at the start of the line, iter is snapped to the start of the line and the function returns TRUE. (Note that this implies that in a loop calling this function, the line number may not change on every iteration, if your first iteration is on line 0.)
;;; 
;;; iter :
;;; 	an iterator
;;; 
;;; Returns :
;;; 	whether iter moved
;;; 
;;; Since 2.8
;;; gtk_text_iter_forward_visible_lines ()
;;; 
;;; gboolean            gtk_text_iter_forward_visible_lines (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Moves count visible lines forward, if possible (if count would move past the start or end of the buffer, moves to the start or end of the buffer). The return value indicates whether the iterator moved onto a dereferenceable position; if the iterator didn't move, or moved onto the end iterator, then FALSE is returned. If count is 0, the function does nothing and returns FALSE. If count is negative, moves backward by 0 - count lines.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; count :
;;; 	number of lines to move forward
;;; 
;;; Returns :
;;; 	whether iter moved and is dereferenceable
;;; 
;;; Since 2.8
;;; gtk_text_iter_backward_visible_lines ()
;;; 
;;; gboolean            gtk_text_iter_backward_visible_lines
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint count);
;;; 
;;; Moves count visible lines backward, if possible (if count would move past the start or end of the buffer, moves to the start or end of the buffer). The return value indicates whether the iterator moved onto a dereferenceable position; if the iterator didn't move, or moved onto the end iterator, then FALSE is returned. If count is 0, the function does nothing and returns FALSE. If count is negative, moves forward by 0 - count lines.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; count :
;;; 	number of lines to move backward
;;; 
;;; Returns :
;;; 	whether iter moved and is dereferenceable
;;; 
;;; Since 2.8
;;; gtk_text_iter_set_offset ()
;;; 
;;; void                gtk_text_iter_set_offset            (GtkTextIter *iter,
;;;                                                          gint char_offset);
;;; 
;;; Sets iter to point to char_offset. char_offset counts from the start of the entire text buffer, starting with 0.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; char_offset :
;;; 	a character number
;;; gtk_text_iter_set_line ()
;;; 
;;; void                gtk_text_iter_set_line              (GtkTextIter *iter,
;;;                                                          gint line_number);
;;; 
;;; Moves iterator iter to the start of the line line_number. If line_number is negative or larger than the number of lines in the buffer, moves iter to the start of the last line in the buffer.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; line_number :
;;; 	line number (counted from 0)
;;; gtk_text_iter_set_line_offset ()
;;; 
;;; void                gtk_text_iter_set_line_offset       (GtkTextIter *iter,
;;;                                                          gint char_on_line);
;;; 
;;; Moves iter within a line, to a new character (not byte) offset. The given character offset must be less than or equal to the number of characters in the line; if equal, iter moves to the start of the next line. See gtk_text_iter_set_line_index() if you have a byte index rather than a character offset.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; char_on_line :
;;; 	a character offset relative to the start of iter's current line
;;; gtk_text_iter_set_line_index ()
;;; 
;;; void                gtk_text_iter_set_line_index        (GtkTextIter *iter,
;;;                                                          gint byte_on_line);
;;; 
;;; Same as gtk_text_iter_set_line_offset(), but works with a byte index. The given byte index must be at the start of a character, it can't be in the middle of a UTF-8 encoded character.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; byte_on_line :
;;; 	a byte index relative to the start of iter's current line
;;; gtk_text_iter_set_visible_line_index ()
;;; 
;;; void                gtk_text_iter_set_visible_line_index
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint byte_on_line);
;;; 
;;; Like gtk_text_iter_set_line_index(), but the index is in visible bytes, i.e. text with a tag making it invisible is not counted in the index.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; byte_on_line :
;;; 	a byte index
;;; gtk_text_iter_set_visible_line_offset ()
;;; 
;;; void                gtk_text_iter_set_visible_line_offset
;;;                                                         (GtkTextIter *iter,
;;;                                                          gint char_on_line);
;;; 
;;; Like gtk_text_iter_set_line_offset(), but the offset is in visible characters, i.e. text with a tag making it invisible is not counted in the offset.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; char_on_line :
;;; 	a character offset
;;; gtk_text_iter_forward_to_end ()
;;; 
;;; void                gtk_text_iter_forward_to_end        (GtkTextIter *iter);
;;; 
;;; Moves iter forward to the "end iterator," which points one past the last valid character in the buffer. gtk_text_iter_get_char() called on the end iterator returns 0, which is convenient for writing loops.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; gtk_text_iter_forward_to_line_end ()
;;; 
;;; gboolean            gtk_text_iter_forward_to_line_end   (GtkTextIter *iter);
;;; 
;;; Moves the iterator to point to the paragraph delimiter characters, which will be either a newline, a carriage return, a carriage return/newline in sequence, or the Unicode paragraph separator character. If the iterator is already at the paragraph delimiter characters, moves to the paragraph delimiter characters for the next line. If iter is on the last line in the buffer, which does not end in paragraph delimiters, moves to the end iterator (end of the last line), and returns FALSE.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if we moved and the new location is not the end iterator
;;; gtk_text_iter_forward_to_tag_toggle ()
;;; 
;;; gboolean            gtk_text_iter_forward_to_tag_toggle (GtkTextIter *iter,
;;;                                                          GtkTextTag *tag);
;;; 
;;; Moves forward to the next toggle (on or off) of the GtkTextTag tag, or to the next toggle of any tag if tag is NULL. If no matching tag toggles are found, returns FALSE, otherwise TRUE. Does not return toggles located at iter, only toggles after iter. Sets iter to the location of the toggle, or to the end of the buffer if no toggle is found.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; tag :
;;; 	a GtkTextTag, or NULL. [allow-none]
;;; 
;;; Returns :
;;; 	whether we found a tag toggle after iter
;;; gtk_text_iter_backward_to_tag_toggle ()
;;; 
;;; gboolean            gtk_text_iter_backward_to_tag_toggle
;;;                                                         (GtkTextIter *iter,
;;;                                                          GtkTextTag *tag);
;;; 
;;; Moves backward to the next toggle (on or off) of the GtkTextTag tag, or to the next toggle of any tag if tag is NULL. If no matching tag toggles are found, returns FALSE, otherwise TRUE. Does not return toggles located at iter, only toggles before iter. Sets iter to the location of the toggle, or the start of the buffer if no toggle is found.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; tag :
;;; 	a GtkTextTag, or NULL. [allow-none]
;;; 
;;; Returns :
;;; 	whether we found a tag toggle before iter
;;; GtkTextCharPredicate ()
;;; 
;;; gboolean            (*GtkTextCharPredicate)             (gunichar ch,
;;;                                                          gpointer user_data);
;;; 
;;; gtk_text_iter_forward_find_char ()
;;; 
;;; gboolean            gtk_text_iter_forward_find_char     (GtkTextIter *iter,
;;;                                                          GtkTextCharPredicate pred,
;;;                                                          gpointer user_data,
;;;                                                          const GtkTextIter *limit);
;;; 
;;; Advances iter, calling pred on each character. If pred returns TRUE, returns TRUE and stops scanning. If pred never returns TRUE, iter is set to limit if limit is non-NULL, otherwise to the end iterator.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; pred :
;;; 	a function to be called on each character. [scope call]
;;; 
;;; user_data :
;;; 	user data for pred
;;; 
;;; limit :
;;; 	search limit, or NULL for none. [allow-none]
;;; 
;;; Returns :
;;; 	whether a match was found
;;; gtk_text_iter_backward_find_char ()
;;; 
;;; gboolean            gtk_text_iter_backward_find_char    (GtkTextIter *iter,
;;;                                                          GtkTextCharPredicate pred,
;;;                                                          gpointer user_data,
;;;                                                          const GtkTextIter *limit);
;;; 
;;; Same as gtk_text_iter_forward_find_char(), but goes backward from iter.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; pred :
;;; 	function to be called on each character. [scope call]
;;; 
;;; user_data :
;;; 	user data for pred
;;; 
;;; limit :
;;; 	search limit, or NULL for none. [allow-none]
;;; 
;;; Returns :
;;; 	whether a match was found
;;; enum GtkTextSearchFlags
;;; 
;;; typedef enum {
;;;   GTK_TEXT_SEARCH_VISIBLE_ONLY     = 1 << 0,
;;;   GTK_TEXT_SEARCH_TEXT_ONLY        = 1 << 1,
;;;   GTK_TEXT_SEARCH_CASE_INSENSITIVE = 1 << 2
;;;   /* Possible future plans: SEARCH_REGEXP */
;;; } GtkTextSearchFlags;
;;; 
;;; gtk_text_iter_forward_search ()
;;; 
;;; gboolean            gtk_text_iter_forward_search        (const GtkTextIter *iter,
;;;                                                          const gchar *str,
;;;                                                          GtkTextSearchFlags flags,
;;;                                                          GtkTextIter *match_start,
;;;                                                          GtkTextIter *match_end,
;;;                                                          const GtkTextIter *limit);
;;; 
;;; Searches forward for str. Any match is returned by setting match_start to the first character of the match and match_end to the first character after the match. The search will not continue past limit. Note that a search is a linear or O(n) operation, so you may wish to use limit to avoid locking up your UI on large buffers.
;;; 
;;; If the GTK_TEXT_SEARCH_VISIBLE_ONLY flag is present, the match may have invisible text interspersed in str. i.e. str will be a possibly-noncontiguous subsequence of the matched range. similarly, if you specify GTK_TEXT_SEARCH_TEXT_ONLY, the match may have pixbufs or child widgets mixed inside the matched range. If these flags are not given, the match must be exact; the special 0xFFFC character in str will match embedded pixbufs or child widgets. If you specify the GTK_TEXT_SEARCH_CASE_INSENSITIVE flag, the text will be matched regardless of what case it is in.
;;; 
;;; iter :
;;; 	start of search
;;; 
;;; str :
;;; 	a search string
;;; 
;;; flags :
;;; 	flags affecting how the search is done
;;; 
;;; match_start :
;;; 	return location for start of match, or NULL. [out caller-allocates][allow-none]
;;; 
;;; match_end :
;;; 	return location for end of match, or NULL. [out caller-allocates][allow-none]
;;; 
;;; limit :
;;; 	bound for the search, or NULL for the end of the buffer. [allow-none]
;;; 
;;; Returns :
;;; 	whether a match was found
;;; gtk_text_iter_backward_search ()
;;; 
;;; gboolean            gtk_text_iter_backward_search       (const GtkTextIter *iter,
;;;                                                          const gchar *str,
;;;                                                          GtkTextSearchFlags flags,
;;;                                                          GtkTextIter *match_start,
;;;                                                          GtkTextIter *match_end,
;;;                                                          const GtkTextIter *limit);
;;; 
;;; Same as gtk_text_iter_forward_search(), but moves backward.
;;; 
;;; iter :
;;; 	a GtkTextIter where the search begins
;;; 
;;; str :
;;; 	search string
;;; 
;;; flags :
;;; 	bitmask of flags affecting the search
;;; 
;;; match_start :
;;; 	return location for start of match, or NULL. [out caller-allocates][allow-none]
;;; 
;;; match_end :
;;; 	return location for end of match, or NULL. [out caller-allocates][allow-none]
;;; 
;;; limit :
;;; 	location of last possible match_start, or NULL for start of buffer. [allow-none]
;;; 
;;; Returns :
;;; 	whether a match was found
;;; gtk_text_iter_equal ()
;;; 
;;; gboolean            gtk_text_iter_equal                 (const GtkTextIter *lhs,
;;;                                                          const GtkTextIter *rhs);
;;; 
;;; Tests whether two iterators are equal, using the fastest possible mechanism. This function is very fast; you can expect it to perform better than e.g. getting the character offset for each iterator and comparing the offsets yourself. Also, it's a bit faster than gtk_text_iter_compare().
;;; 
;;; lhs :
;;; 	a GtkTextIter
;;; 
;;; rhs :
;;; 	another GtkTextIter
;;; 
;;; Returns :
;;; 	TRUE if the iterators point to the same place in the buffer
;;; gtk_text_iter_compare ()
;;; 
;;; gint                gtk_text_iter_compare               (const GtkTextIter *lhs,
;;;                                                          const GtkTextIter *rhs);
;;; 
;;; A qsort()-style function that returns negative if lhs is less than rhs, positive if lhs is greater than rhs, and 0 if they're equal. Ordering is in character offset order, i.e. the first character in the buffer is less than the second character in the buffer.
;;; 
;;; lhs :
;;; 	a GtkTextIter
;;; 
;;; rhs :
;;; 	another GtkTextIter
;;; 
;;; Returns :
;;; 	-1 if lhs is less than rhs, 1 if lhs is greater, 0 if they are equal
;;; gtk_text_iter_in_range ()
;;; 
;;; gboolean            gtk_text_iter_in_range              (const GtkTextIter *iter,
;;;                                                          const GtkTextIter *start,
;;;                                                          const GtkTextIter *end);
;;; 
;;; Checks whether iter falls in the range [start, end). start and end must be in ascending order.
;;; 
;;; iter :
;;; 	a GtkTextIter
;;; 
;;; start :
;;; 	start of range
;;; 
;;; end :
;;; 	end of range
;;; 
;;; Returns :
;;; 	TRUE if iter is in the range
;;; gtk_text_iter_order ()
;;; 
;;; void                gtk_text_iter_order                 (GtkTextIter *first,
;;;                                                          GtkTextIter *second);
;;; 
;;; Swaps the value of first and second if second comes before first in the buffer. That is, ensures that first and second are in sequence. Most text buffer functions that take a range call this automatically on your behalf, so there's no real reason to call it yourself in those cases. There are some exceptions, such as gtk_text_iter_in_range(), that expect a pre-sorted range.
;;; 
;;; first :
;;; 	a GtkTextIter
;;; 
;;; second :
;;; 	another GtkTextIter
;;; 
;;; 
