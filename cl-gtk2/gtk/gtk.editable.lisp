;;; ----------------------------------------------------------------------------
;;; gtk.editable.lisp
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
;;; GtkEditable
;;; 
;;; GtkEditable — Interface for text-editing widgets
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;;                     GtkEditable;
;;; void                gtk_editable_select_region          (GtkEditable *editable,
;;;                                                          gint start_pos,
;;;                                                          gint end_pos);
;;; gboolean            gtk_editable_get_selection_bounds   (GtkEditable *editable,
;;;                                                          gint *start_pos,
;;;                                                          gint *end_pos);
;;; void                gtk_editable_insert_text            (GtkEditable *editable,
;;;                                                          const gchar *new_text,
;;;                                                          gint new_text_length,
;;;                                                          gint *position);
;;; void                gtk_editable_delete_text            (GtkEditable *editable,
;;;                                                          gint start_pos,
;;;                                                          gint end_pos);
;;; gchar *             gtk_editable_get_chars              (GtkEditable *editable,
;;;                                                          gint start_pos,
;;;                                                          gint end_pos);
;;; void                gtk_editable_cut_clipboard          (GtkEditable *editable);
;;; void                gtk_editable_copy_clipboard         (GtkEditable *editable);
;;; void                gtk_editable_paste_clipboard        (GtkEditable *editable);
;;; void                gtk_editable_delete_selection       (GtkEditable *editable);
;;; void                gtk_editable_set_position           (GtkEditable *editable,
;;;                                                          gint position);
;;; gint                gtk_editable_get_position           (GtkEditable *editable);
;;; void                gtk_editable_set_editable           (GtkEditable *editable,
;;;                                                          gboolean is_editable);
;;; gboolean            gtk_editable_get_editable           (GtkEditable *editable);
;;; 
;;; Object Hierarchy
;;; 
;;;   GInterface
;;;    +----GtkEditable
;;; 
;;; Known Implementations
;;; 
;;; GtkEditable is implemented by GtkEntry and GtkSpinButton.
;;; Signals
;;; 
;;;   "changed"                                        : Run Last
;;;   "delete-text"                                    : Run Last
;;;   "insert-text"                                    : Run Last
;;; 
;;; Description
;;; 
;;; The GtkEditable interface is an interface which should be implemented by text editing widgets, such as GtkEntry and GtkText. It contains functions for generically manipulating an editable widget, a large number of action signals used for key bindings, and several signals that an application can connect to to modify the behavior of a widget.
;;; 
;;; As an example of the latter usage, by connecting the following handler to "insert_text", an application can convert all entry into a widget into uppercase.
;;; 
;;; Example 57. Forcing entry to uppercase.
;;; 
;;; 1
;;; 2
;;; 3
;;; 4
;;; 5
;;; 6
;;; 7
;;; 8
;;; 9
;;; 10
;;; 11
;;; 12
;;; 13
;;; 14
;;; 15
;;; 16
;;; 17
;;; 18
;;; 19
;;; 20
;;; 21
;;; 
;;; 	
;;; 
;;; #include <ctype.h>
;;; 
;;; void
;;; insert_text_handler (GtkEditable *editable,
;;;                      const gchar *text,
;;;                      gint         length,
;;;                      gint        *position,
;;;                      gpointer     data)
;;; {
;;;   gchar *result = g_utf8_strup (text, length);
;;; 
;;;   g_signal_handlers_block_by_func (editable,
;;;                                (gpointer) insert_text_handler, data);
;;;   gtk_editable_insert_text (editable, result, length, position);
;;;   g_signal_handlers_unblock_by_func (editable,
;;;                                      (gpointer) insert_text_handler, data);
;;; 
;;;   g_signal_stop_emission_by_name (editable, "insert_text");
;;; 
;;;   g_free (result);
;;; }
;;; 
;;; 
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkEditable
;;; 
;;; typedef struct _GtkEditable GtkEditable;
;;; 
;;; gtk_editable_select_region ()
;;; 
;;; void                gtk_editable_select_region          (GtkEditable *editable,
;;;                                                          gint start_pos,
;;;                                                          gint end_pos);
;;; 
;;; Selects a region of text. The characters that are selected are those characters at positions from start_pos up to, but not including end_pos. If end_pos is negative, then the the characters selected are those characters from start_pos to the end of the text.
;;; 
;;; Note that positions are specified in characters, not bytes.
;;; 
;;; Virtual: set_selection_bounds
;;; 
;;; editable :
;;; 	a GtkEditable
;;; 
;;; start_pos :
;;; 	start of region
;;; 
;;; end_pos :
;;; 	end of region
;;; gtk_editable_get_selection_bounds ()
;;; 
;;; gboolean            gtk_editable_get_selection_bounds   (GtkEditable *editable,
;;;                                                          gint *start_pos,
;;;                                                          gint *end_pos);
;;; 
;;; Retrieves the selection bound of the editable. start_pos will be filled with the start of the selection and end_pos with end. If no text was selected both will be identical and FALSE will be returned.
;;; 
;;; Note that positions are specified in characters, not bytes.
;;; 
;;; editable :
;;; 	a GtkEditable
;;; 
;;; start_pos :
;;; 	location to store the starting position, or NULL. [out][allow-none]
;;; 
;;; end_pos :
;;; 	location to store the end position, or NULL. [out][allow-none]
;;; 
;;; Returns :
;;; 	TRUE if an area is selected, FALSE otherwise
;;; gtk_editable_insert_text ()
;;; 
;;; void                gtk_editable_insert_text            (GtkEditable *editable,
;;;                                                          const gchar *new_text,
;;;                                                          gint new_text_length,
;;;                                                          gint *position);
;;; 
;;; Inserts new_text_length bytes of new_text into the contents of the widget, at position position.
;;; 
;;; Note that the position is in characters, not in bytes. The function updates position to point after the newly inserted text.
;;; 
;;; Virtual: do_insert_text
;;; 
;;; editable :
;;; 	a GtkEditable
;;; 
;;; new_text :
;;; 	the text to append
;;; 
;;; new_text_length :
;;; 	the length of the text in bytes, or -1
;;; 
;;; position :
;;; 	location of the position text will be inserted at. [inout]
;;; gtk_editable_delete_text ()
;;; 
;;; void                gtk_editable_delete_text            (GtkEditable *editable,
;;;                                                          gint start_pos,
;;;                                                          gint end_pos);
;;; 
;;; Deletes a sequence of characters. The characters that are deleted are those characters at positions from start_pos up to, but not including end_pos. If end_pos is negative, then the the characters deleted are those from start_pos to the end of the text.
;;; 
;;; Note that the positions are specified in characters, not bytes.
;;; 
;;; Virtual: do_delete_text
;;; 
;;; editable :
;;; 	a GtkEditable
;;; 
;;; start_pos :
;;; 	start position
;;; 
;;; end_pos :
;;; 	end position
;;; gtk_editable_get_chars ()
;;; 
;;; gchar *             gtk_editable_get_chars              (GtkEditable *editable,
;;;                                                          gint start_pos,
;;;                                                          gint end_pos);
;;; 
;;; Retrieves a sequence of characters. The characters that are retrieved are those characters at positions from start_pos up to, but not including end_pos. If end_pos is negative, then the the characters retrieved are those characters from start_pos to the end of the text.
;;; 
;;; Note that positions are specified in characters, not bytes.
;;; 
;;; editable :
;;; 	a GtkEditable
;;; 
;;; start_pos :
;;; 	start of text
;;; 
;;; end_pos :
;;; 	end of text
;;; 
;;; Returns :
;;; 	a pointer to the contents of the widget as a string. This string is allocated by the GtkEditable implementation and should be freed by the caller.
;;; gtk_editable_cut_clipboard ()
;;; 
;;; void                gtk_editable_cut_clipboard          (GtkEditable *editable);
;;; 
;;; Removes the contents of the currently selected content in the editable and puts it on the clipboard.
;;; 
;;; editable :
;;; 	a GtkEditable
;;; gtk_editable_copy_clipboard ()
;;; 
;;; void                gtk_editable_copy_clipboard         (GtkEditable *editable);
;;; 
;;; Copies the contents of the currently selected content in the editable and puts it on the clipboard.
;;; 
;;; editable :
;;; 	a GtkEditable
;;; gtk_editable_paste_clipboard ()
;;; 
;;; void                gtk_editable_paste_clipboard        (GtkEditable *editable);
;;; 
;;; Pastes the content of the clipboard to the current position of the cursor in the editable.
;;; 
;;; editable :
;;; 	a GtkEditable
;;; gtk_editable_delete_selection ()
;;; 
;;; void                gtk_editable_delete_selection       (GtkEditable *editable);
;;; 
;;; Deletes the currently selected text of the editable. This call doesn't do anything if there is no selected text.
;;; 
;;; editable :
;;; 	a GtkEditable
;;; gtk_editable_set_position ()
;;; 
;;; void                gtk_editable_set_position           (GtkEditable *editable,
;;;                                                          gint position);
;;; 
;;; Sets the cursor position in the editable to the given value.
;;; 
;;; The cursor is displayed before the character with the given (base 0) index in the contents of the editable. The value must be less than or equal to the number of characters in the editable. A value of -1 indicates that the position should be set after the last character of the editable. Note that position is in characters, not in bytes.
;;; 
;;; editable :
;;; 	a GtkEditable
;;; 
;;; position :
;;; 	the position of the cursor
;;; gtk_editable_get_position ()
;;; 
;;; gint                gtk_editable_get_position           (GtkEditable *editable);
;;; 
;;; Retrieves the current position of the cursor relative to the start of the content of the editable.
;;; 
;;; Note that this position is in characters, not in bytes.
;;; 
;;; editable :
;;; 	a GtkEditable
;;; 
;;; Returns :
;;; 	the cursor position
;;; gtk_editable_set_editable ()
;;; 
;;; void                gtk_editable_set_editable           (GtkEditable *editable,
;;;                                                          gboolean is_editable);
;;; 
;;; Determines if the user can edit the text in the editable widget or not.
;;; 
;;; editable :
;;; 	a GtkEditable
;;; 
;;; is_editable :
;;; 	TRUE if the user is allowed to edit the text in the widget
;;; gtk_editable_get_editable ()
;;; 
;;; gboolean            gtk_editable_get_editable           (GtkEditable *editable);
;;; 
;;; Retrieves whether editable is editable. See gtk_editable_set_editable().
;;; 
;;; editable :
;;; 	a GtkEditable
;;; 
;;; Returns :
;;; 	TRUE if editable is editable.
;;; Signal Details
;;; The "changed" signal
;;; 
;;; void                user_function                      (GtkEditable *editable,
;;;                                                         gpointer     user_data)      : Run Last
;;; 
;;; The ::changed signal is emitted at the end of a single user-visible operation on the contents of the GtkEditable.
;;; 
;;; E.g., a paste operation that replaces the contents of the selection will cause only one signal emission (even though it is implemented by first deleting the selection, then inserting the new content, and may cause multiple ::notify::text signals to be emitted).
;;; 
;;; editable :
;;; 	the object which received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; The "delete-text" signal
;;; 
;;; void                user_function                      (GtkEditable *editable,
;;;                                                         gint         start_pos,
;;;                                                         gint         end_pos,
;;;                                                         gpointer     user_data)      : Run Last
;;; 
;;; This signal is emitted when text is deleted from the widget by the user. The default handler for this signal will normally be responsible for deleting the text, so by connecting to this signal and then stopping the signal with g_signal_stop_emission(), it is possible to modify the range of deleted text, or prevent it from being deleted entirely. The start_pos and end_pos parameters are interpreted as for gtk_editable_delete_text().
;;; 
;;; editable :
;;; 	the object which received the signal
;;; 
;;; start_pos :
;;; 	the starting position
;;; 
;;; end_pos :
;;; 	the end position
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; The "insert-text" signal
;;; 
;;; void                user_function                      (GtkEditable *editable,
;;;                                                         gchar       *new_text,
;;;                                                         gint         new_text_length,
;;;                                                         gpointer     position,
;;;                                                         gpointer     user_data)            : Run Last
;;; 
;;; This signal is emitted when text is inserted into the widget by the user. The default handler for this signal will normally be responsible for inserting the text, so by connecting to this signal and then stopping the signal with g_signal_stop_emission(), it is possible to modify the inserted text, or prevent it from being inserted entirely.
;;; 
;;; editable :
;;; 	the object which received the signal
;;; 
;;; new_text :
;;; 	the new text to insert
;;; 
;;; new_text_length :
;;; 	the length of the new text, in bytes, or -1 if new_text is nul-terminated
;;; 
;;; position :
;;; 	the position, in characters, at which to insert the new text. this is an in-out parameter. After the signal emission is finished, it should point after the newly inserted text. [inout][type int]
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 


;; GtkEditable

(defcfun (editable-select-region "gtk_editable_select_region") :void
  (editable (g-object editable))
  (start :int)
  (end :int))

(export 'editable-select-region)

(defcfun gtk-editable-get-selection-bounds :boolean
  (editable (g-object editable))
  (start (:pointer :int))
  (end (:pointer :int)))

(defun editable-selection (editable)
  (with-foreign-objects ((start :int) (end :int))
    (let ((selected-p (gtk-editable-get-selection-bounds editable start end)))
      (values selected-p (mem-ref start :int) (mem-ref end :int)))))

(export 'editable-selection)

(defcfun gtk-editable-insert-text :void
  (editable (g-object editable))
  (new-text :string)
  (new-text-length :int)
  (position (:pointer :int)))

(defun editable-insert-text (editable text position)
  (with-foreign-object (pos :int)
    (setf (mem-ref pos :int) position)
    (gtk-editable-insert-text editable text (length text) pos)
    (mem-ref pos :int)))

(export 'editable-insert-text)

(defcfun gtk-editable-delete-text :void
  (editable (g-object editable))
  (start-pos :int)
  (end-pos :int))

(defun editable-delete-text (editable &key start-pos end-pos)
  (gtk-editable-delete-text editable (or start-pos -1) (or end-pos -1)))

(export 'editable-delete-text)

(defcfun gtk-editable-get-chars g-string
  (editable (g-object editable))
  (start-pos :int)
  (end-pos :int))

(defun editable-get-chars (editable &key (start 0) (end -1))
  (gtk-editable-get-chars editable start end))

(export 'editable-get-chars)

(defcfun (editable-cut-clipboard "gtk_editable_cut_clipboard") :void
  (editable (g-object editable)))

(export 'editable-cut-clipboard)

(defcfun (editable-copy-clipboard "gtk_editable_copy_clipboard") :void
  (editable (g-object editable)))

(export 'editable-copy-clipboard)

(defcfun (editable-paste-clipboard "gtk_editable_paste_clipboard") :void
  (editable (g-object editable)))

(export 'editable-paste-clipboard)

(defcfun (editable-delete-selection "gtk_editable_delete_selection") :void
  (editable (g-object editable)))

(export 'editable-delete-selection)

