;;; ----------------------------------------------------------------------------
;;; gtk.text-tag-table.lisp
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
;;; GtkTextTagTable
;;; 
;;; GtkTextTagTable — Collection of tags that can be used together
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;;                     GtkTextTagTable;
;;; void                (*GtkTextTagTableForeach)           (GtkTextTag *tag,
;;;                                                          gpointer data);
;;; GtkTextTagTable *   gtk_text_tag_table_new              (void);
;;; void                gtk_text_tag_table_add              (GtkTextTagTable *table,
;;;                                                          GtkTextTag *tag);
;;; void                gtk_text_tag_table_remove           (GtkTextTagTable *table,
;;;                                                          GtkTextTag *tag);
;;; GtkTextTag *        gtk_text_tag_table_lookup           (GtkTextTagTable *table,
;;;                                                          const gchar *name);
;;; void                gtk_text_tag_table_foreach          (GtkTextTagTable *table,
;;;                                                          GtkTextTagTableForeach func,
;;;                                                          gpointer data);
;;; gint                gtk_text_tag_table_get_size         (GtkTextTagTable *table);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkTextTagTable
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkTextTagTable implements GtkBuildable.
;;; Signals
;;; 
;;;   "tag-added"                                      : Run Last
;;;   "tag-changed"                                    : Run Last
;;;   "tag-removed"                                    : Run Last
;;; 
;;; Description
;;; 
;;; You may wish to begin by reading the text widget conceptual overview which gives an overview of all the objects and data types related to the text widget and how they work together.
;;; 
;;; GtkTextTagTables as GtkBuildable
;;; 
;;; The GtkTextTagTable implementation of the GtkBuildable interface supports adding tags by specifying "tag" as the "type" attribute of a <child> element.
;;; 
;;; Example 58. A UI definition fragment specifying tags
;;; 
;;; 1
;;; 2
;;; 3
;;; 4
;;; 5
;;; 
;;; 	
;;; 
;;; <object class="GtkTextTagTable">
;;;  <child type="tag">
;;;    <object class="GtkTextTag"/>
;;;  </child>
;;; </object>
;;; 
;;; 
;;; Details
;;; GtkTextTagTable
;;; 
;;; typedef struct _GtkTextTagTable GtkTextTagTable;
;;; 
;;; GtkTextTagTableForeach ()
;;; 
;;; void                (*GtkTextTagTableForeach)           (GtkTextTag *tag,
;;;                                                          gpointer data);
;;; 
;;; gtk_text_tag_table_new ()
;;; 
;;; GtkTextTagTable *   gtk_text_tag_table_new              (void);
;;; 
;;; Creates a new GtkTextTagTable. The table contains no tags by default.
;;; 
;;; Returns :
;;; 	a new GtkTextTagTable
;;; gtk_text_tag_table_add ()
;;; 
;;; void                gtk_text_tag_table_add              (GtkTextTagTable *table,
;;;                                                          GtkTextTag *tag);
;;; 
;;; Add a tag to the table. The tag is assigned the highest priority in the table.
;;; 
;;; tag must not be in a tag table already, and may not have the same name as an already-added tag.
;;; 
;;; table :
;;; 	a GtkTextTagTable
;;; 
;;; tag :
;;; 	a GtkTextTag
;;; gtk_text_tag_table_remove ()
;;; 
;;; void                gtk_text_tag_table_remove           (GtkTextTagTable *table,
;;;                                                          GtkTextTag *tag);
;;; 
;;; Remove a tag from the table. This will remove the table's reference to the tag, so be careful - the tag will end up destroyed if you don't have a reference to it.
;;; 
;;; table :
;;; 	a GtkTextTagTable
;;; 
;;; tag :
;;; 	a GtkTextTag
;;; gtk_text_tag_table_lookup ()
;;; 
;;; GtkTextTag *        gtk_text_tag_table_lookup           (GtkTextTagTable *table,
;;;                                                          const gchar *name);
;;; 
;;; Look up a named tag.
;;; 
;;; table :
;;; 	a GtkTextTagTable
;;; 
;;; name :
;;; 	name of a tag
;;; 
;;; Returns :
;;; 	The tag, or NULL if none by that name is in the table. [transfer none]
;;; gtk_text_tag_table_foreach ()
;;; 
;;; void                gtk_text_tag_table_foreach          (GtkTextTagTable *table,
;;;                                                          GtkTextTagTableForeach func,
;;;                                                          gpointer data);
;;; 
;;; Calls func on each tag in table, with user data data. Note that the table may not be modified while iterating over it (you can't add/remove tags).
;;; 
;;; table :
;;; 	a GtkTextTagTable
;;; 
;;; func :
;;; 	a function to call on each tag. [scope call]
;;; 
;;; data :
;;; 	user data
;;; gtk_text_tag_table_get_size ()
;;; 
;;; gint                gtk_text_tag_table_get_size         (GtkTextTagTable *table);
;;; 
;;; Returns the size of the table (number of tags)
;;; 
;;; table :
;;; 	a GtkTextTagTable
;;; 
;;; Returns :
;;; 	number of tags in table
;;; Signal Details
;;; The "tag-added" signal
;;; 
;;; void                user_function                      (GtkTextTagTable *texttagtable,
;;;                                                         GtkTextTag      *tag,
;;;                                                         gpointer         user_data)         : Run Last
;;; 
;;; texttagtable :
;;; 	the object which received the signal.
;;; 
;;; tag :
;;; 	the added tag.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; The "tag-changed" signal
;;; 
;;; void                user_function                      (GtkTextTagTable *texttagtable,
;;;                                                         GtkTextTag      *tag,
;;;                                                         gboolean         size_changed,
;;;                                                         gpointer         user_data)         : Run Last
;;; 
;;; texttagtable :
;;; 	the object which received the signal.
;;; 
;;; tag :
;;; 	the changed tag.
;;; 
;;; size_changed :
;;; 	whether the size has been changed.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; The "tag-removed" signal
;;; 
;;; void                user_function                      (GtkTextTagTable *texttagtable,
;;;                                                         GtkTextTag      *tag,
;;;                                                         gpointer         user_data)         : Run Last
;;; 
;;; texttagtable :
;;; 	the object which received the signal.
;;; 
;;; tag :
;;; 	the removed tag.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; 
