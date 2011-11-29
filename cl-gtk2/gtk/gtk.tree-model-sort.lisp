;;; ----------------------------------------------------------------------------
;;; gtk.tree-model-sort.lisp
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
;;; GtkTreeModelSort
;;; 
;;; A GtkTreeModel which makes an underlying tree model sortable
;;; 	
;;; Synopsis
;;; 
;;; #include <gtk/gtk.h>
;;; 
;;; struct              GtkTreeModelSort;
;;; GtkTreeModel *      gtk_tree_model_sort_new_with_model  (GtkTreeModel *child_model);
;;; GtkTreeModel *      gtk_tree_model_sort_get_model       (GtkTreeModelSort *tree_model);
;;; GtkTreePath *       gtk_tree_model_sort_convert_child_path_to_path
;;;                                                         (GtkTreeModelSort *tree_model_sort,
;;;                                                          GtkTreePath *child_path);
;;; gboolean            gtk_tree_model_sort_convert_child_iter_to_iter
;;;                                                         (GtkTreeModelSort *tree_model_sort,
;;;                                                          GtkTreeIter *sort_iter,
;;;                                                          GtkTreeIter *child_iter);
;;; GtkTreePath *       gtk_tree_model_sort_convert_path_to_child_path
;;;                                                         (GtkTreeModelSort *tree_model_sort,
;;;                                                          GtkTreePath *sorted_path);
;;; void                gtk_tree_model_sort_convert_iter_to_child_iter
;;;                                                         (GtkTreeModelSort *tree_model_sort,
;;;                                                          GtkTreeIter *child_iter,
;;;                                                          GtkTreeIter *sorted_iter);
;;; void                gtk_tree_model_sort_reset_default_sort_func
;;;                                                         (GtkTreeModelSort *tree_model_sort);
;;; void                gtk_tree_model_sort_clear_cache     (GtkTreeModelSort *tree_model_sort);
;;; gboolean            gtk_tree_model_sort_iter_is_valid   (GtkTreeModelSort *tree_model_sort,
;;;                                                          GtkTreeIter *iter);
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkTreeModelSort
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkTreeModelSort implements GtkTreeModel, GtkTreeSortable and
;;; GtkTreeDragSource.

;;; Properties
;;; 
;;;   "model"       GtkTreeModel*         : Read / Write / Construct Only
;;; 
;;; Description
;;; 
;;; The GtkTreeModelSort is a model which implements the GtkTreeSortable
;;; interface. It does not hold any data itself, but rather is created with a
;;; child model and proxies its data. It has identical column types to this
;;; child model, and the changes in the child are propagated. The primary
;;; purpose of this model is to provide a way to sort a different model without
;;; modifying it. Note that the sort function used by GtkTreeModelSort is not
;;; guaranteed to be stable.
;;; 
;;; The use of this is best demonstrated through an example. In the following
;;; sample code we create two GtkTreeView widgets each with a view of the same
;;; data. As the model is wrapped here by a GtkTreeModelSort, the two
;;; GtkTreeViews can each sort their view of the data without affecting the
;;; other. By contrast, if we simply put the same model in each widget, then
;;; sorting the first would sort the second.
;;; 
;;; Example 62. Using a GtkTreeModelSort
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
;;; 22
;;; 23
;;; 24
;;; 
;;; 	
;;; 
;;; {
;;;   GtkTreeView *tree_view1;
;;;   GtkTreeView *tree_view2;
;;;   GtkTreeModel *sort_model1;
;;;   GtkTreeModel *sort_model2;
;;;   GtkTreeModel *child_model;
;;; 
;;;   // get the child model
;;;   child_model = get_my_model ();
;;; 
;;;   // Create the first tree
;;;   sort_model1 = gtk_tree_model_sort_new_with_model (child_model);
;;;   tree_view1 = gtk_tree_view_new_with_model (sort_model1);
;;; 
;;;   // Create the second tree
;;;   sort_model2 = gtk_tree_model_sort_new_with_model (child_model);
;;;   tree_view2 = gtk_tree_view_new_with_model (sort_model2);
;;; 
;;;   // Now we can sort the two models independently
;;;   gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (sort_model1),
;;;                                         COLUMN_1, GTK_SORT_ASCENDING);
;;;   gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (sort_model2),
;;;                                         COLUMN_1, GTK_SORT_DESCENDING);
;;; }
;;; 
;;; 
;;; To demonstrate how to access the underlying child model from the sort model,
;;; the next example will be a callback for the GtkTreeSelection "changed"
;;; signal. In this callback, we get a string from COLUMN_1 of the model. We
;;; then modify the string, find the same selected row on the child model, and
;;; change the row there.
;;; 
;;; Example 63. Accessing the child model of in a selection changed callback
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
;;; 22
;;; 23
;;; 24
;;; 25
;;; 26
;;; 27
;;; 28
;;; 29
;;; 30
;;; 31
;;; 32
;;; 33
;;; 34
;;; 35
;;; 36
;;; 37
;;; 38
;;; 39
;;; 40
;;; 41
;;; 
;;; 	
;;; 
;;; void
;;; selection_changed (GtkTreeSelection *selection, gpointer data)
;;; {
;;;   GtkTreeModel *sort_model = NULL;
;;;   GtkTreeModel *child_model;
;;;   GtkTreeIter sort_iter;
;;;   GtkTreeIter child_iter;
;;;   char *some_data = NULL;
;;;   char *modified_data;
;;; 
;;;   // Get the current selected row and the model.
;;;   if (! gtk_tree_selection_get_selected (selection,
;;;                                          &sort_model,
;;;                                          &sort_iter))
;;;     return;
;;; 
;;;   /* Look up the current value on the selected row and get a new value
;;;    * to change it to.
;;;    */
;;;   gtk_tree_model_get (GTK_TREE_MODEL (sort_model), &sort_iter,
;;;                       COLUMN_1, &some_data,
;;;                       -1);
;;; 
;;;   modified_data = change_the_data (some_data);
;;;   g_free (some_data);
;;; 
;;;   // Get an iterator on the child model, instead of the sort model.
;;;   gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (sort_model),
;;;                                                   &child_iter,
;;;                                                   &sort_iter);
;;; 
;;;   /* Get the child model and change the value of the row.  In this
;;;    * example, the child model is a GtkListStore.  It could be any other
;;;    * type of model, though.
;;;    */
;;;   child_model = gtk_tree_model_sort_get_model (GTK_TREE_MODEL_SORT (sort_model));
;;;   gtk_list_store_set (GTK_LIST_STORE (child_model), &child_iter,
;;;                       COLUMN_1, &modified_data,
;;;                       -1);
;;;   g_free (modified_data);
;;; }
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeModelSort
;;; 
;;; struct GtkTreeModelSort;
;;; 
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_new_with_model ()
;;; 
;;; GtkTreeModel * gtk_tree_model_sort_new_with_model
;;;                                                  (GtkTreeModel *child_model)
;;; 
;;; Creates a new GtkTreeModel, with child_model as the child model.
;;; 
;;; child_model :
;;; 	A GtkTreeModel
;;; 
;;; Returns :
;;; 	A new GtkTreeModel. [transfer full]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_get_model ()
;;; 
;;; GtkTreeModel * gtk_tree_model_sort_get_model (GtkTreeModelSort *tree_model)
;;; 
;;; Returns the model the GtkTreeModelSort is sorting.
;;; 
;;; tree_model :
;;; 	a GtkTreeModelSort
;;; 
;;; Returns :
;;; 	the "child model" being sorted. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_child_path_to_path ()
;;; 
;;; GtkTreePath * gtk_tree_model_sort_convert_child_path_to_path
;;;                                          (GtkTreeModelSort *tree_model_sort,
;;;                                           GtkTreePath *child_path)
;;; 
;;; Converts child_path to a path relative to tree_model_sort. That is,
;;; child_path points to a path in the child model. The returned path will point
;;; to the same row in the sorted model. If child_path isn't a valid path on the
;;; child model, then NULL is returned.
;;; 
;;; tree_model_sort :
;;; 	A GtkTreeModelSort
;;; 
;;; child_path :
;;; 	A GtkTreePath to convert
;;; 
;;; Returns :
;;; 	A newly allocated GtkTreePath, or NULL
;;; ----------------------------------------------------------------------------

(defcfun (tree-model-sort-convert-child-path-to-path
          "gtk_tree_model_sort_convert_child_path_to_path")
         (g-boxed-foreign tree-path :return)
  (tree-model-sort (g-object tree-model-sort))
  (child-path (g-boxed-foreign tree-path)))

(export 'tree-model-sort-convert-child-path-to-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_child_iter_to_iter ()
;;; 
;;; gboolean gtk_tree_model_sort_convert_child_iter_to_iter
;;;                                          (GtkTreeModelSort *tree_model_sort,
;;;                                           GtkTreeIter *sort_iter,
;;;                                           GtkTreeIter *child_iter)
;;; 
;;; Sets sort_iter to point to the row in tree_model_sort that corresponds to
;;; the row pointed at by child_iter. If sort_iter was not set, FALSE is
;;; returned. Note: a boolean is only returned since 2.14.
;;; 
;;; tree_model_sort :
;;; 	A GtkTreeModelSort
;;; 
;;; sort_iter :
;;; 	An uninitialized GtkTreeIter. [out]
;;; 
;;; child_iter :
;;; 	A valid GtkTreeIter pointing to a row on the child model
;;; 
;;; Returns :
;;; 	TRUE, if sort_iter was set, i.e. if sort_iter is a valid iterator
;;;     pointer to a visible row in the child model.
;;; ----------------------------------------------------------------------------

(defcfun gtk-tree-model-sort-convert-child-iter-to-iter :boolean
  (tree-model-sort (g-object tree-model-sort))
  (sort-iter (g-boxed-foreign tree-iter))
  (child-iter (g-boxed-foreign tree-iter)))

(defun tree-model-sort-convert-child-iter-to-iter (tree-model-sort child-iter)
  (let ((sort-iter (make-tree-iter)))
    (when (gtk-tree-model-sort-convert-child-iter-to-iter tree-model-sort
                                                          sort-iter child-iter)
      sort-iter)))

(export 'tree-model-sort-convert-child-iter-to-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_path_to_child_path ()
;;; 
;;; GtkTreePath * gtk_tree_model_sort_convert_path_to_child_path
;;;                                          (GtkTreeModelSort *tree_model_sort,
;;;                                           GtkTreePath *sorted_path)
;;; 
;;; Converts sorted_path to a path on the child model of tree_model_sort. That
;;; is, sorted_path points to a location in tree_model_sort. The returned path
;;; will point to the same location in the model not being sorted. If
;;; sorted_path does not point to a location in the child model, NULL is
;;; returned.
;;; 
;;; tree_model_sort :
;;; 	A GtkTreeModelSort
;;; 
;;; sorted_path :
;;; 	A GtkTreePath to convert
;;; 
;;; Returns :
;;; 	A newly allocated GtkTreePath, or NULL
;;; ----------------------------------------------------------------------------

(defcfun (tree-model-sort-convert-path-to-child-path
          "gtk_tree_model_sort_convert_path_to_child_path")
         (g-boxed-foreign tree-path :return)
  (tree-model-sort (g-object tree-model-sort))
  (sort-path (g-boxed-foreign tree-path)))

(export 'tree-model-sort-convert-path-to-child-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_iter_to_child_iter ()
;;; 
;;; void gtk_tree_model_sort_convert_iter_to_child_iter
;;;                                          (GtkTreeModelSort *tree_model_sort,
;;;                                           GtkTreeIter *child_iter,
;;;                                           GtkTreeIter *sorted_iter)
;;; 
;;; Sets child_iter to point to the row pointed to by sorted_iter.
;;; 
;;; tree_model_sort :
;;; 	A GtkTreeModelSort
;;; 
;;; child_iter :
;;; 	An uninitialized GtkTreeIter. [out]
;;; 
;;; sorted_iter :
;;; 	A valid GtkTreeIter pointing to a row on tree_model_sort.
;;; ----------------------------------------------------------------------------

(defcfun gtk-tree-model-sort-convert-iter-to-child-iter :void
  (tree-model-sort (g-object tree-model-sort))
  (child-iter (g-boxed-foreign tree-iter))
  (sorted-iter (g-boxed-foreign tree-iter)))

(defun tree-model-sort-convert-iter-to-child-iter (tree-model-sort sorted-iter)
  (let ((child-iter (make-tree-iter)))
    (gtk-tree-model-sort-convert-iter-to-child-iter tree-model-sort
                                                    child-iter sorted-iter)
    child-iter))

(export 'tree-model-sort-convert-iter-to-child-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_reset_default_sort_func ()
;;; 
;;; void gtk_tree_model_sort_reset_default_sort_func
;;;                                          (GtkTreeModelSort *tree_model_sort)
;;; 
;;; This resets the default sort function to be in the 'unsorted' state. That
;;; is, it is in the same order as the child model. It will re-sort the model
;;; to be in the same order as the child model only if the GtkTreeModelSort is
;;; in 'unsorted' state.
;;; 
;;; tree_model_sort :
;;; 	A GtkTreeModelSort
;;; ----------------------------------------------------------------------------

(defcfun (tree-model-sort-reset-default-sort-func
          "gtk_tree_model_sort_reset_default_sort_func")
         :void
  (tree-model-sort (g-object tree-model-sort)))

(export 'tree-model-sort-reset-default-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_clear_cache ()
;;; 
;;; void gtk_tree_model_sort_clear_cache (GtkTreeModelSort *tree_model_sort)
;;; 
;;; This function should almost never be called. It clears the tree_model_sort
;;; of any cached iterators that haven't been reffed with
;;; gtk_tree_model_ref_node(). This might be useful if the child model being
;;; sorted is static (and doesn't change often) and there has been a lot of
;;; unreffed access to nodes. As a side effect of this function, all unreffed
;;; iters will be invalid.
;;; 
;;; tree_model_sort :
;;; 	A GtkTreeModelSort
;;; ----------------------------------------------------------------------------

(defcfun (tree-model-sort-clear-cache "gtk_tree_model_sort_clear_cache") :void
  (tree-model-sort (g-object tree-model-sort)))

(export 'tree-model-sort-clear-cached)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_iter_is_valid ()
;;; 
;;; gboolean gtk_tree_model_sort_iter_is_valid 
;;;                                          (GtkTreeModelSort *tree_model_sort,
;;;                                           GtkTreeIter *iter)
;;; 
;;; Warning
;;; 
;;; This function is slow. Only use it for debugging and/or testing purposes.
;;; 
;;; Checks if the given iter is a valid iter for this GtkTreeModelSort.
;;; 
;;; tree_model_sort :
;;; 	A GtkTreeModelSort.
;;; 
;;; iter :
;;; 	A GtkTreeIter.
;;; 
;;; Returns :
;;; 	TRUE if the iter is valid, FALSE if the iter is invalid.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun (tree-model-sort-iter-is-valid "gtk_tree_model_sort_iter_is_valid")
         :boolean
  (tree-model-sort (g-object tree-model-sort))
  (iter (g-boxed-foreign tree-iter)))

(export 'tree-model-sort-iter-is-valid)

;;; ----------------------------------------------------------------------------
;;; Property Details
;;;
;;; The "model" property
;;; 
;;;   "model"          GtkTreeModel*         : Read / Write / Construct Only
;;; 
;;; The model for the TreeModelSort to sort.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.tree-model-sort.lisp -----------------------------------
