;;; ----------------------------------------------------------------------------
;;; glib.mem.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2 from
;;; http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GLib 2.30.2 Reference Manual
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
;;; Memory Allocation
;;; 
;;; General memory-handling
;;; 	
;;; Synopsis
;;; 
;;; #define            g_new                    (struct_type,
;;;                                              n_structs)
;;; #define            g_new0                   (struct_type,
;;;                                              n_structs)
;;; #define            g_renew                  (struct_type,
;;;                                              mem,
;;;                                              n_structs)
;;; #define            g_try_new                (struct_type,
;;;                                              n_structs)
;;; #define            g_try_new0               (struct_type,
;;;                                              n_structs)
;;; #define            g_try_renew              (struct_type,
;;;                                              mem,
;;;                                              n_structs)
;;;                                             
;;; gpointer           g_malloc                 (gsize n_bytes);
;;; gpointer           g_malloc0                (gsize n_bytes);
;;; gpointer           g_realloc                (gpointer mem,
;;;                                              gsize n_bytes);
;;; gpointer           g_try_malloc             (gsize n_bytes);
;;; gpointer           g_try_malloc0            (gsize n_bytes);
;;; gpointer           g_try_realloc            (gpointer mem,
;;;                                              gsize n_bytes);
;;; gpointer           g_malloc_n               (gsize n_blocks,
;;;                                              gsize n_block_bytes);
;;; gpointer           g_malloc0_n              (gsize n_blocks,
;;;                                              gsize n_block_bytes);
;;; gpointer           g_realloc_n              (gpointer mem,
;;;                                              gsize n_blocks,
;;;                                              gsize n_block_bytes);
;;; gpointer           g_try_malloc_n           (gsize n_blocks,
;;;                                              gsize n_block_bytes);
;;; gpointer           g_try_malloc0_n          (gsize n_blocks,
;;;                                              gsize n_block_bytes);
;;; gpointer           g_try_realloc_n          (gpointer mem,
;;;                                              gsize n_blocks,
;;;                                              gsize n_block_bytes);
;;;                                             
;;; void               g_free                   (gpointer mem);
;;; extern gboolean    g_mem_gc_friendly;       
;;;                                             
;;; #define            g_alloca                 (size)
;;; #define            g_newa                   (struct_type,
;;;                                              n_structs)
;;;                                             
;;; #define            g_memmove                (dest,
;;;                                              src,
;;;                                              len)
;;; gpointer           g_memdup                 (gconstpointer mem,
;;;                                              guint byte_size);
;;;                                             
;;; struct             GMemVTable;              
;;; void               g_mem_set_vtable         (GMemVTable *vtable);
;;; gboolean           g_mem_is_system_malloc   (void);
;;;                                             
;;; extern GMemVTable* glib_mem_profiler_table; 
;;; void               g_mem_profile            (void);
;;; 
;;; Description
;;; 
;;; These functions provide support for allocating and freeing memory.
;;; 
;;; Note
;;; If any call to allocate memory fails, the application is terminated. This 
;;; also means that there is no need to check if the call succeeded.
;;; 
;;; Note
;;; It's important to match g_malloc() with g_free(), plain malloc() with
;;; free(), and (if you're using C++) new with delete and new[] with delete[]. 
;;; Otherwise bad things can happen, since these allocators may use different 
;;; memory pools (and new/delete call constructors and destructors). See also
;;; g_mem_set_vtable().
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; g_new()
;;; 
;;; #define             g_new(struct_type, n_structs)
;;; 
;;; Allocates n_structs elements of type struct_type. The returned pointer is cast to a pointer to the given type. If n_structs is 0 it returns NULL. Care is taken to avoid overflow when calculating the size of the allocated block.
;;; 
;;; Since the returned pointer is already casted to the right type, it is normally unnecessary to cast it explicitly, and doing so might hide memory allocation errors.
;;; 
;;; struct_type :
;;; 	the type of the elements to allocate
;;; 
;;; n_structs :
;;; 	the number of elements to allocate
;;; 
;;; Returns :
;;; 	a pointer to the allocated memory, cast to a pointer to struct_type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_new0()
;;; 
;;; #define             g_new0(struct_type, n_structs)
;;; 
;;; Allocates n_structs elements of type struct_type, initialized to 0's. The returned pointer is cast to a pointer to the given type. If n_structs is 0 it returns NULL. Care is taken to avoid overflow when calculating the size of the allocated block.
;;; 
;;; Since the returned pointer is already casted to the right type, it is normally unnecessary to cast it explicitly, and doing so might hide memory allocation errors.
;;; 
;;; struct_type :
;;; 	the type of the elements to allocate.
;;; 
;;; n_structs :
;;; 	the number of elements to allocate.
;;; 
;;; Returns :
;;; 	a pointer to the allocated memory, cast to a pointer to struct_type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_renew()
;;; 
;;; #define             g_renew(struct_type, mem, n_structs)
;;; 
;;; Reallocates the memory pointed to by mem, so that it now has space for n_structs elements of type struct_type. It returns the new address of the memory, which may have been moved. Care is taken to avoid overflow when calculating the size of the allocated block.
;;; 
;;; struct_type :
;;; 	the type of the elements to allocate
;;; 
;;; mem :
;;; 	the currently allocated memory
;;; 
;;; n_structs :
;;; 	the number of elements to allocate
;;; 
;;; Returns :
;;; 	a pointer to the new allocated memory, cast to a pointer to struct_type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_try_new()
;;; 
;;; #define             g_try_new(struct_type, n_structs)
;;; 
;;; Attempts to allocate n_structs elements of type struct_type, and returns NULL on failure. Contrast with g_new(), which aborts the program on failure. The returned pointer is cast to a pointer to the given type. The function returns NULL when n_structs is 0 of if an overflow occurs.
;;; 
;;; struct_type :
;;; 	the type of the elements to allocate
;;; 
;;; n_structs :
;;; 	the number of elements to allocate
;;; 
;;; Returns :
;;; 	a pointer to the allocated memory, cast to a pointer to struct_type
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_try_new0()
;;; 
;;; #define             g_try_new0(struct_type, n_structs)
;;; 
;;; Attempts to allocate n_structs elements of type struct_type, initialized to 0's, and returns NULL on failure. Contrast with g_new0(), which aborts the program on failure. The returned pointer is cast to a pointer to the given type. The function returns NULL when n_structs is 0 of if an overflow occurs.
;;; 
;;; struct_type :
;;; 	the type of the elements to allocate
;;; 
;;; n_structs :
;;; 	the number of elements to allocate
;;; 
;;; Returns :
;;; 	a pointer to the allocated memory, cast to a pointer to struct_type
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_try_renew()
;;; 
;;; #define             g_try_renew(struct_type, mem, n_structs)
;;; 
;;; Attempts to reallocate the memory pointed to by mem, so that it now has space for n_structs elements of type struct_type, and returns NULL on failure. Contrast with g_renew(), which aborts the program on failure. It returns the new address of the memory, which may have been moved. The function returns NULL if an overflow occurs.
;;; 
;;; struct_type :
;;; 	the type of the elements to allocate
;;; 
;;; mem :
;;; 	the currently allocated memory
;;; 
;;; n_structs :
;;; 	the number of elements to allocate
;;; 
;;; Returns :
;;; 	a pointer to the new allocated memory, cast to a pointer to struct_type
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_malloc ()
;;; 
;;; gpointer g_malloc (gsize n_bytes)
;;; 
;;; Allocates n_bytes bytes of memory. If n_bytes is 0 it returns NULL.
;;; 
;;; n_bytes :
;;; 	the number of bytes to allocate
;;; 
;;; Returns :
;;; 	a pointer to the allocated memory
;;; ----------------------------------------------------------------------------

(defcfun (g-malloc "g_malloc0") :pointer
  (n-bytes gsize))

;;; ----------------------------------------------------------------------------
;;; g_malloc0 ()
;;; 
;;; gpointer            g_malloc0                           (gsize n_bytes);
;;; 
;;; Allocates n_bytes bytes of memory, initialized to 0's. If n_bytes is 0 it returns NULL.
;;; 
;;; n_bytes :
;;; 	the number of bytes to allocate
;;; 
;;; Returns :
;;; 	a pointer to the allocated memory
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_realloc ()
;;; 
;;; gpointer            g_realloc                           (gpointer mem,
;;;                                                          gsize n_bytes);
;;; 
;;; Reallocates the memory pointed to by mem, so that it now has space for n_bytes bytes of memory. It returns the new address of the memory, which may have been moved. mem may be NULL, in which case it's considered to have zero-length. n_bytes may be 0, in which case NULL will be returned and mem will be freed unless it is NULL.
;;; 
;;; mem :
;;; 	the memory to reallocate
;;; 
;;; n_bytes :
;;; 	new size of the memory in bytes
;;; 
;;; Returns :
;;; 	the new address of the allocated memory
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_try_malloc ()
;;; 
;;; gpointer            g_try_malloc                        (gsize n_bytes);
;;; 
;;; Attempts to allocate n_bytes, and returns NULL on failure. Contrast with g_malloc(), which aborts the program on failure.
;;; 
;;; n_bytes :
;;; 	number of bytes to allocate.
;;; 
;;; Returns :
;;; 	the allocated memory, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_try_malloc0 ()
;;; 
;;; gpointer            g_try_malloc0                       (gsize n_bytes);
;;; 
;;; Attempts to allocate n_bytes, initialized to 0's, and returns NULL on failure. Contrast with g_malloc0(), which aborts the program on failure.
;;; 
;;; n_bytes :
;;; 	number of bytes to allocate
;;; 
;;; Returns :
;;; 	the allocated memory, or NULL
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_try_realloc ()
;;; 
;;; gpointer            g_try_realloc                       (gpointer mem,
;;;                                                          gsize n_bytes);
;;; 
;;; Attempts to realloc mem to a new size, n_bytes, and returns NULL on failure. Contrast with g_realloc(), which aborts the program on failure. If mem is NULL, behaves the same as g_try_malloc().
;;; 
;;; mem :
;;; 	previously-allocated memory, or NULL.
;;; 
;;; n_bytes :
;;; 	number of bytes to allocate.
;;; 
;;; Returns :
;;; 	the allocated memory, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_malloc_n ()
;;; 
;;; gpointer            g_malloc_n                          (gsize n_blocks,
;;;                                                          gsize n_block_bytes);
;;; 
;;; This function is similar to g_malloc(), allocating (n_blocks * n_block_bytes) bytes, but care is taken to detect possible overflow during multiplication.
;;; 
;;; n_blocks :
;;; 	the number of blocks to allocate
;;; 
;;; n_block_bytes :
;;; 	the size of each block in bytes
;;; 
;;; Returns :
;;; 	a pointer to the allocated memory
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_malloc0_n ()
;;; 
;;; gpointer            g_malloc0_n                         (gsize n_blocks,
;;;                                                          gsize n_block_bytes);
;;; 
;;; This function is similar to g_malloc0(), allocating (n_blocks * n_block_bytes) bytes, but care is taken to detect possible overflow during multiplication.
;;; 
;;; n_blocks :
;;; 	the number of blocks to allocate
;;; 
;;; n_block_bytes :
;;; 	the size of each block in bytes
;;; 
;;; Returns :
;;; 	a pointer to the allocated memory
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_realloc_n ()
;;; 
;;; gpointer            g_realloc_n                         (gpointer mem,
;;;                                                          gsize n_blocks,
;;;                                                          gsize n_block_bytes);
;;; 
;;; This function is similar to g_realloc(), allocating (n_blocks * n_block_bytes) bytes, but care is taken to detect possible overflow during multiplication.
;;; 
;;; mem :
;;; 	the memory to reallocate
;;; 
;;; n_blocks :
;;; 	the number of blocks to allocate
;;; 
;;; n_block_bytes :
;;; 	the size of each block in bytes
;;; 
;;; Returns :
;;; 	the new address of the allocated memory
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_try_malloc_n ()
;;; 
;;; gpointer            g_try_malloc_n                      (gsize n_blocks,
;;;                                                          gsize n_block_bytes);
;;; 
;;; This function is similar to g_try_malloc(), allocating (n_blocks * n_block_bytes) bytes, but care is taken to detect possible overflow during multiplication.
;;; 
;;; n_blocks :
;;; 	the number of blocks to allocate
;;; 
;;; n_block_bytes :
;;; 	the size of each block in bytes
;;; 
;;; Returns :
;;; 	the allocated memory, or NULL.
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_try_malloc0_n ()
;;; 
;;; gpointer            g_try_malloc0_n                     (gsize n_blocks,
;;;                                                          gsize n_block_bytes);
;;; 
;;; This function is similar to g_try_malloc0(), allocating (n_blocks * n_block_bytes) bytes, but care is taken to detect possible overflow during multiplication.
;;; 
;;; n_blocks :
;;; 	the number of blocks to allocate
;;; 
;;; n_block_bytes :
;;; 	the size of each block in bytes
;;; 
;;; Returns :
;;; 	the allocated memory, or NULL
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_try_realloc_n ()
;;; 
;;; gpointer            g_try_realloc_n                     (gpointer mem,
;;;                                                          gsize n_blocks,
;;;                                                          gsize n_block_bytes);
;;; 
;;; This function is similar to g_try_realloc(), allocating (n_blocks * n_block_bytes) bytes, but care is taken to detect possible overflow during multiplication.
;;; 
;;; mem :
;;; 	previously-allocated memory, or NULL.
;;; 
;;; n_blocks :
;;; 	the number of blocks to allocate
;;; 
;;; n_block_bytes :
;;; 	the size of each block in bytes
;;; 
;;; Returns :
;;; 	the allocated memory, or NULL.
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_free ()
;;; 
;;; void g_free (gpointer mem)
;;; 
;;; Frees the memory pointed to by mem. If mem is NULL it simply returns.
;;; 
;;; mem :
;;; 	the memory to free
;;; ----------------------------------------------------------------------------

(defcfun g-free :void
  (ptr :pointer))

;;; ----------------------------------------------------------------------------
;;; g_mem_gc_friendly
;;; 
;;; extern gboolean g_mem_gc_friendly;
;;; 
;;; This variable is TRUE if the G_DEBUG environment variable includes the key
;;; gc-friendly.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_alloca()
;;; 
;;; #define g_alloca(size)
;;; 
;;; Allocates size bytes on the stack; these bytes will be freed when the
;;; current stack frame is cleaned up. This macro essentially just wraps the
;;; alloca() function present on most UNIX variants. Thus it provides the same
;;; advantages and pitfalls as alloca():
;;; 
;;; + alloca() is very fast, as on most systems it's implemented by just
;;;   adjusting the stack pointer register.
;;;
;;; + It doesn't cause any memory fragmentation, within its scope, separate
;;;   alloca() blocks just build up and are released together at function end.
;;; 
;;; - Allocation sizes have to fit into the current stack frame. For instance
;;;   in a threaded environment on Linux, the per-thread stack size is limited
;;;   to 2 Megabytes, so be sparse with alloca() uses.
;;; 
;;; - Allocation failure due to insufficient stack space is not indicated with 
;;;   a NULL return like e.g. with malloc(). Instead, most systems probably 
;;;   handle it the same way as out of stack space situations from infinite
;;;   function recursion, i.e. with a segmentation fault.
;;; 
;;; - Special care has to be taken when mixing alloca() with GNU C variable 
;;;   sized arrays. Stack space allocated with alloca() in the same scope as a 
;;;   variable sized array will be freed together with the variable sized array 
;;;   upon exit of that scope, and not upon exit of the enclosing function 
;;;   scope.
;;; 
;;; size :
;;; 	number of bytes to allocate.
;;; 
;;; Returns :
;;; 	space for size bytes, allocated on the stack
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_newa()
;;; 
;;; #define g_newa(struct_type, n_structs)
;;; 
;;; Wraps g_alloca() in a more typesafe manner.
;;; 
;;; struct_type :
;;; 	Type of memory chunks to be allocated
;;; 
;;; n_structs :
;;; 	Number of chunks to be allocated
;;; 
;;; Returns :
;;; 	Pointer to stack space for n_structs chunks of type struct_type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_memmove()
;;; 
;;; #define g_memmove(dest,src,len)
;;; 
;;; Copies a block of memory len bytes long, from src to dest. The source and
;;; destination areas may overlap.
;;; 
;;; In order to use this function, you must include string.h yourself, because
;;; this macro will typically simply resolve to memmove() and GLib does not
;;; include string.h for you.
;;; 
;;; dest :
;;; 	the destination address to copy the bytes to.
;;; 
;;; src :
;;; 	the source address to copy the bytes from.
;;; 
;;; len :
;;; 	the number of bytes to copy.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_memdup ()
;;; 
;;; gpointer g_memdup (gconstpointer mem, guint byte_size)
;;; 
;;; Allocates byte_size bytes of memory, and copies byte_size bytes into it
;;; from mem. If mem is NULL it returns NULL.
;;; 
;;; mem :
;;; 	the memory to copy.
;;; 
;;; byte_size :
;;; 	the number of bytes to copy.
;;; 
;;; Returns :
;;; 	a pointer to the newly-allocated copy of the memory, or NULL if mem is
;;;     NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GMemVTable
;;; 
;;; struct GMemVTable {
;;;   gpointer (*malloc)      (gsize    n_bytes);
;;;   gpointer (*realloc)     (gpointer mem,
;;; 			   gsize    n_bytes);
;;;   void     (*free)        (gpointer mem);
;;;   /* optional; set to NULL if not used ! */
;;;   gpointer (*calloc)      (gsize    n_blocks,
;;; 			   gsize    n_block_bytes);
;;;   gpointer (*try_malloc)  (gsize    n_bytes);
;;;   gpointer (*try_realloc) (gpointer mem,
;;; 			   gsize    n_bytes);
;;; };
;;; 
;;; A set of functions used to perform memory allocation. The same GMemVTable
;;; must be used for all allocations in the same program; a call to
;;; g_mem_set_vtable(), if it exists, should be prior to any use of GLib.
;;; 
;;; malloc ()
;;; 	function to use for allocating memory.
;;; 
;;; realloc ()
;;; 	function to use for reallocating memory.
;;; 
;;; free ()
;;; 	function to use to free memory.
;;; 
;;; calloc ()
;;; 	function to use for allocating zero-filled memory.
;;; 
;;; try_malloc ()
;;; 	function to use for allocating memory without a default error handler.
;;; 
;;; try_realloc ()
;;; 	function to use for reallocating memory without a default error handler.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_mem_set_vtable ()
;;; 
;;; void g_mem_set_vtable (GMemVTable *vtable)
;;; 
;;; Sets the GMemVTable to use for memory allocation. You can use this to
;;; provide custom memory allocation routines. This function must be called 
;;; before using any other GLib functions. The vtable only needs to provide 
;;; malloc(), realloc(), and free() functions; GLib can provide default 
;;; implementations of the others. The malloc() and realloc() implementations 
;;; should return NULL on failure, GLib will handle error-checking for you. 
;;; vtable is copied, so need not persist after this function has been called.
;;; 
;;; vtable :
;;; 	table of memory allocation routines.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_mem_is_system_malloc ()
;;; 
;;; gboolean g_mem_is_system_malloc (void)
;;; 
;;; Checks whether the allocator used by g_malloc() is the system's malloc 
;;; implementation. If it returns TRUE memory allocated with malloc() can be 
;;; used interchangeable with memory allocated using g_malloc(). This function 
;;; is useful for avoiding an extra copy of allocated memory returned by a 
;;; non-GLib-based API.
;;; 
;;; A different allocator can be set using g_mem_set_vtable().
;;; 
;;; Returns :
;;; 	if TRUE, malloc() and g_malloc() can be mixed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; glib_mem_profiler_table
;;; 
;;; extern GMemVTable *glib_mem_profiler_table;
;;; 
;;; A GMemVTable containing profiling variants of the memory allocation 
;;; functions. Use them together with g_mem_profile() in order to get 
;;; information about the memory allocation pattern of your program.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_mem_profile ()
;;; 
;;; void g_mem_profile (void)
;;; 
;;; Outputs a summary of memory usage.
;;; 
;;; It outputs the frequency of allocations of different sizes, the total number
;;; of bytes which have been allocated, the total number of bytes which have 
;;; been freed, and the difference between the previous two values, i.e. the 
;;; number of bytes still in use.
;;; 
;;; Note that this function will not output anything unless you have previously 
;;; installed the glib_mem_profiler_table with g_mem_set_vtable().
;;; ----------------------------------------------------------------------------

;;; --- End of file glib. mem.lisp ---------------------------------------------

