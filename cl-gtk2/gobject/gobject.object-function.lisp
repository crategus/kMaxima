(in-package :gobject)

(defcstruct object-func-ref
  (:object :pointer)
  (:fn-id :int))

(defmacro define-cb-methods (name return-type (&rest args))
  (flet ((make-name (control-string) (intern (format nil control-string (symbol-name name)) (symbol-package name))))
    (let ((call-cb (make-name "~A-CB"))
          (destroy-cb (make-name "~A-DESTROY-NOTIFY"))
          (object (gensym "OBJECT"))
          (fn-id (gensym "FN-ID"))
          (fn (gensym "FN"))
          (data (gensym "DATA"))
          (arg-names (mapcar #'first args)))
      `(progn
         (defcallback ,call-cb ,return-type (,@args (,data :pointer))
           (let* ((,object (convert-from-foreign (foreign-slot-value ,data 'object-func-ref :object) 'g-object))
                  (,fn-id (foreign-slot-value ,data 'object-func-ref :fn-id))
                  (,fn (retrieve-handler-from-object ,object ,fn-id)))
             (funcall ,fn ,@arg-names)))
         (defcallback ,destroy-cb :void ((,data :pointer))
           (let* ((,object (convert-from-foreign (foreign-slot-value ,data 'object-func-ref :object) 'g-object))
                  (,fn-id (foreign-slot-value ,data 'object-func-ref :fn-id)))
             (delete-handler-from-object ,object ,fn-id))
           (foreign-free ,data))))))

(defun create-fn-ref (object function)
  (let ((ref (foreign-alloc 'object-func-ref))
        (fn-id (save-handler-to-object object function)))
    (setf (foreign-slot-value ref 'object-func-ref :object)
          (pointer object)
          (foreign-slot-value ref 'object-func-ref :fn-id)
          fn-id)
    ref))
