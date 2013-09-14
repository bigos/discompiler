(in-package :discompiler)

(defmacro define-constant (name value &optional doc)
  (if (boundp name)
      (unless (equalp (symbol-value name) value)
        (format t
         "~&warning: constant ~A defined as:~&~s attempted redefining as:~&~s~%"
         name (symbol-value name) value)))
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +dll-folder+ "~/Programming/Lisp/discompiler/Test executables/")

(define-constant +ms-dos-header-size+ 64 )
