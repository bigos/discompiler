(in-package :discompiler)

(defmacro define-constant (name value &optional doc)
  (if (boundp name)
      (progn
        (if (not (equalp (symbol-value name) value)) 
            (format 
             t "~&warning: constant ~A defined as~&~s attempted value~&~s~%"
             name (symbol-value name) value)
            (format
             t "~&redefining constant ~S with the same value ~S~%"
             name value))
        `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
           ,@(when doc (list doc))))))

(define-constant +dll-folder+ "~/.wine/drive_c/windows/system32")
