(in-package :discompiler)

(defmacro define-constant (name value &optional doc)
  (if (boundp name)
      (format t
              "~&already defined ~A~%old value ~s~%attempted value ~s~%"
              name (symbol-value name) value))
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +dll-folder+ "~/.wine/drive_c/windows/system32")
