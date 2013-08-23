
(defsystem #:discompiler
  :serial t
  :version "0.0.1"
  :author "Jacek Podkanski"
  :licence "GPLv3"
  :depends-on (:lisp-unit :cl-ppcre :cl-fad :cl-utilities)
  :components ((:file "packages") 	  
	       (:file "discompiler")
               (:file "lisp-unit")
               (:file "tests"))
  :description "machine code experiment"
  :long-description "experimenting with Intel machine code"
  )
