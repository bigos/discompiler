
(defsystem #:discompiler
  :serial t
  :version "0.0.1"
  :author "Jacek Podkanski"
  :licence "GPLv3"
  :components ((:file "packages") 	  
	       (:file "discompiler")
               (:file "tests"))
  :description "machine code experiment"
  :long-description "experimenting with Intel machine code"
  )
