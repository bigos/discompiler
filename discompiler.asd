(asdf:defsystem #:discompiler

  :version "0.0.2"
  :author "Jacek Podkanski"
  :licence "GPLv3"
  :depends-on (:lisp-unit :cl-ppcre :cl-fad :cl-utilities :md5)
  :components ((:file "packages")
               (:file "constants")
               (:module "src" :components
                        (
                         (:module "pecoff-executable" :components
                                  (
                                   (:file "pe-header")
                                   (:file "coff-header")
                                   (:file "optional-header")
                                   )
                                  )

                         (:file "section-table")
                         (:file "byte-utilities")
                         (:file "reference-data")
                         (:file "summary-table-columns")
                         (:file "header-info")))
               (:module "test"
                        :components ((:file "lisp-unit")
                                     (:file "tests")))
               (:file "discompiler")
               (:file "loader"))
  :description "machine code experiment"
  :long-description "experimenting with Intel machine code")
