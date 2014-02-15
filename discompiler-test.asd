;; (defsystem #:discompiler-test
;;   :serial t
;;   :depends-on (:discompiler :fiveam)
;;   :components ((:module "test"
;;                         :components
;;                         (
;;                          (:file "fiveam-suites")
;;                          (:file "pe-coff-tests")
;;                          (:file "elf-tests")
;;                          (:file "fiveam-conclusion")
;;                          ))))
