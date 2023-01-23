;; Set up building with Alire -*- no-byte-compile : t -*-

(wisi-prj-select-cache
 "prj-alire.el"
 (create-alire-prj
  :name "wisitoken main Alire"
  :gpr-file "build/wisitoken_alire_mains.gpr"
  :xref-label 'gpr_query)
 "Alire.make")

;; end of file
