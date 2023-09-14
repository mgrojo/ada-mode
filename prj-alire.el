;; Set up building with Alire -*- no-byte-compile : t -*-

(wisi-prj-select-cache
 "prj-alire.el"
 (create-alire-prj
  :name "sal main Alire"
  :gpr-file "build/stephes_ada_library.gpr"
  :xref-label 'gpr_query)
 "Alire.make")

;; end of file
