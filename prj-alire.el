;; Set up building with Alire -*- no-byte-compile : t -*-

(wisi-prj-select-cache
 "prj-alire.el"
 (create-alire-prj
  :name "ada-mode main Alire"
  :gpr-file "emacs_ada_mode.gpr"
  :xref-label 'gpr_query)
 "Alire.make")

;; end of file
