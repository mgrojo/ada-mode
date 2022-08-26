;; Project definitions for compiling gpr-mode

(wisi-prj-select-cache
 "gnat-compiler.prj"
 (make-wisi-prj
  :name "gnat-compiler main"
  :compile-env
  (list
   (concat "WISI=" (expand-file-name "../../org.emacs.wisi.stephe-4"))
   ))
 "Makefile")

;; end of file
