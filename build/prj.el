;; Project definitions for compiling gpr-mode

(unless (memq #'wisi-prj-find-dominating-cached project-find-functions)
  (add-hook 'project-find-functions #'wisi-prj-find-dominating-cached -10)
  (add-hook 'xref-backend-functions #'wisi-prj-xref-backend 10))

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
