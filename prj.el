;; Project definitions for compiling gnat-compiler

(let ((wisitoken-core "/Projects/org.wisitoken")
      (wisi-core "/Projects/org.emacs.wisi"))
  (wisi-prj-select-cache
   (expand-file-name "prj.el")
   (make-wisi-prj
    :name "gnat-compiler main"
    :source-path (list default-directory)
    :compile-env (list (concat "WISI=" (expand-file-name "../org.emacs.wisi.stephe-4"))))
   "Makefile"
   ))

;; end of file
