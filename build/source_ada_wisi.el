;; Load ada-mode, wisi from source/devel workspaces. Used when ELPA
;; packages not installed

(message "hello from source_ada_wisi.el")

(let ((dir "c:/Projects/org.emacs.wisi"))
  (add-to-list 'load-path dir)
  (load-file (expand-file-name "autoloads.el" dir)))

(let ((dir "c:/Projects/org.emacs.ada-mode"))
  (add-to-list 'load-path dir)
  (load-file (expand-file-name "autoloads.el" dir)))

;; end of file
