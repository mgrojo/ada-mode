;; see uninstall-elpa.el

(when (package--dir "ada-mode" ada-mode-version)
  (package-delete "ada-mode" ada-mode-version))

(when (package--dir "ada-ref-man" ada-ref-man-version)
  (package-delete "ada-ref-man" ada-ref-man-version))

(when (package--dir "wisi" wisi-version)
  (package-delete "wisi" wisi-version))

;; end of file
