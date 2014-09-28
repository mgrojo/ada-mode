;; see install-elpa.el

;; We may be installing a newer version without a version bump, so
;; just delete the packages.
(when (package--dir "ada-mode" ada-mode-version)
  (package-delete "ada-mode" ada-mode-version))

(when (package--dir "ada-ref-man" ada-ref-man-version)
  (package-delete "ada-ref-man" ada-ref-man-version))

(when (package--dir "wisi" wisi-version)
  (package-delete "wisi" wisi-version))

(package-refresh-contents)
(package-install 'wisi)
(package-install 'ada-mode)
(package-install 'ada-ref-man)

;; end of file
