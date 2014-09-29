;; see install-elpa.el

(defun pkg-dir (name version)
  (concat (locate-user-emacs-file "elpa") "/" name "-" version))

(defun pkg-dir-clean (name version)
  (let ((dir (pkg-dir name version)))
    (when (file-exists-p dir)
      (delete-directory dir t))))

;; We may be installing a newer version without a version bump, so
;; just delete the packages.
(pkg-dir-clean "ada-mode" ada-mode-version)
(pkg-dir-clean "ada-ref-man" ada-ref-man-version)
(pkg-dir-clean "wisi" wisi-version)

(defun pkg-download (name version)
  (let ((default-directory (pkg-dir name version))
	(pkg-desc
	 (package-desc-create
	  :name (intern name)
	  :version (version-to-list version)
	  :kind 'tar
	  :archive "test")))
    (package-install-from-archive pkg-desc)))

;; download in dependency order
(pkg-download "wisi" wisi-version)
(pkg-download "ada-mode" ada-mode-version)
(pkg-download "ada-ref-man" ada-ref-man-version)

;; end of file
