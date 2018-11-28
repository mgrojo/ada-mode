;; see install-elpa.el

(defun pkg-dir (name version)
  (concat (locate-user-emacs-file "elpa") "/" name "-" version))

(defun pkg-download (name version &optional archive kind)
  (let ((default-directory (pkg-dir name version))
	(pkg-desc
	 (package-desc-create
	  :name (intern name)
	  :version (version-to-list version)
	  :kind (or kind 'tar)
	  :archive (or archive "test"))))
    (package-install-from-archive pkg-desc)))

;; download in dependency order
(when (< emacs-major-version 25)
  (pkg-download "seq" "2.20" "gnu" 'tar))
(pkg-download "wisi" wisi-version)
(pkg-download "ada-mode" ada-mode-version)
(pkg-download "ada-ref-man" ada-ref-man-version)

;; end of file
