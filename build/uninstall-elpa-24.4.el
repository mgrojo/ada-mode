;; see uninstall-elpa.el

(defun pkg-dir (name version)
  (concat (locate-user-emacs-file "elpa") "/" name "-" version))

(defun pkg-dir-clean (name version)
  (let ((dir (pkg-dir name version)))
    (when (file-exists-p dir)
      (delete-directory dir t))))

(pkg-dir-clean "ada-mode" ada-mode-version)
(pkg-dir-clean "ada-ref-man" ada-ref-man-version)
(pkg-dir-clean "wisi" wisi-version)

;; end of file
