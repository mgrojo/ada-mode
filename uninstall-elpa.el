;; uninstall wisitoken-grammar-mode, wisi from ~/.emacs.d/elpa, for
;; reinstall with different emacs version

(require 'package)

(package-initialize)

(setq wisitoken-grammar-mode-version (getenv "WISITOKEN_GRAMMAR_MODE_VERSION"))
(setq wisi-version (getenv "WISI_VERSION"))

(defun pkg-dir (name version)
  (concat (locate-user-emacs-file "elpa") "/" name "-" version))

(defun pkg-dir-clean (name version)
  (let ((dir (pkg-dir name version)))
    (when (file-exists-p dir)
      (delete-directory dir t))))

(pkg-dir-clean "wisitoken-grammar-mode" wisitoken-grammar-mode-version)
(pkg-dir-clean "wisi" wisi-version)

;; end of file
