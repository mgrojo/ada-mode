;; Byte compile ada-mode files

;; Disable ELPA ada-mode
(defvar package-load-list '(all))
(push '(ada-mode . nil) package-load-list)
(push '(wisi . nil) package-load-list)

(package-initialize)
(setq byte-compile-error-on-warn t)
(batch-byte-compile)

;; end of file
