;; Disable installed ELPA packages, except uniquify-files
(defvar package-load-list '(all))
(push '(ada-mode . nil) package-load-list)
(push '(wisi . nil) package-load-list)

;; also disable native compilation; it just slows things down
;; (defun native-comp-available-p () nil)
(setq native-comp-deferred-compilation nil)

;; end of file
