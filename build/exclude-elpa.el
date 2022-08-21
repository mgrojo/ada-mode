;; Disable installed ELPA packages, except uniquify-files
(defvar package-load-list '(all))
(push '(gnat-compiler . nil) package-load-list)
(push '(gpr-mode . nil) package-load-list)
(push '(wisi . nil) package-load-list)

;; also disable native compilation; it just slows things down
(setq native-comp-deferred-compilation nil)

;; end of file
