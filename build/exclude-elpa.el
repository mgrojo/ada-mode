;; Disable installed ELPA packages, except uniquify-files
(defvar package-load-list '(all))
(push '(ada-mode . nil) package-load-list)
(push '(wisi . nil) package-load-list)

;; also disable native compilation; it just slows things down
(setq native-comp-speed -1)

;; end of file
