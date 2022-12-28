;; Disable installed ELPA packages
(defvar package-load-list '(all))
(push '(wisi . nil) package-load-list)
(push '(gnat-compiler . nil) package-load-list)
(push '(gpr-query . nil) package-load-list)
(push '(ada-mode . nil) package-load-list)
(push '(gpr-mode . nil) package-load-list)
;; end of file
