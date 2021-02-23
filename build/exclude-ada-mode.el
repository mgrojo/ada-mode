;; Disable installed ELPA ada-mode, wisi
(defvar package-load-list '(all))
(push '(ada-mode . nil) package-load-list)
(push '(wisi . nil) package-load-list)
;; end of file
