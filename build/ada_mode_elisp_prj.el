;; Project for byte-compiling elisp code.

(require 'elisp-project)

(elisp-project
 "Ada mode elisp"
 (list
  (expand-file-name "../..")
  (expand-file-name "..")
  (expand-file-name "../../test/wisi")))

;; end of file
