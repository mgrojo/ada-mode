;; Project definitions -*-no-byte-compile: t; -*-

(wisi-prj-select-cache
 "wisitoken.prj"
 (create-ada-prj
  :name "wisitoken main"
  :compile-env
  (list
   (concat "SAL=" (expand-file-name "../../org.stephe_leake.sal.stephe-2" (file-name-directory load-file-name)))))
  "Makefile")

;; Separate so can be updated in CM
(load-file "wisitoken-keys.el")

;; End of file
