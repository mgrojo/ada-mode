;; Project definitions

(wisi-prj-select-cache
 "wisitoken.prj"
 (create-ada-prj
  :name "wisitoken stephe-1"
  :compile-env
  '("SAL=../../org.stephe_leake.sal.stephe-1"))
  "Makefile")

;; Separate so can be updated in CM
(load-file "wisitoken-keys.el")

;; End of file
