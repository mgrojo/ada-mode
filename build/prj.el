;; Project definitions

(wisi-prj-select-cache
 "wisitoken.prj"
 (create-ada-prj
  :name "wisitoken main"
  :compile-env
  '("SAL=../../org.stephe_leake.sal"))
  "Makefile")

;; Separate so can be updated in CM
(load-file "wisitoken-keys.el")

;; End of file
