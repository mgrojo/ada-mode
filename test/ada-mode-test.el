;;; Unit tests on some ada-mode.el functions

;;; don't need (require 'cl-macs); 'assert' is auto-loaded from there
(require 'ada-mode)

(defun test (label computed expected)
  (assert (equal computed expected) nil label))

(setq ada-other-file-alist
  '(("\\.ads$" (".adb"))
    ("\\.adb$" (".ads"))))

;; new orthogonal extensions
(ada-add-extensions ".adaspec" ".adabody")
(test "ada-add-extensions 1"
     ada-other-file-alist
     '(("\\.adaspec$" (".adabody"))
       ("\\.adabody$" (".adaspec"))
       ("\\.ads$" (".adb"))
       ("\\.adb$" (".ads"))))

(ada-add-extensions ".ads" ".adb.gp")
(test "ada-add-extensions 2"
     ada-other-file-alist
     '(("\\.adb\\.gp$" (".ads"))
       ("\\.adaspec$" (".adabody"))
       ("\\.adabody$" (".adaspec"))
       ("\\.ads$" (".adb.gp" ".adb"))
       ("\\.adb$" (".ads"))))

;;; end of file
