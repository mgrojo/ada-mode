;;; Unit tests on some ada-mode.el functions
;;
;; when running interactively:
;; (setq default-directory "c:/Projects/org.emacs.ada-mode.stephe-1/build/wisi")

;; Default includes mtn, among others, which is broken in Emacs 22.2, 24.3
(setq vc-handled-backends '(CVS))

(package-initialize) ;; for queue

(require 'cl);; assert
(require 'ada-mode)

(defun test (label computed expected)
  (assert (equal computed expected) nil label))

;;; other-file tests
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

;;; casing exceptions tests
(when (file-exists-p "case-exceptions-1.tmp")
  (delete-file "case-exceptions-1.tmp"))
(when (file-exists-p "case-exceptions-2.tmp")
  (delete-file "case-exceptions-2.tmp"))

(copy-file "../test/case-exceptions-1" "case-exceptions-1.tmp")
(copy-file "../test/case-exceptions-2" "case-exceptions-2.tmp")

;; ada-case-read-all-exceptions only reads from project variable; duplicate here.
(setq ada-case-full-exceptions nil)
(setq ada-case-partial-exceptions nil)
(dolist (file '("case-exceptions-1.tmp" "case-exceptions-2.tmp"))
  (ada-case-merge-all-exceptions (ada-case-read-exceptions file)))

(test "case 1 full words"
      ada-case-full-exceptions
      (list
       (cons "GDS" t)
       (cons "GPL" t)
       (cons "OpenToken" t)
       (cons "SAL" t)
       (cons "Text_IO" t)
       ))

(test "case 1 partial words"
      ada-case-partial-exceptions
      (list
       (cons "ANother" t)
       (cons "ASCII" t)
       (cons "AUnit" t)
       (cons "DOF" t)
       (cons "ID" t)
       (cons "IO" t)
       ))

(ada-case-create-exception "CaMeLcase" "case-exceptions-1.tmp" nil)
(ada-case-create-exception "CaMeL" "case-exceptions-2.tmp" t)

(test "case 2 full words"
      ada-case-full-exceptions
      (list
       (cons "CaMeLcase" t)
       (cons "GDS" t)
       (cons "GPL" t)
       (cons "OpenToken" t)
       (cons "SAL" t)
       (cons "Text_IO" t)
       ))

(test "case 2 partial words"
      ada-case-partial-exceptions
      (list
       (cons "CaMeL" t)
       (cons "ANother" t)
       (cons "ASCII" t)
       (cons "AUnit" t)
       (cons "DOF" t)
       (cons "ID" t)
       (cons "IO" t)
       ))

;; in read-all-exceptions, merge-exceptions inverts the result of
;; read-exceptions.
(test "case 2 1.tmp"
      (ada-case-read-exceptions "case-exceptions-1.tmp")
      (list
       (list
	 (cons "Text_IO" t)
	 (cons "SAL" t)
	 (cons "OpenToken" t)
	 (cons "GPL" t)
	 (cons "GDS" t)
	 (cons "CaMeLcase" t))
       (cons "IO" t)
       (cons "ID" t)
       (cons "DOF" t)
       (cons "AUnit" t)
       (cons "ASCII" t)))

(test "case 2 2.tmp"
      (ada-case-read-exceptions "case-exceptions-2.tmp")
      (list
       (list
	(cons "OpenTOKen" t))
       (cons "CaMeL" t)
       (cons "ANother" t)))

(ada-select-prj-file "../test/subdir/ada_mode.adp")

(test "case 3 full words"
      ada-case-full-exceptions
      (list
       (cons "GDS" t)
       (cons "GPL" t)
       (cons "OpenToken" t)
       (cons "SAL" t)
       (cons "Text_IO" t)
       ))

(test "case 3 partial words"
      ada-case-partial-exceptions
      (list
       (cons "ANother" t)
       (cons "ASCII" t)
       (cons "AUnit" t)
       (cons "DOF" t)
       (cons "ID" t)
       (cons "IO" t)
       ))

;;; end of file
