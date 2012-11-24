;;; Unit tests on some ada-mode.el functions

;;; don't need (require 'cl-macs); 'assert' is auto-loaded from there
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

(copy-file "case-exceptions-1" "case-exceptions-1.tmp")
(copy-file "case-exceptions-2" "case-exceptions-2.tmp")

(setq ada-case-exception-file '("case-exceptions-1.tmp" "case-exceptions-2.tmp"))

(ada-case-read-all-exceptions)

(test "case 1 full words"
      ada-case-full-exceptions
      (list
       (cons "OpenToken" t)
       (cons "Text_IO" t)
       ))

(test "case 1 partial words"
      ada-case-partial-exceptions
      (list
       (cons "ANother" t)
       (cons "ASCII" t)
       (cons "AUnit" t)
       ))

(ada-case-create-exception "CaMeLcase" nil nil)
(ada-case-create-exception "CaMeL" "case-exceptions-2.tmp" t)

(test "case 2 full words"
      ada-case-full-exceptions
      (list
       (cons "CaMeLcase" t)
       (cons "OpenToken" t)
       (cons "Text_IO" t)
       ))

(test "case 2 partial words"
      ada-case-partial-exceptions
      (list
       (cons "CaMeL" t)
       (cons "ANother" t)
       (cons "ASCII" t)
       (cons "AUnit" t)
       ))

;; in read-all-exceptions, merge-excpetions inverts the result of
;; read-exceptions.
(test "case 2 1.tmp"
      (ada-case-read-exceptions "case-exceptions-1.tmp")
      (list
       (list
	 (cons "Text_IO" t)
	 (cons "OpenToken" t)
	 (cons "CaMeLcase" t))
       (cons "AUnit" t)
       (cons "ASCII" t)))

(test "case 2 2.tmp"
      (ada-case-read-exceptions "case-exceptions-2.tmp")
      (list
       (list
	(cons "OpenTOKen" t))
       (cons "CaMeL" t)
       (cons "ANother" t)))

(ada-parse-prj-file "ada_mode.adp")
(ada-select-prj-file "ada_mode.adp")

(test "case 3 full words"
      ada-case-full-exceptions
      (list
       (cons "OpenToken" t)
       (cons "Text_IO" t)
       ))

(test "case 3 partial words"
      ada-case-partial-exceptions
      (list
       (cons "ANother" t)
       (cons "ASCII" t)
       (cons "AUnit" t)
       ))

;;; end of file
