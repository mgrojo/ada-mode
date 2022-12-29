;; run tests using eglot, no wisi parser

(require 'cl-lib)

(add-to-list 'load-path "/Projects/eglot-stephe")
(require 'eglot)

(setq ada-diagnostics-backend 'eglot)
(setq ada-face-backend 	      'eglot) ;; als 23 supports semantic_tokens; FIXME: testing eglot-stephe
(setq ada-indent-backend      'eglot) ;; FIXME: testing gnatpp package in .gpr, patched als, eglot-stephe
(setq ada-statement-backend   'none)
(setq ada-xref-backend 	      'eglot)

(setq ada-eglot-require-gpr t)
(setq ada-eglot-gpr-file (expand-file-name "../test/ada_mode.gpr")) ;; individual tests can override if needed.

(setq skip-reindent-test (eq ada-indent-backend 'none))
(setq skip-recase-test   (eq ada-face-backend 'none))
(setq ada-eglot-require-gpr t)

(setq project-find-functions '(wisi-prj-current-cached))

(setq gnat-lsp-server-exec "/Projects/ada_language_server-23.1-patch/.obj/server/ada_language_server")
;; (setq
;;  gnat-lsp-server-exec
;;  (concat
;;   (cl-case system-type
;;     (gnu/linux
;;      "/Projects/alire-workspace/")
;;     (windows-nt
;;      "c:/Projects/alire/"))
;;   "ada_language_server_23.0.0_66f2e7fb/.obj/server/ada_language_server"))

(require 'run-indent-test)

;; Specific names for all the LSP token and modifier faces, to
;; make the tests clear and independent of eglot.el defaults.
(defface lsp-comment       '((t . (:inherit font-lock-comment-face))) "")
(defface lsp-keyword       '((t . (:inherit font-lock-keyword-face))) "")
(defface lsp-string        '((t . (:inherit font-lock-string-face))) "")
(defface lsp-number        '((t . (:inherit font-lock-constant-face))) "")
(defface lsp-regexp        '((t . (:inherit font-lock-string-face))) "")
(defface lsp-operator      '((t . (:inherit font-lock-function-name-face))) "")
(defface lsp-namespace     '((t . (:inherit font-lock-function-name-face))) "")
(defface lsp-type          '((t . (:inherit font-lock-constant-face))) "")
(defface lsp-struct        '((t . (:inherit font-lock-type-face))) "")
(defface lsp-class         '((t . (:inherit font-lock-type-face))) "")
(defface lsp-interface     '((t . (:inherit font-lock-type-face))) "")
(defface lsp-enum          '((t . (:inherit font-lock-type-face))) "")
(defface lsp-typeParameter '((t . (:inherit font-lock-type-face))) "")
(defface lsp-function      '((t . (:inherit font-lock-function-name-face))) "")
(defface lsp-method        '((t . (:inherit font-lock-function-name-face))) "")
(defface lsp-member        '((t . (:inherit font-lock-variable-name-face))) "")
(defface lsp-field         '((t . (:inherit font-lock-variable-name-face))) "")
(defface lsp-property      '((t . (:inherit font-lock-variable-name-face))) "")
(defface lsp-event         '((t . (:inherit font-lock-variable-name-face))) "")
(defface lsp-macro         '((t . (:inherit font-lock-preprocessor-face))) "")
(defface lsp-variable      '((t . (:inherit font-lock-variable-name-face))) "")
(defface lsp-parameter     '((t . (:inherit font-lock-variable-name-face))) "")
(defface lsp-label         '((t . (:inherit font-lock-comment-face))) "")
(defface lsp-enumConstant  '((t . (:inherit font-lock-constant-face))) "")
(defface lsp-enumMember    '((t . (:inherit font-lock-constant-face))) "")
(defface lsp-dependent     '((t . (:inherit font-lock-type-face))) "")
(defface lsp-concept       '((t . (:inherit font-lock-type-face))) "")

(setq eglot-semantic-token-faces
  '(("comment"       . lsp-comment)
    ("keyword"       . lsp-keyword)
    ("string"        . lsp-string)
    ("number"        . lsp-number)
    ("regexp"        . lsp-regexp)
    ("operator"      . lsp-operator)
    ("namespace"     . lsp-namespace)
    ("type"          . lsp-type)
    ("struct"        . lsp-struct)
    ("class"         . lsp-class)
    ("interface"     . lsp-interface)
    ("enum"          . lsp-enum)
    ("typeParameter" . lsp-typeParameter)
    ("function"      . lsp-function)
    ("method"        . lsp-method)
    ("member"        . lsp-member)
    ("field"         . lsp-field)
    ("property"      . lsp-property)
    ("event"         . lsp-event)
    ("macro"         . lsp-macro)
    ("variable"      . lsp-variable)
    ("parameter"     . lsp-parameter)
    ("label"         . lsp-label)
    ("enumConstant"  . lsp-enumConstant)
    ("enumMember"    . lsp-enumMember)
    ("dependent"     . lsp-dependent)
    ("concept"       . lsp-concept)))

(defface lsp-declaration    '((t . (:overline "green" :underline "green"))) "")
(defface lsp-definition     '((t . (:underline "black"))) "")
(defface lsp-implementation '((t . (:underline "black"))) "")
(defface lsp-readonly       '((t . (:slant reverse-italic))) "")
(defface lsp-static 	    '((t . (:slant italic))) "")
(defface lsp-abstract       '((t . (:overline "green"))) "")
(defface lsp-async 	    '((t . (:overline "blue"))) "")
(defface lsp-modification   '((t . (:overline "orange"))) "")
(defface lsp-deprecated     '((t . (:strike-through t))) "")
(defface lsp-documentation  '((t . (:weight light))) "")
(defface lsp-defaultLibrary '((t . (:overline "black"))) "")

(setq eglot-semantic-token-modifier-faces
  '(("declaration"    . lsp-declaration)
    ("definition"     . lsp-definition)
    ("implementation" . lsp-implementation)
    ("readonly"       . lsp-readonly)
    ("static" 	      . lsp-static)
    ("abstract"       . lsp-abstract)
    ("async" 	      . lsp-async)
    ("modification"   . lsp-modification)
    ("deprecated"     . lsp-deprecated)
    ("documentation"  . lsp-documentation)
    ("defaultLibrary" . lsp-defaultLibrary)))

(add-to-list 'eglot-stay-out-of "progress")

(defun face-wait ()
  ;; There is not an easy way to wait for a specific LSP message.
  (sleep-for 0.1)) ;; slow enough after indexing done

(setq test-face-wait-fn 'face-wait)

;; Make space for the indexing progress display
(setq mode-line-format (cl-delete '(vc-mode vc-mode) mode-line-format :test 'equal))

;;; end of file
