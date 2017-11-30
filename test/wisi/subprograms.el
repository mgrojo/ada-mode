;; elisp declarations for the "subpgrograms" psuedo-language used for testing wisi
;; only used by ../../build/run-indent-test.el

(require 'wisi-process-parse)

;; These must match subprograms_wisi_runtime.ads
(defvar subp-indent 3 "base indent")
(defvar subp-indent-broken 2 "continuation indent")
(defvar subp-indent-comment-col-0 t "see ’wisi-indent-comment-col-0’")


(cl-defstruct (subprograms-wisi-parser (:include wisi-process--parser))
  "subprograms parser"
  ;; no new slots
  )

(cl-defmethod wisi-parse-format-language-options ((parser subprograms-wisi-parser))
  (format "%d %d %d" subp-indent subp-indent-broken (if subp-indent-comment-col-0 1 0)))

(defun subp-indent-function ()
  "Simple function to test translation to Ada"
  subp-indent-broken)

(provide 'subprograms)
;; end of file
