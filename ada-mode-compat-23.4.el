;;; Implement current Emacs features not present in Emacs 23.4

(defvar compilation-filter-start (make-marker)
  "")

(defun compilation-filter-start (proc)
  ""
  (set-marker compilation-filter-start (point-max)))

(defun compilation--put-prop (matchnum prop val)
  (when (and (integerp matchnum) (match-beginning matchnum))
    (put-text-property
     (match-beginning matchnum) (match-end matchnum)
     prop val)))

;; end of file
