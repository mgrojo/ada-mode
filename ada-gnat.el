;; Ada mode functionality specific to the GNAT compiler
;;
;; FIXME: file header, GNAT web link

(defun ada-indent-gnatprep ()
  "If point is on a gnatprep keyword, return indentation column
for it. Otherwise return nil. Intended to be added to
`smie-indent-functions'."
  ;; gnatprep keywords are:
  ;;
  ;; #if identifier [then]
  ;; #elsif identifier [then]
  ;; #else
  ;; #end if;
  ;;
  ;; they are all indented at column 0.
  (when (equal (char-after) ?\#) 0))

(provide 'ada-gnat)
;; end of file
