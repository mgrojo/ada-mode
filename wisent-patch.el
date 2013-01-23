;;; Patches to wisent code in Emacs 24.2.91

(require 'semantic/wisent/wisent); so we overload it, not vice versa

;; fix this to compute f, l to match $regioni:
;; $regioni is a nested list ((f l)), not a cons
;; odd stack items are (symbol (f l))
(defsubst wisent-production-bounds (stack i j)
  "Determine the start and end locations of a production value.
Return a pair (START . END), where START is the first available start
location, and END the last available end location, in components
values of the rule currently reduced.
Return nil when no component location is available.
STACK is the parser stack.
I and J are the indices in STACK of respectively the value of the
first and last components of the current rule.
This function is for internal use by semantic actions' generated
lambda-expression."
  (let ((f (caadr (aref stack i)))
        (l (cadadr (aref stack j))))
    (while (/= i j)
      (cond
       ((not f) (setq f (cadr (aref stack (setq i (+ i 2))))))
       ((not l) (setq l (cadr (aref stack (setq j (- j 2))))))
       ((setq i j))))
    (and f l (list (list f l)))))

(provide 'wisent-patch)
;;; end of file
