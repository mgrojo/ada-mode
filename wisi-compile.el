;;; Grammar compiler for the wisent LALR parser, integrating Wisi OpenToken output.
;;
;; Copyright (C) 2012, 2013 Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; History: first experimental version Jan 2013
;;
;;; Context
;;
;; Semantic (FIXME: ref) provides an LALR parser wisent-parse. The
;; grammar used is defined by the functions
;; semantic-grammar-create-package, which reads a bison-like source
;; file and produces corresponding elisp source, and
;; wisent-compile-grammar, which generates a parser table.
;;
;; However, the algorithm used in wisent-compile-grammar cannot cope
;; with the grammar for the Ada language. So we use the OpenToken LALR
;; parser generator, which can.
;;
;; The Ada function Wisi.Generate reads the bison-like input and
;; produces corresponding Ada source code, using OpenToken. Compiling
;; and running that Ada code produces an elisp file, similar to that
;; produced by semantic-grammar-create-package.
;;
;; The elisp produced by Wisi.Generate code uses wisi-compile-grammar
;; (provided here) to generate the automaton structure required by
;; wisent-parse. wisi-compile-grammar uses functions from
;; wisent-comp.el
;;
;;;;

;; can't just 'require'; `wisent-with-context' doesn't work.
(load (locate-library "semantic/wisent/comp.el"))

(defun wisi-replace-symbols (items symbol-array)
  (let (result item)
    (while items
     (setq item (pop items))
     (cond
      ((numberp item)
       (push item result))

      ((stringp item)
       (let ((symbol (intern-soft item symbol-array)))
	 (if symbol
	     (push symbol result)
	   (error "%s not in symbol-array" (cdr item)))))

      (t
       (error "unexpected '%s'; expected numberp, stringp" (cdr item)))
      ))
   (reverse result)))

(defun wisi-replace-actions (action symbol-array)
  "Replace semantic action symbol names in ACTION with corresponding symbol from SYMBOL-ARRAY.
Return the new action."
  (let (result item)
    (while action
     (setq item (pop action))
      (cond
       ((or
      	 (memq (cdr item) '(error accept))
      	 (numberp (cdr item)))
      	(push item result))

       ((stringp (cdr item))
      	(let ((symbol (intern-soft (cdr item) symbol-array)))
      	  (if symbol
      	      (push (cons (car item) symbol) result)
      	    (error "%s not in symbol-array" (cdr item)))))

       ((listp (cdr item))
	(push (cons (car item) (wisi-replace-symbols (cdr item) symbol-array)) result))

       (t
      	(error "unexpected '%s'; expected 'error, 'accept, numberp, stringp, listp" (cdr item)))
       )
      )
   (reverse result)))

(defun wisi-semantic-action (r)
  ;; copied from comp.el, modified to use 'stack' instead of ',stack'
  ;; for stack, sp, gotos, state; just to make things clearer.
  ;;
  "Define an Elisp function for semantic action at rule R.
On entry RCODE[R] contains a vector [BODY N (NTERM I)] where BODY is the
body of the semantic action, N is the maximum number of values
available in the parser's stack, NTERM is the nonterminal the semantic
action belongs to, and I is the index of the semantic action inside
NTERM definition.  Return the semantic action symbol, which is interned in RCODE[0].
The semantic action function accepts three arguments:

- the state/value stack
- the top-of-stack index
- the goto table

And returns the updated top-of-stack index."
  (if (not (aref ruseful r))
      (aset rcode r nil)
    (let* ((actn (aref rcode r))
           (n    (aref actn 1))         ; nb of val avail. in stack
           (NAME (apply 'format "%s:%d" (aref actn 2)))
           (form (wisent-semantic-action-expand-body (aref actn 0) n))
           ($l   (car form))            ; list of $vars used in body
           (form (cdr form))            ; expanded form of body
           (nt   (aref rlhs r))         ; nonterminal item no.
           (bl   nil)                   ; `let*' binding list
           $v i j)

      ;; Compute $N and $regionN bindings
      (setq i n)
      (while (> i 0)
        (setq j (1+ (* 2 (- n i))))
        ;; Only bind $regionI if used in action
        (setq $v (intern (format "$region%d" i)))
        (if (memq $v $l)
            (setq bl (cons `(,$v (cdr (aref stack (- sp ,j)))) bl)))
        ;; Only bind $I if used in action
        (setq $v (intern (format "$%d" i)))
        (if (memq $v $l)
            (setq bl (cons `(,$v (car (aref stack (- sp ,j)))) bl)))
        (setq i (1- i)))

      ;; Compute J, the length of rule's RHS.  It will give the
      ;; current parser state at STACK[SP - 2*J], and where to push
      ;; the new semantic value and the next state, respectively at:
      ;; STACK[SP - 2*J + 1] and STACK[SP - 2*J + 2].  Generally N,
      ;; the maximum number of values available in the stack, is equal
      ;; to J.  But, for mid-rule actions, N is the number of rule
      ;; elements before the action and J is always 0 (empty rule).
      (setq i (aref rrhs r)
            j 0)
      (while (> (aref ritem i) 0)
        (setq j (1+ j)
              i (1+ i)))

      ;; Create the semantic action symbol.
      (setq actn (intern NAME (aref rcode 0)))

      ;; Store source code in function cell of the semantic action
      ;; symbol.  It will be byte-compiled at automaton's compilation
      ;; time.  Using a byte-compiled automaton can significantly
      ;; speed up parsing!
      (fset actn
            `(lambda (stack sp gotos)
               (let* (,@bl
                      ($region
                       ,(cond
                         ((= n 1)
                          (if (assq '$region1 bl)
                              '$region1
                            `(cdr (aref stack (1- sp)))))
                         ((> n 1)
                          `(wisent-production-bounds
                            stack (- sp ,(1- (* 2 n))) (1- sp)))))
                      ($action ,NAME)
                      ($nterm  ',(aref tags nt))
                      ,@(and (> j 0) `((sp (- sp ,(* j 2)))))
                      (state (cdr (assq $nterm
                                         (aref gotos
                                               (aref stack sp))))))
                 (setq sp (+ sp 2))
                 ;; push semantic value
                 (aset stack (1- sp) (cons ,form $region))
                 ;; push next state
                 (aset stack sp state)
                 ;; return new top of stack
                 sp)))

      ;; Return the semantic action symbol
      actn)))

(defun wisi-compile-grammar (grammar)
  "Compile the LALR(1) GRAMMAR; return the automaton for wisi-parse.
GRAMMAR is a list TERMINALS NONTERMS ACTIONS GOTOS, where:

TERMINALS is a list of terminal token symbols.

NONTERMS is a list of productions; each production is a
list (LHS (RHS action) ...)

ACTIONS is an array indexed by parser state, of alists indexed by
terminal tokens. The value of each item in the alists is 'error
or 'accept, state number, or production symbol. A state number
gives the next state for each terminal token after a shift
action.  A production symbol is a symbol name:index composed of
the left hand side nonterminal token of a production and an
integer giving the right hand side, used for a reduce action. The
first item in the alist must have the key 'default; it is used
when no other item matches the current terminal.

GOTOS is an array indexed by parser state, of alists giving the
new state after a reduce for each nonterminal legal in that
state.

The automaton is an array with 4 elements:

actions is a copy of the input ACTIONS, with semantic action
strings replaced by symbols from symbol-array (below).

gotos is a copy of the input GOTOS

starts is nil (for compatibility with wisent-parse)

symbol-array contains functions that implement the reduction action
and the user action for each nonterminal; the function names
match the production symbol names."
  (wisent-with-context compile-grammar
    (wisent-parse-grammar;; set global vars used by wisent-semantic-action
     (cons
      (nth 0 grammar);; TOKENS
      (cons nil ;; ASSOCS
	    (nth 1 grammar));; NONTERMS
      ))

    (aset rcode 0 (make-vector 13 0));; obarray for semantic actions

    ;; create semantic action functions, interned in rcode[0]
    (let* ((i 1))
      (while (<= i nrules)
	(wisi-semantic-action i)
	(setq i (1+ i)))
      )

    ;; replace semantic actions in ACTIONS with symbols from symbol-array
    (let ((nactions (length (nth 2 grammar)))
	  (actions (nth 2 grammar))
	  (symbol-array (aref rcode 0))
	  (i 0))
      (while (< i nactions)
	(aset actions i
	      (wisi-replace-actions (aref actions i) symbol-array))
	(setq i (1+ i)))
      (vector
       actions
       (nth 3 grammar)
       nil ;; starts
       symbol-array)
      )))

(provide 'wisi-compile)

;;;; end of file
