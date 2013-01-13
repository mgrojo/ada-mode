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

(require 'semantic/wisent/comp)

(defun wisi-compile-grammar (grammar actions gotos)
  "Compile the LALR(1) GRAMMAR, assumed produced by Wisi.Generate; return the automaton for wisi-parse.
GRAMMAR is a list (TOKENS ASSOCS . NONTERMS)) as expected by
`wisent-parse-grammar'. ACTIONS and GOTOS should be the output of
the OpenToken Wisi parser generator; ACTIONS is an array, indexed
by state, of alists indexed by terminal tokens. The content of
each alist item is one of 'error, 'accept, ('shift . state),
or ('reduce . production), where state is an integer, and
production is a symbol name:index composed of the left hand side
nonterminal token of a production and an integer giving the right
hand side; `wisent-compile-grammar' compiles the semantic action
into that symbol. GOTOS is an array, indexed by state, of alists
giving the new state after a reduce for each nonterminal legal in
that state. The production used in the reduce is compiled by
`wisent-parse-grammar' along with the user action. The first
NONTERMS is the accept symbol."
  (wisent-with-context compile-grammar
    (setq wisent-new-log-flag t)
    ;; Parse input grammar
    (wisent-parse-grammar grammar nil)

    (list actions gotos )))

(provide 'wisi-compile)

;;;; end of file
