;; copied from /Project/cedet-1.1/semantic/wisent/grammar-make-script (created by that Makefile)
;; fixed load paths to work from here
(add-to-list 'load-path nil)
(add-to-list 'load-path "/Projects/cedet-1.1/semantic/wisent")
(add-to-list 'load-path "/Projects/cedet-1.1/speedbar")
(add-to-list 'load-path "/Projects/cedet-1.1/common")
(add-to-list 'load-path "/Projects/cedet-1.1/semantic/bovine")
(add-to-list 'load-path "/Projects/cedet-1.1/semantic")
(add-to-list 'load-path "/Projects/cedet-1.1/ede")
(add-to-list 'load-path "/Projects/cedet-1.1/eieio")
(require 'eieio)
(require 'semantic-load)
(require 'semantic-grammar)
