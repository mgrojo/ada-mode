--  Abstract :
--
--  ada_mode_gps_indent bug #1: wisi-validate-cache: parse failed
--
--  When using C-M-e end-of-defun.
--
--  Also enable wisi-parser-based syntax highlighting.

--EMACSCMD:(jit-lock-fontify-now)
--EMACSCMD:(test-face "with" font-lock-keyword-face)
--EMACSCMD:(test-face "Ada" font-lock-function-name-face)
with Ada.Strings.Unbounded;
package Bug_001 is
   --EMACSCMD: (progn (forward-line -1)(end-of-defun)(looking-at "; -- Bug_001"))
   --EMACSRESULT: t
private
end Bug_001; -- Bug_001
