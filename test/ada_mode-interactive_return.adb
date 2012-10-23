-- Test indenting after interactive return; while the user is typing code.
-- Only cases that have actually occured in practice with Ada mode 5.0.

-- This file doesn't compile; it contains deliberately incomplete code.

-- It also doesn't pass the reindent and diff test, since we are deliberately adding newlines
--EMACSCMD:(setq skip-reindent-test t)
procedure Procedure_1
is
   -- Newline before a blank line followed by code used to indent to 5
   --EMACSCMD:(progn (forward-line 1)(end-of-line) (newline-and-indent)(current-column))

   --EMACSRESULT:3
   -- This indents to 3
   --EMACSCMD:(progn (forward-line 1)(back-to-indentation) (newline-and-indent)(current-column))
   function Local_Function_1 return Float;
   --EMACSRESULT:3

   -- Error about unbalanced parens; should indent to paren
   --EMACSCMD:(progn (forward-line 1)(end-of-line) (newline-and-indent)(current-column))
   type Wait_Return is (Read_Success,

   --EMACSRESULT:3
   --
   -- This is from before keyword; the parent of "end" is now "type"
   -- because of the missing syntax. It is tempting to try to force
   -- using after keyword first for interactive newline and indent.
   -- But we often do that to insert new code in already legal code,
   -- and there's no easy way to tell the difference.

   --EMACSCMD:(progn (forward-line 1)(back-to-indentation)(forward-char 2) (comment-indent-new-line)(current-column))
   --
   -- is; used to cause an unrecognized "is" because indent-according-to-mode is called with the comment text exposed!
   -- fixed by ada-indent-comment-indent.
end Procedure_1;

-- This used to cause problems; refining "procedure" requires scanning forward.
--EMACSCMD:(progn (forward-line 1)(end-of-line) (newline-and-indent))
procedure Ada_Mode.Interactive_Return
  
