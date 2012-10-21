-- Test indenting after interactive return; while the user is typing code.

-- This file doesn't compile; it contains deliberately incomplete code.

-- It also doesn't pass the reindent and diff test, since we are deliberately adding newlines
--EMACSCMD:(setq skip-reindent-test t)
procedure Procedure_1
is
   -- Newline before a blank line followed by code used to indent to 5
   --EMACSCMD:(progn (forward-line 1)(goto-char (line-end-position)) (newline-and-indent)(current-column))

   --EMACSRESULT:3
   -- This indents to 3
   --EMACSCMD:(progn (forward-line 1)(back-to-indentation) (newline-and-indent)(current-column))
   function Local_Function_1 return Float;
   --EMACSRESULT:3
end Procedure_1;

-- This used to cause problems; refining "procedure" requires scanning forward.
--EMACSCMD:(progn (forward-line 1)(goto-char (line-end-position)) (newline-and-indent))
procedure Ada_Mode.Interactive_Return
  
