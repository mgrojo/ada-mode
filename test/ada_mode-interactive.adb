-- Test various things interactively; while the user is typing code.

-- This file doesn't compile; it contains deliberately incomplete code.

-- It also doesn't pass the reindent and diff test, since we are deliberately adding newlines
--EMACSCMD:(setq skip-reindent-test t)

-- Test cache managment when inserting a with clause; used to screw up
--EMACSCMD:(progn (forward-line 2)(ada-smie-validate-cache (point))(insert "with A;\n")ada-smie-cache-max)
--EMACSRESULT:(point-min)
procedure Procedure_1
is
   -- Adding text inside a comment must not leave ada-smie-cache-max inside comment
   --EMACSCMD:(progn (beginning-of-line)(forward-comment 100)(forward-char 2)(ada-smie-validate-cache (point)))
   --EMACSCMD:(progn (beginning-of-line)(forward-comment -2)(forward-word 1)(insert "some text")ada-smie-cache-max)
   --EMACSRESULT:(progn (forward-line -4)(point))

   -- Ditto string
   A : constant String :=
     "hi there!";
   --EMACSCMD:(progn (beginning-of-line)(forward-comment 100)(ada-smie-validate-cache (point)))
   --EMACSCMD:(progn (forward-line -2)(end-of-line)(forward-word -1)(insert "some text")ada-smie-cache-max)
   --EMACSRESULT:(progn (forward-line -3)(backward-char 3)(point))

   -- Newline before a blank line followed by code used to indent to 5
   --EMACSCMD:(progn (forward-line 1)(end-of-line) (newline-and-indent)(current-column))

   --EMACSRESULT:3
   -- This indents to 3
   --EMACSCMD:(progn (forward-line 1)(back-to-indentation) (newline-and-indent)(current-column))
   function Local_Function_1 return Float;
   --EMACSRESULT:3

   -- error from is-generic-p refining Function_11 while entering body of Function_Access_1
   -- fixed by 'if ada-smie-debug-refine'
   --EMACSCMD:(progn (forward-line 5)(newline-and-indent)(current-column))
   function Function_Access_1
     (A_Param : in Float)
      return
      Standard.Float

   function Function_11 return Float;

   -- Error about unbalanced parens; should indent to paren
   --EMACSCMD:(progn (forward-line 1)(end-of-line) (newline-and-indent)(current-column))
   type Wait_Return is (Read_Success,

   --EMACSRESULT:24
   --
   -- This is from before keyword; the parent of "end" is now "type"
   -- because of the missing syntax. It is tempting to try to force
   -- using after keyword first for interactive newline and indent.
   -- But we often do that to insert new code in already legal code,
   -- and there's no easy way to tell the difference.

   --EMACSCMD:(progn (forward-line 1)(back-to-indentation)(forward-char 2) (comment-indent-new-line)(current-column))
   --
   -- is; used to cause an unrecognized "is" because indent-according-to-mode is called with the comment text exposed!
   -- fixed by ada-smie-comment-indent.

   -- testing auto-case
   --EMACSCMD:(progn (end-of-line 1)(newline-and-indent)(insert "A")(char-before))

   --EMACSRESULT:?A
   --EMACSCMD:(progn (end-of-line 1)(newline-and-indent)(insert "a")(char-before))

   --EMACSRESULT:?a
   --EMACSCMD:(progn (end-of-line 1)(newline-and-indent)(execute-kbd-macro "a_b ")(let ((case-fold-search nil))(looking-back "A_B ")))

   --EMACSRESULT:t

   --EMACSCMD:(progn (end-of-line 1)(newline-and-indent)(execute-kbd-macro "foo_bar_baz ")(let ((case-fold-search nil))(looking-back "Foo_Bar_Baz ")))

   --EMACSRESULT:t
   --EMACSCMD:(progn (forward-line 1)(downcase-word 2)(execute-kbd-macro "\C-u\C-c\C-w")(let ((case-fold-search nil))(looking-back "Ada_Identifier")))
   -- Ada_Identifier in comment; force auto-case

   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(execute-kbd-macro "\n")(let ((case-fold-search nil))(looking-at "begin")))
   is begin
   --EMACSRESULT:t

   -- don't capitalize word after punctuation
   --EMACSCMD:(progn (forward-line 1)(back-to-indentation)(execute-kbd-macro "((")(let ((case-fold-search nil))(looking-at "begin")))
   begin
   --EMACSRESULT:t

      -- don't capitalize character constant
      --EMACSCMD:(progn (end-of-line 2)(backward-char 1)(execute-kbd-macro "'")(let ((case-fold-search nil))(looking-back "'z'")))
      E := 'z';

end Procedure_1;

package Package_1 is
   -- package declaration resets smie indentation to something reasonable

   --EMACSCMD:(progn (forward-line 2)(ada-make-subprogram-body))
   --EMACSAT:"procedure Proc_1\n   is begin\n     \nend Proc_1;"
   procedure Proc_1;
   
   --EMACSCMD:(progn (forward-line 2)(ada-make-subprogram-body))
   --EMACSAT:"procedure Proc_2 (A : in Integer)\n   is begin\n     \nend Proc_2;"
   procedure Proc_2 (A : in Integer);
   
   --EMACSCMD:(progn (forward-line 2)(ada-make-subprogram-body))
   --EMACSAT:"function Func_1 return Integer\n   is begin\n     \nend Func_1;"
   function Func_1 return Integer;
   
   --EMACSCMD:(progn (forward-line 2)(ada-make-subprogram-body))
   --EMACSAT:"function Func_1\n     (A : in Integer)\n     return Integer\n   is begin\n     \nend Func_1;"
   function Func_1
     (A : in Integer)
     return Integer;

   --EMACSCMD:(progn (forward-line 3)(ada-make-subprogram-body))
   -- FIXME: verify expected error? 
   procedure Proc_1;

end Package_1;

-- This used to cause problems at end of buffer; refining "procedure"
-- requires scanning forward.
--EMACSCMD:(progn (forward-line 1)(end-of-line) (newline-and-indent))
procedure Ada_Mode.Interactive_Return
