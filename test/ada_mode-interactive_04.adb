-- Split out from ada_mode-interactive_1.adb, for debugging; used to
-- encounter a bug in Left_Breakdown, and lose a new_line, and found a
-- bug in Edit_Tree when removing '--'.

procedure Ada_Mode.Interactive_04
is
   E : Character;
begin

   -- This edit caused the lost new_line; it removes the '--' from a
   -- comment, but the comment text was not lexed by Edit_Tree.
   --
   --EMACSCMD:(progn (back-to-indentation)(delete-char 2)(wisi-indent-statement))
   begin
      --EMACSCMD:(progn (forward-line -2)(back-to-indentation)(insert "--"))

      E := 'z';
   end;

   -- This edit encountered a bug in Left_Breakdown, due to an empty
   -- nonterm. 'null;' inserted before 'end loop;'
   --
   --EMACSCMD:(progn (end-of-line 2)(kill-line 3)(newline-and-indent)(insert "end loop;")(newline-and-indent))
   for Char of String'("ABcd") loop
      null;
   end loop;
   --EMACSCMD:(progn (end-of-line -1)(newline-and-indent)(insert "null;"))

   E := 'A';

end Ada_Mode.Interactive_04;
