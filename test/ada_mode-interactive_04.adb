-- Split out from ada_mode-interactive_1.adb, for debugging; used to
-- encounter a bug in Left_Breakdown, and lose a new_line, and found a
-- bug in Edit_Tree when removing '--'.

procedure Ada_Mode.Interactive_04
is
   Obj_1 : access String;
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
   --EMACSCMD:(progn (end-of-line 2)(kill-line 2)(newline-and-indent)(insert "end loop;")(newline-and-indent))
   for File_Name in File_Names loop
      end loop;

   E := (1 => 'A');

end Ada_Mode.Interactive_04;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- End:
