-- Test various things interactively; while the user is typing code.
-- See also ada_mode-interactive_*.adb

-- It doesn't compile; missing bodies, undefined vars.

-- It also doesn't pass the reindent and diff test, since we are deliberately adding code
--EMACSCMD:(setq skip-reindent-test t)

-- We don't disable the casing test; that is important during interactive editing.

-- Test the buffer does parse
--EMACSCMD:(progn (wisi-parse-buffer 'face) (length (wisi-parser-parse-errors wisi--parser)))
--EMACSRESULT:0

-- FIXME: At EOB, insert token that is deleted by error recovery, then
-- extended by next edit; special case at EOB.


procedure Ada_Mode.Interactive_02
is
   -- Newline before a blank line
   --EMACSCMD:(progn (end-of-line 2)(newline-and-indent)(current-column))

   --EMACSRESULT:3

   -- newline before code
   --EMACSCMD:(progn (forward-line 1)(back-to-indentation) (newline-and-indent)(current-column))
   function Local_Function_1 return Float;
   --EMACSRESULT:3

   -- New comment; parse after first "-" reports syntax error.
   -- Error should be cleared after second "-". First "-" is deleted by
   -- error recovery, second "-" should be part of that token; must be
   -- handled by Edit_Tree.
   --
   --EMACSCMD:(progn (end-of-line 2)(newline-and-indent))
   --EMACSCMD:(progn (end-of-line 2)(execute-kbd-macro "-")(indent-for-tab-command)(length (wisi-parser-parse-errors wisi--parser)))

   --EMACSRESULT: 1
   --EMACSCMD:(progn (end-of-line -2)(execute-kbd-macro "-")(indent-for-tab-command)(length (wisi-parser-parse-errors wisi--parser)))
   --EMACSRESULT: 0
   --EMACSCMD:(progn (beginning-of-line -4)(kill-line 1))


   -- New parameter in a subpgrogram. See comment in ada_mode-parens.adb
   -- Local_10 on hanging paren indent; this is indented as code,
   -- because error recover inserts the missing parameter spec.
   --
   --EMACSCMD:(progn (end-of-line 2)(forward-char -2)(execute-kbd-macro ";\r")(current-column))
   procedure Local_Proc_1 (Param_1 : in Float);
   --EMACSRESULT:27

   -- We do _not_ clean up the syntax error; parser must tolerate errors
   -- like this and still give good results.

   -- No test here; just let resume finish from previous error recover
   -- before next edit.
   procedure Local_Proc_2 (Param_2 : in Integer);

   -- Test that adding a body interactively leaves it properly indented,
   -- and navigation text properties correct. We start with invalid
   -- syntax (missing final ';'), automatic indent after syntax fixed
   -- should indent entire statement correctly. Note that this requires
   -- the function name after 'end', since ada-end-name-optional is set
   -- nil below.
   --
   -- Indentation of 'null;' before 'end' is inserted is somewhat
   -- random, due to error correction. Error correction finds multiple
   -- solutions to the error; one inserts 'end;' to complete the
   -- subprogram_body as desired, but others delete 'begin' and complete
   -- a subprogram_body_stub, or delete 'is begin' and complete a
   -- subprogram_specification. Each of those solutions result in
   -- different indentations (6, 5, 3). They all eventuallly lead to
   -- identical parser stacks, where one is arbitrarily dropped.
   --
   -- After 'end Function_Access_1;' is inserted, there is no error, so
   -- there is only one possible indentation for 'null;'.

   --EMACSCMD:(progn (end-of-line 7)(delete-char -2)(newline-and-indent))
   --EMACSCMD:(progn (end-of-line 6)(execute-kbd-macro "is begin\rnull;\rend Function_Access_1;\r")(current-indentation))
   --EMACSRESULT:3
   function Function_Access_1
     (A_Param : in Float)
     return Standard.Float;

   --EMACSCMD:(progn (forward-line -4)(current-indentation))
   --EMACSRESULT:3
   --EMACSCMD:(progn (forward-line -5)(current-indentation))
   --EMACSRESULT:6
   --EMACSCMD:(progn (forward-line -6)(current-indentation))
   --EMACSRESULT:3
   --EMACSCMD:(progn (forward-line -8)(ada-goto-declaration-start)(looking-at "function Function_Access_1"))
   --EMACSRESULT:t
   --EMACSCMD:(progn (forward-line -15)(forward-word 1)(ada-goto-declaration-end)(looking-back "end Function_Access_1"))
   --EMACSRESULT:t

   -- calling ada-make-subprogram-body tested in ada_mode-interactive_common.adb

   -- New_Line before 'end'
   --EMACSCMD:(progn (forward-line 6)(delete-char 11)(indent-for-tab-command)(current-column))
   --EMACSRESULT:6
   function Function_Access_2
     (A_Param : in Float)
     return Standard.Float
   is begin
      null;
   end Function_Access_2;
   --EMACSCMD:(progn (forward-line -2)(execute-kbd-macro "null;")(indent-for-tab-command))

   -- New_Line before 'end case', for 'end if;'
   --EMACSCMD:(progn (forward-line 8)(delete-char 19)(indent-for-tab-command)(current-column))
   --EMACSRESULT:15
   procedure New_Line_2
   is begin
      case Command is
         when Time =>
            if Time_Start_Set then
               Time_End := Ada.Calendar.Clock;
            end if;
      end case;
   end New_Line_2;
   --EMACSCMD:(progn (forward-line -3)(execute-kbd-macro "end if;")(indent-for-tab-command))

   -- add an enumeration value in parens
   --EMACSCMD:(progn (end-of-line 4)(backward-char 2) (execute-kbd-macro ",\rWrite_Success")(indent-for-tab-command)(current-indentation))
   --EMACSRESULT:6
   type Wait_Return is
     (Read_Success);

   -- Typing a new type declaration; indent on new blank line should be
   -- correct for component.
   --
   --EMACSCMD:(progn (end-of-line 2)(kill-line 3)(execute-kbd-macro "\r")(current-indentation))
   type Record_1 is record
      Component_1 : Integer;
   end record;

   --EMACSRESULT:6
   --EMACSCMD:(progn (end-of-line -1)(execute-kbd-macro "Component_1 : Integer;\r end record;\r"))

begin
   --  extending block; no errors
   --EMACSCMD:(progn (forward-line 3)(kill-line 1)(forward-line 1)(yank))
   begin
      Stuff_2;
   end;
   Stuff_3;

   -- Typing code after missing semicolon.
   --
   --EMACSCMD:(progn (end-of-line 2)(kill-word 1)(delete-char 1)(execute-kbd-macro "\r+ C;")(current-indentation))
   A := B
     + C;

   --EMACSRESULT:5

   -- Typing comment after missing parens. Bad indent due to blank line
   -- and comment confusing our heuristic.
   --
   --EMACSCMD:(progn (forward-line 3)(forward-word 1)(forward-char 1)(delete-char 1))
   --EMACSCMD:(progn (forward-line 3)(kill-line 2))
   --EMACSCMD:(progn (end-of-line 2)(execute-kbd-macro "\r-- Comment 1")(current-indentation))
   if (A and B
         -- Comment 1
      )
     -- Comment 2
     or C
   then
      null;
   end if;
   --EMACSRESULT:3
   --EMACSCMD:(progn (forward-line -8)(forward-word 1)(forward-char 1)(execute-kbd-macro "(")(end-of-line 2)(execute-kbd-macro "\r)")(indent-for-tab-command))

   -- Start comment on code line; transient error on next line should
   -- disappear when comment is finished. Note there is still one error
   -- from earlier in the file.
   --
   --EMACSCMD:(progn (forward-line 4)(execute-kbd-macro "-- ")(indent-for-tab-command)(length (wisi-parser-parse-errors wisi--parser)))
   --EMACSRESULT:2
   --EMACSCMD:(progn (forward-line 2)(search-forward "-- ")(execute-kbd-macro "comment\r")(indent-for-tab-command)(length (wisi-parser-parse-errors wisi--parser)))
   --EMACSRESULT:1
      for I in 1 .. 10 loop
      null;
   end loop;
   --EMACSCMD:(progn (forward-line -4)(kill-line 1)(indent-for-tab-command))
end Ada_Mode.Interactive_02;
-- Local Variables:
-- ada-end-name-optional: nil
-- compare-tree-text: nil
-- End:
