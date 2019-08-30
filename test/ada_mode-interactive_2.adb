-- Test various things interactively; while the user is typing code.
-- See also ada_mode-interactive_*.adb

-- It doesn't compile; missing bodies, undefined vars.

-- It also doesn't pass the reindent and diff test, since we are deliberately adding code
--EMACSCMD:(setq skip-reindent-test t)

-- We don't disable the casing test; that is important during interactive editing.

-- Since we are editing, the syntax will be illegal at times; don't fail for that.
--EMACSCMD:(setq wisi-debug 0)

-- Test the buffer does parse
--EMACSCMD:(progn (wisi-parse-buffer 'face) (length (wisi-parser-parse-errors wisi--parser)))
--EMACSRESULT:0

procedure Ada_Mode.Interactive_2
is
   -- Newline before a blank line
   --EMACSCMD:(progn (end-of-line 2)(newline-and-indent)(current-column))

   --EMACSRESULT:3

   -- newline before code
   --EMACSCMD:(progn (forward-line 1)(back-to-indentation) (newline-and-indent)(current-column))
   function Local_Function_1 return Float;
   --EMACSRESULT:3

   -- New parameter in a subpgrogram. See comment in
   -- ada_mode-parens.adb Local_10 on hanging paren indent.

   --EMACSCMD:(progn (end-of-line 3)(delete-indentation)(delete-char -1)(insert ";")(wisi-indent-newline-indent) (current-column))
   procedure Local_Proc_1 (Param_1 : in Float;
                          );
   --EMACSRESULT:26

   -- Adding a body interactively leaves it properly indented, and caches
   -- updated. Start with invalid syntax (missing final ';'), automatic
   -- indent after syntax fixed should indent entire statement correctly.
   -- Note that this requires the function name after 'end', since
   -- ada-end-name-optional is nil by default.
   --
   -- Indentation of 'null;' before 'end;' is inserted is somewhat
   -- random, due to error correction. Error correction finds multiple
   -- solutions to the error; one inserts 'end;' to complete the
   -- subprogram_body as desired, but others delete 'begin' and complete
   -- a subprogram_body_stub, or delete 'is begin' and complete a
   -- subprogram_specification. Each of those solutions result in
   -- different indentations (6, 5, 3). They all eventuallly lead to
   -- identical parser stacks, where one is arbitrarily dropped. The
   -- results are not even repeatable in this test, since error recovery
   -- uses multiple tasks, so there is a race condition in the order the
   -- solutions are delivered.
   --
   -- After 'end;' is inserted, there is no error, so there is only one
   -- possible indentation for 'null;'.

   --EMACSCMD:(progn (end-of-line 7)(delete-char -2)(newline-and-indent))
   --EMACSCMD:(progn (forward-line 5)(execute-kbd-macro "is begin\nnull;\nend Function_Access_1;\n")(current-indentation))
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

   -- add an enumeration value in parens
   --EMACSCMD:(progn (end-of-line 4)(backward-char 2) (execute-kbd-macro ",\nWrite_Success")(indent-for-tab-command)(current-indentation))
   --EMACSRESULT:6
   type Wait_Return is
     (Read_Success);

begin
   --  extending block
   --EMACSCMD:(progn (forward-line 3)(kill-line 1)(forward-line 1)(yank))
   begin -- target extending
      Stuff_2;
   end; -- target extending
   Stuff_3;

end Ada_Mode.Interactive_2;
