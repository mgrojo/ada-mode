-- Test various things interactively; while the user is typing code.

-- It doesn't compile; missing bodies, undefined vars.

-- It also doesn't pass the reindent and diff test, since we are deliberately adding code
--EMACSCMD:(setq skip-reindent-test t)

-- We don't disable the casing test; that is important during interactive editing.

-- Since we are editing, the syntax will be illegal at times; don't fail for that.
--EMACSCMD:(setq wisi-debug 0)

-- Test the buffer does parse
--EMACSCMD:(progn (wisi-parse-buffer) wisi-cache-max)
--EMACSRESULT:(point-max)

-- Test cache invalidation when inserting code programmatically
--EMACSCMD:(progn(forward-line 2)(insert "with A;\n") wisi-cache-max)
--EMACSRESULT:(line-beginning-position 2)
procedure Ada_Mode.Interactive_Wisi
is
   -- adding text inside a string does not invalidate cache
   --EMACSCMD:(wisi-parse-buffer)
   A : constant String :=
     "hi there!";
   --EMACSCMD:(progn (end-of-line 0)(forward-word -1)(insert "some text ")wisi-cache-max)
   --EMACSRESULT:(point-max)

   -- adding text inside a comment does not invalidate cache
   --EMACSCMD:(progn (end-of-line 0)(forward-word -1)(insert "some text ")wisi-cache-max)
   --EMACSRESULT:(point-max)

   -- Newline before a blank line
   --EMACSCMD:(progn (end-of-line 2)(newline-and-indent)(current-column))


   --EMACSRESULT:3

   -- newline before code
   --EMACSCMD:(progn (forward-line 1)(back-to-indentation) (newline-and-indent)(current-column))

   function Local_Function_1 return Float;
   --EMACSRESULT:3

   -- adding a body interactively leaves it properly indented, and caches updated.
   -- Start with invalid syntax (missing final ';')
   --EMACSCMD:(progn (end-of-line 7)(delete-char -1)(newline-and-indent)(current-column))
   --EMACSRESULT:5
   --EMACSCMD:(progn (forward-line 5)(back-to-indentation)(execute-kbd-macro "is begin\nnull;\nend;")(indent-for-tab-command)(current-indentation))
   --EMACSRESULT:3
   function Function_Access_1
     (A_Param : in Float)
     return Standard.Float;
   --EMACSCMD:(progn (forward-line -3)(current-indentation))
   --EMACSRESULT:3
   --EMACSCMD:(progn (forward-line -4)(current-indentation))
   --EMACSRESULT:6
   --EMACSCMD:(progn (forward-line -5)(current-indentation))
   --EMACSRESULT:3
   --EMACSCMD:(progn (forward-line -7)(ada-goto-declaration-start)(looking-at "function Function_Access_1"))
   --EMACSRESULT:t
   --EMACSCMD:(progn (forward-line -14)(forward-word 1)(ada-goto-declaration-end)(looking-back "Standard.Floa"))
   --EMACSRESULT:t
   -- The last test should look for "end;", but wisi-before-change
   -- does not invalidate the start of the declaration, so the end
   -- marker in that cache is not updated.

   -- calling ada-make-subprogram-body tested in ada_mode-interactive_common.adb

   -- add an enumeration value in parens
   --EMACSCMD:(progn (end-of-line 4)(backward-char 2) (execute-kbd-macro ",\nWrite_Success")(indent-for-tab-command)(current-indentation))
   --EMACSRESULT:6
   type Wait_Return is
     (Read_Success);

begin
   --  extending block
   --EMACSCMD:(progn (forward-line 4)(kill-line 1)(forward-line 1)(yank) wisi-cache-max)
   --EMACSRESULT:(line-beginning-position 4)
   begin -- target extending
      Stuff_2;
   end; -- target extending
   Stuff_3;

   --EMACSCMD:(progn (forward-line -2)(forward-word 1)(wisi-goto-statement-start) (looking-at "begin -- target extending"))
   --EMACSRESULT:t
   --EMACSCMD:(progn (forward-line -4)(forward-word 1)(wisi-goto-statement-end) (looking-at "; -- target extending"))
   --EMACSRESULT:t
end Ada_Mode.Interactive_Wisi;
