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
--EMACSRESULT:(point-min)
procedure Ada_Mode.Interactive_Wisi
is
   -- Start with a valid parse.
   --EMACSCMD:(wisi-parse-buffer)

   -- Inserting/deleting space (ie indenting) does not invalidate cache.
   --EMACSCMD:(progn (forward-line -1)(back-to-indentation)(insert "  ") wisi-cache-max)
   --EMACSRESULT:(point-max)
   --EMACSCMD:(progn (forward-line -3)(back-to-indentation)(delete-char -2) wisi-cache-max)
   --EMACSRESULT:(point-max)

   -- adding text inside a string does not invalidate cache
   A : constant String :=
     "hi there!";
   --EMACSCMD:(progn (end-of-line 0)(forward-word -1)(insert "some text ")wisi-cache-max)
   --EMACSRESULT:(point-max)

   -- Delete/insert range including close quote of string invalidates cache
   --EMACSCMD:(progn (end-of-line 5)(backward-char 1)(backward-delete-char 2)(jit-lock-fontify-now) wisi-cache-max)
   --EMACSRESULT:(line-beginning-position 4)
   --EMACSCMD:(progn (end-of-line 3)(backward-char 1)(execute-kbd-macro "o\"")(jit-lock-fontify-now) wisi-cache-max)
   --EMACSRESULT:(point-max)
   Obj_2 : constant String := "foo";

   -- Delete/insert range including open quote of string invalidates cache
   --EMACSCMD:(progn (end-of-line 5)(backward-char 4)(backward-delete-char 2)(jit-lock-fontify-now) wisi-cache-max)
   --EMACSRESULT:(line-beginning-position 4)
   --EMACSCMD:(progn (end-of-line 3)(backward-char 4)(execute-kbd-macro "\"f")(jit-lock-fontify-now) wisi-cache-max)
   --EMACSRESULT:(point-max)
   Obj_3 : constant String := "foo";

   -- adding text inside a comment does not invalidate cache
   --EMACSCMD:(progn (end-of-line 0)(forward-word -1)(insert "some text ")wisi-cache-max)
   --EMACSRESULT:(point-max)

   -- delete/insert range including comment start invalidates cache
   --EMACSCMD:(progn (beginning-of-line 0)(kill-word 1) wisi-cache-max)
   --EMACSRESULT:(line-beginning-position -7)
   --EMACSCMD:(progn (beginning-of-line -2)(execute-kbd-macro "   -- delete")(jit-lock-fontify-now) wisi-cache-max)
   --EMACSRESULT:(point-max)

   -- delete/insert range including comment end invalidates cache

   --EMACSCMD:(progn (end-of-line 0)(backward-delete-char 1) wisi-cache-max)
   --EMACSRESULT:(line-beginning-position -13)
   --EMACSCMD:(progn (beginning-of-line -1)(execute-kbd-macro "\n")(jit-lock-fontify-now) wisi-cache-max)
   --EMACSRESULT:(point-max)

   -- Newline before a blank line
   --EMACSCMD:(progn (end-of-line 2)(newline-and-indent)(current-column))


   --EMACSRESULT:3

   -- newline before code
   --EMACSCMD:(progn (forward-line 1)(back-to-indentation) (newline-and-indent)(current-column))

   function Local_Function_1 return Float;
   --EMACSRESULT:3

   -- new parameter in a subpgrogram
   --EMACSCMD:(progn (end-of-line 3)(delete-indentation)(delete-char -1)(insert ";")(ada-indent-newline-indent)(current-column))
   procedure Local_Proc_1 (Param_1 : in Float;
                           );
   --EMACSRESULT:26

   -- Adding a body interactively leaves it properly indented, and
   -- caches updated. Start with invalid syntax (missing final ';') -
   -- indent after syntax fixed should indent entire statement. Or GPS
   -- fallback should indent properly.

   --EMACSCMD:(progn (end-of-line 7)(delete-char -1)(newline-and-indent)(current-column))
   --EMACSRESULT:(if (eq wisi-indent-fallback 'wisi-indent-fallback-default) 5 0)
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
   --EMACSCMD:(progn (forward-line -14)(forward-word 1)(ada-goto-declaration-end)(looking-back "end"))
   --EMACSRESULT:t

   -- calling ada-make-subprogram-body tested in ada_mode-interactive_common.adb

   -- add an enumeration value in parens
   --EMACSCMD:(progn (end-of-line 4)(backward-char 2) (execute-kbd-macro ",\nWrite_Success")(indent-for-tab-command)(current-indentation))
   --EMACSRESULT:6
   type Wait_Return is
     (Read_Success);

begin
   --  extending block
   --EMACSCMD:(progn (forward-line 4)(kill-line 1)(forward-line 1)(yank) wisi-cache-max)
   --EMACSRESULT:(line-beginning-position 3)
   begin -- target extending
      Stuff_2;
   end; -- target extending
   Stuff_3;

   --EMACSCMD:(progn (forward-line -2)(forward-word 1)(wisi-goto-statement-start) (looking-at "begin -- target extending"))
   --EMACSRESULT:t
   --EMACSCMD:(progn (forward-line -4)(forward-word 1)(wisi-goto-statement-end) (looking-at "; -- target extending"))
   --EMACSRESULT:t

   -- merging one 'if' into another, as an 'elsif'. This encountered
   -- a bug in an earlier version.
   --
   -- Scenario:
   --
   -- - Paste the 'if'; parse is triggered by font-lock, succeeds
   --
   -- - Edit the 'if', leaving 'if then'; parse is triggered by
   --   font-lock, fails, leaving wisi-cache-max before 'if'
   --
   -- - Finish editing the 'elsif'; parse is triggered by font-lock,
   --   changing the IDENTIFIER cache to ELSIF.
   --
   -- - indent properly

   --EMACSCMD:(progn (forward-line 11)(kill-line 2)(forward-line -2)(yank)(forward-line -1)(kill-line)wisi-cache-max)
   --EMACSRESULT:(line-beginning-position 8)
   --EMACSCMD:(progn (forward-line 7)(wisi-validate-cache (line-end-position)))
   --EMACSCMD:(progn (forward-line 6)(back-to-indentation)(wisi-get-cache (point)))
   --EMACSRESULT:nil
   --EMACSCMD:(progn (forward-line 4)(back-to-indentation)(insert "el")(wisi-validate-cache (line-end-position))(insert "s")(wisi-validate-cache (line-end-position)))
   --EMACSCMD:(progn (forward-line 3)(back-to-indentation)(wisi-cache-token (wisi-get-cache (point))))
   --EMACSRESULT:'ELSIF
   if  then
   else
   end if;
   if  then
   end if;
end Ada_Mode.Interactive_Wisi;
