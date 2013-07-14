-- Test various things parser-independent things interactively; while the user is typing code.

-- It doesn't compile; missing bodies, undefined vars.

-- It does pass the reindent and diff test; all insertions are deleted.

procedure Ada_Mode.Interactive_Common
is
   -- newline at start of comment; indent-according-to-mode is called with the comment text exposed!
   --EMACSCMD:(progn (forward-line 2)(back-to-indentation)(forward-char 2) (comment-indent-new-line)(indent-for-tab-command)wisi-cache-max)
   --EMACSRESULT:(point-max)
   --
   --is; used to cause an unrecognized "is"
   --EMACSCMD:(progn (forward-line -2)(kill-line 1))

   -- testing auto-case

   --EMACSCMD:(progn (end-of-line 1)(newline-and-indent)(execute-kbd-macro "A")(char-before))
   --EMACSRESULT:?A
   -- clean up to keep syntax legal, allow diff compare
   --EMACSCMD:(progn (forward-line -3)(kill-line 1))

   --EMACSCMD:(progn (end-of-line 1)(newline-and-indent)(execute-kbd-macro "a")(char-before))
   --EMACSRESULT:?a
   --EMACSCMD:(progn (forward-line -2)(kill-line 1))

   --EMACSCMD:(progn (end-of-line 1)(newline-and-indent)(execute-kbd-macro "a_b ")(let ((case-fold-search nil))(looking-back "A_B ")))
   --EMACSRESULT:t
   --EMACSCMD:(progn (forward-line -2)(kill-line 1))

   --EMACSCMD:(progn (end-of-line 1)(newline-and-indent)(execute-kbd-macro "foo_bar_baz ")(let ((case-fold-search nil))(looking-back "Foo_Bar_Baz ")))
   --EMACSRESULT:t
   --EMACSCMD:(progn (forward-line -2)(kill-line 1))

   --EMACSCMD:(progn (forward-line 1)(downcase-word 1)(execute-kbd-macro "\C-u\C-c\C-w")(let ((case-fold-search nil))(looking-back "Ada_Identifier")))
   -- Ada_Identifier in comment; force auto-case
   --EMACSRESULT:t

   -- testing ada-make-subprogram-body

   -- procedure, no parameters
   --EMACSCMD:(progn (end-of-line 3)(kill-line 4)(insert ";")(ada-make-subprogram-body))
   -- result verified by diff.
   procedure Proc_1
   is begin
      null;
   end Proc_1;

   -- procedure, parameters on one line
   --EMACSCMD:(progn (end-of-line 3)(kill-line 4)(insert ";")(ada-make-subprogram-body))
   -- result verified by diff
   procedure Proc_2 (A : in Integer)
   is begin
      null;
   end Proc_2;

   -- function, no parameters
   --EMACSCMD:(progn (end-of-line 3)(kill-line 4)(insert ";")(ada-make-subprogram-body)(kill-word 1)(insert "return 0"))
   -- result verified by diff
   function Func_1 return Integer
   is begin
      return 0;
   end Func_1;

   -- function, parameters on separate line
   --EMACSCMD:(progn (end-of-line 5)(kill-line 4)(insert ";")(ada-make-subprogram-body)(kill-word 1)(insert "return 0"))
   -- result verified by diff
   function Func_1
     (A : in Integer)
     return Integer
   is begin
      return 0;
   end Func_1;

   -- don't capitalize keywords after typing ' '
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(execute-kbd-macro " ")(let ((case-fold-search nil))(looking-back "begin ")))
begin
   --EMACSRESULT:t
   --EMACSCMD:(progn (end-of-line -1)(kill-backward-chars 1))

   -- don't capitalize word after inserting punctuation before it
   --EMACSCMD:(progn (forward-line 1)(back-to-indentation)(execute-kbd-macro "((")(let ((case-fold-search nil))(looking-at "begin")))
   begin
      --EMACSRESULT:t
      --EMACSCMD:(progn (forward-line -2)(back-to-indentation)(delete-char 2))

      -- don't capitalize character constant
      --EMACSCMD:(progn (end-of-line 2)(backward-char 1)(delete-char -1)(execute-kbd-macro "'")(let ((case-fold-search nil))(looking-back "'z'")))
      E := 'Z';
      --EMACSCMD:(progn (end-of-line 0)(backward-char 1))
   end;
end Ada_Mode.Interactive_Common;
