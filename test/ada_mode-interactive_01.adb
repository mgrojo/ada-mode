-- Test various things interactively; while the user is typing code.
-- See also ada_mode-interactive_*.adb

-- It doesn't compile; missing bodies, undefined vars.

-- It does pass the reindent and diff test; all insertions are deleted.

procedure Ada_Mode.Interactive_01
is
   -- For 'indent-line', do insert spaces on an empty line
   --EMACSCMD:(progn (forward-line 2)(indent-for-tab-command)(current-column))
   --EMACSRESULT:3

   -- For 'indent-region', don't insert spaces on an empty line
   --EMACSCMD:(progn (forward-line 2)(wisi-indent-statement)(current-column))
   --EMACSRESULT:0

   -- newline at start of comment; indent-according-to-mode is called
   -- with the comment text exposed if font-lock is invoked during the
   -- function. No longer happens with Emacs 28 (and possibly earlier),
   -- even with jit-lock-defer-time nil. Keeping test in case it comes
   -- back.
   --
   --EMACSCMD:(progn (forward-line 2)(back-to-indentation)(forward-char 2) (comment-indent-new-line)(indent-for-tab-command))
   --
   --is; used to cause an unrecognized "is"
   --EMACSCMD:(progn (forward-line -2)(kill-line 1))

   -- testing auto-case

   --EMACSCMD:(progn (end-of-line 1)(newline-and-indent)(execute-kbd-macro "A")(char-before))
   --EMACSRESULT:?A
   --EMACSCMD:(progn (forward-line -2)(kill-line 1))

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

   -- testing ada-make-subprogram-body. see ada_mode-nominal-child.adb for 'overriding'

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
   --EMACSCMD:(progn (end-of-line 3)(kill-line 4)(insert ";")(ada-make-subprogram-body)(delete-char 4)(insert "return 0"))
   -- result verified by diff
   function Func_1 return Integer
   is begin
      return 0;
   end Func_1;

   -- function, parameters on separate line
   --EMACSCMD:(progn (end-of-line 5)(kill-line 4)(insert ";")(ada-make-subprogram-body)(delete-char 4)(insert "return 0"))
   -- result verified by diff
   function Func_2
     (A : in Integer)
     return Integer
   is begin
      return 0;
   end Func_2;

   -- Properly highlight keyword next to type identifier when insert/delete separating space
   --EMACSCMD:(progn (end-of-line 6)(backward-word 1)(backward-delete-char 1))
   --EMACSCMD:(test-face "accessString" nil) ;; can't extract type name from name
   --EMACSCMD:(progn (end-of-line 4)(backward-char 7)(execute-kbd-macro " "))
   --EMACSCMD:(test-face "access" 'font-lock-keyword-face)
   --EMACSCMD:(test-face "String" 'font-lock-type-face)
   Obj_1 : access String;

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
      E := 'z';
      --EMACSRESULT:t
   end;

   --EMACSCMD:(when wisi-parser-shared (end-of-line 2)(kill-line 2)(newline-and-indent)(insert "end loop;")(newline-and-indent)(wisi-goto-statement-start)(looking-at "for File_Name"))
   for File_Name in File_Names loop
   end loop;
   --EMACSRESULT:(not (null wisi-parser-shared))
   --EMACSCMD:(when wisi-parser-shared (beginning-of-line -2)(wisi-goto-statement-end)(looking-back "end loop"))
   --EMACSRESULT:(not (null wisi-parser-shared))

   -- Insert a comment after code; used to signal error.
   --EMACSCMD:(progn (end-of-line 2)(backward-delete-char 2)(comment-dwim nil)(looking-back "--$"))
   E := (1 =>                   --
           'A');
   --EMACSRESULT:t

   -- Re-indent a comment after code, to `comment-column'.
   --EMACSCMD:(progn (forward-line 1)(comment-dwim nil)(end-of-line)(current-column))
   E := (1 =>                   --
           'A');
   --EMACSRESULT:34

   -- `comment-dwim' should not change the indentation of the next comment.
   --EMACSCMD:(progn (forward-line 2)(comment-dwim nil)(back-to-indentation)(current-column))
   E := (1,
         --  a comment
         2,
         -- another comment
         3);
   --EMACSRESULT:9

end Ada_Mode.Interactive_01;
