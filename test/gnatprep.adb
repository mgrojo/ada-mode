--EMACSCMD:(setq skip-recase-test t)

-- Tests the way gnatprep statements are highlighted. We also test
-- other usage of the '#' sign, especially in based numbers or in
-- strings.

-- gnatprep font-lock is enabled in ada-gnat-post-local-vars, which is
-- added to hack-local-variables-hook in ada-gnat-setup, which is
-- added to ada-mode-hook by ada-select-prj-file. But that is too late
-- here, so we call ada-gnat-post-local-vars directly.
--
-- Similarly, gnatprep syntax-propertize is set by
-- ada-gnat-select-prj-xref.
--
-- FIXME: need simpler mechanism for user to enable gnatprep per-file.
--
--EMACSCMD:(ada-gnat-select-prj-xref)
--EMACSCMD:(progn (syntax-ppss-flush-cache (point-min)) (syntax-propertize (point-max)))
--EMACSCMD:(ada-gnat-post-local-vars)

-- font-lock doesn't turn on when non-interactive
--EMACSCMD:(font-lock-fontify-buffer)

procedure Gnatprep is

begin
   if A = 1 then
      -- Can't use 'test-face' here because it skips comments, and gnatprep lines have comment syntax.
      --EMACSCMD:(progn (forward-line 1)(face-at-point))
#if Gnat_Compiler
      --EMACSRESULT:font-lock-type-face
      A := 1;
      --EMACSCMD:(progn (forward-line 1)(syntax-class (syntax-after (point))))
#elsif Other_Compiler
      --EMACSRESULT:11
      A := 2;
#else
      B := 3;
      --EMACSCMD:(progn (forward-line 1)(face-at-point))
#end if;
      --EMACSRESULT:font-lock-type-face
      A := 3;

   end if;

   --EMACSCMD:(progn (forward-line 1)(forward-char 14)(face-at-point))
   A := "in a # string";
   --EMACSRESULT:font-lock-string-face

   declare
      Control_Flags : Integer;
      --EMACSCMD:(progn (end-of-line 5)(forward-char -4)(face-at-point))
      --EMACSRESULT:font-lock-constant-face
      --EMACSCMD:(progn (end-of-line 3)(forward-char -3)(syntax-class (syntax-after (point))))
      --EMACSRESULT:2
      for Control_Flags use at (BASE_ADDRESS + 16#3fd#);

      --  Don't change the hex literal to mixed case
      --EMACSCMD:(progn (end-of-line 2)(forward-char -1)(ada-case-adjust)(buffer-substring (- (point) 6) (point)))
      Other_Flags : constant Interfaces.Unsigned_16 := 16#ffff#;
      --EMACSRESULT:"#ffff#"
   begin
      null;
   end;

end Gnatprep;

-- Local Variables:
-- ada-indent-comment-col-0: t
-- End:
-- end of file
