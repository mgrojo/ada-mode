
--  Tests the way gnatprep statements are highlighted.
--  We also test other usage of the '#' sign, especially in
--  based numbers or in strings.

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
      --EMACSCMD:(progn (end-of-line 2)(forward-char -4)(face-at-point))
      for Control_Flags use at (BASE_ADDRESS + 16#3fd#);
      --EMACSRESULT:font-lock-constant-face

      --  Don't change the hex literal to mixed case
      --EMACSCMD:(progn (end-of-line 2)(forward-char -1)(buffer-substring (- (point) 6) (point)))
      Other_Flags : constant Interfaces.Unsigned_16 := 16#ffff#;
      --EMACSRESULT:"#ffff#"
   begin
   end;

end Gnatprep;

-- end of file
