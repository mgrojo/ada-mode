--EMACSCMD:(setq skip-recase-test t)

-- Tests the way gnatprep statements are highlighted and indented. We
-- also test other usage of the '#' sign, especially in based numbers
-- or in strings.

-- gnatprep indentation is enabled by `ada-*-setup', which is added to
-- ada-mode-hook when a project file is selected. When this file is
-- run as a test, no project file has been selected before the file is
-- opened, so gnatprep indentation is not enabled. So we run
-- gnatprep-setup explicitly.

--EMACSCMD:(ada-parse-prj-file "ada_mode.gpr")
--EMACSCMD:(ada-select-prj-file "ada_mode.gpr")
--EMACSCMD:(gnatprep-setup)

--EMACSCMD:(jit-lock-fontify-now)

procedure Gnatprep is

begin
   if A = 1 then
      -- Can't use 'test-face' here because it skips comments, and gnatprep lines have comment syntax.
      --EMACSCMD:(progn (forward-line 1)(face-at-point))
#if Gnat_Compiler
--EMACSRESULT:font-lock-preprocessor-face
      A := 1;
      --EMACSCMD:(progn (forward-line 1)(syntax-class (syntax-after (point))))
#elsif Other_Compiler
--EMACSRESULT:11
      A := 2;
#else
      B := 3;
      --EMACSCMD:(progn (forward-line 1)(face-at-point))
#end if;
--EMACSRESULT:font-lock-preprocessor-face
      A := 3;

   end if;

   --EMACSCMD:(test-face "#" font-lock-string-face))
   A := "in a # string";

   declare
      Control_Flags : Integer;
      --EMACSCMD:(test-face "16#3fd#" font-lock-constant-face)
      --EMACSCMD:(progn (end-of-line 3)(forward-char -3)(syntax-class (syntax-after (point))))
      --EMACSRESULT:1
      for Control_Flags use at (BASE_ADDRESS + 16#3fd#);

      --  Don't change the hex literal to mixed case
      --EMACSCMD:(progn (end-of-line 2)(forward-char -1)(ada-case-adjust)(buffer-substring (- (point) 6) (point)))
      Other_Flags : constant Interfaces.Unsigned_16 := 16#ffff#;
      --EMACSRESULT:"#ffff#"
   begin
      null;
   end;

end Gnatprep;

-- end of file
