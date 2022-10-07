--EMACSCMD:(setq skip-recase-test t)

-- Tests the way gnatprep statements are highlighted and indented. We
-- also test other usage of the '#' character, especially in based
-- numbers and strings.

-- gnatprep-setup is added to ada-mode-hook when gnat-core.el is loaded,
-- which is normally before ada-mode-hook is executed. But in this test,
-- gnat-core.el is not loaded until the project file is parsed.
--EMACSCMD:(wisi-prj-select-cache "ada_mode.gpr" (ada-prj-default))
--EMACSCMD:(ada-mode)

--EMACSCMD:(when wisi-parser-shared (wisi-parse-buffer 'face)(font-lock-ensure))

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
      --EMACSCMD:(progn (end-of-line 2)(forward-char -1)(wisi-case-adjust)(buffer-substring (- (point) 6) (point)))
      Other_Flags : constant Interfaces.Unsigned_16 := 16#ffff#;
      --EMACSRESULT:"#ffff#"
   begin
      null;
   end;

end Gnatprep;
-- end of file
