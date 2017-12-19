-- Test that grammar actions tolerate virtual tokens.
--
-- Does not compile.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)

-- While editing, we mistakenly pasted 'if then end if;' in the
-- exception handler area. This causes the parser to insert several
-- virtual tokens, both there and before the later 'begin end'. Those
-- tokens are then referenced in 'wisi-containing-action' and other
-- grammar actions.

-- The test is that parsing signals no errors, and the syntax errors
-- are reported properly.

--EMACSCMD:(wisi-validate-cache (point-max) nil 'navigate)
--EMACSCMD:(length (wisi-parser-errors wisi--parser))
--EMACSRESULT:1

--EMACSCMD:(wisi-validate-cache (point-max) nil 'face)

--EMACSCMD:(wisi-indent-region (point-min) (point-max))
procedure Journal_To_TSV
is
   procedure Process_Text_File
   is begin
   exception
      if then end if;
   end Process_Text_File;
begin
   begin
   end;
end Journal_To_TSV;
