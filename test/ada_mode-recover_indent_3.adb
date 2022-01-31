--  A real error recovery case.
--
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
procedure Journal_To_TSV
is
   procedure Process_CSV_File
   is
      procedure To_Month
      is begin

         -- missing 'end To_Month;'
         begin
   end Process_CSV_File;
begin
   A := 1;
end Journal_To_TSV;
-- Local Variables:
-- ada-end-name-optional: nil
-- End:
