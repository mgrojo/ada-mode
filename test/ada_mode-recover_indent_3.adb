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

         -- end To_Month;
         --
         -- have not typed this yet. Error recovery pushes back the following
         -- block, inserts the missing 'end;' in the right place.
   begin
   end Process_CSV_File;
begin
   A := 1;
end Journal_To_TSV;
