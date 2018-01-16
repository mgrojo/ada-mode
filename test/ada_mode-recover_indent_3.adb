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
         -- have not typed this yet. Error recovery deletes the following block
         -- (in order to insert the missing 'end;' in the right place) and
         -- replaces it with virtual tokens, so it is not indented correctly.
         -- Following code is indented correctly.
begin
end Process_CSV_File;

begin
   A := 1;
end Journal_To_TSV;
