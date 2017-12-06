--  A real error recovery case. Current implementation is fast enough,
--  but is confusing. Correct solution requires matching start and end
--  procedure names.
--
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
procedure Journal_To_TSV
is

   procedure Process_CSV_File
   is

      procedure To_Month
      is begin

         --  end To_Month;
         -- have not typed this yet, so the following code appears to be:
         begin
         end Process_CSV_File;

         begin

         end Journal_To_TSV;

         --  Errors here; need to insert (cost 8):
         --        end to_month;
         --     begin
         --     end process_csv_file;
         --  begin
         --  end journal_to_tsv;
         -- Local variables:
         -- wisi-mckenzie-cost-limit: 8
         -- End:
