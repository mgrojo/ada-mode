-- Example from real code.
--
-- First error, after "C_File_Name :", is corrected easily by one of:
-- ((insert ';') (insert IDENTIFIER))
-- ((push_back IDENTIFIER) (insert 'exception ;')
--
-- One solution is chosen at random; they lead do different indents.
-- So the indent can change with changes in grammar or error recovery.
--
-- Second error, after "Context, name" is fixed by (insert
-- ') THEN END IF ;').

--EMACSCMD:(setq skip-recase-test t)
procedure Ada_Mode.Recover_19 is
   use Interfaces.C.Strings;

   C_File_Name :
     B : Integer;
begin
   if 0 /= AV_Format_Open_Input (Context, Name
end Ada_Mode.Recover_19;
