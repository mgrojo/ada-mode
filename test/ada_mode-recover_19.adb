-- Example from real code.
--
-- First error, after "C_File_Name :", is corrected easily by (insert 'exception ;').
--
-- Second error, after "Context, name" is fixed by (insert
-- ') THEN END IF ;').

--EMACS_SKIP_UNLESS: (eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
procedure Ada_Mode.Recover_19 is
   use Interfaces.C.Strings;

   C_File_Name :
     B : Integer;
begin
   if 0 /= AV_Format_Open_Input (Context, Name
end Ada_Mode.Recover_19;
