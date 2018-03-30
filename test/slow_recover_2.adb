-- Example from real code.
--
-- First error, after "C_File_Name :", is corrected easily by 'insert IDENTIFIER ;'.
--
-- Second error, after "Context, name" should be fixed by 'insert )
-- THEN END IF ;' cost 11, but instead it finds 'insert ) THEN',
--'delete END' cost 5.
--
-- That causes third error at EOF, which leaves three parsers active,
-- of which one is picked, to allow indent.
--
-- IMPROVEME: don't pop END if name matches.

--EMACSCMD:(setq skip-recase-test t)
procedure SMM.Metadata.Debug is
   use Interfaces.C.Strings;

   C_File_Name :
begin
   if 0 /= AV_Format_Open_Input (Context, Name

end SMM.Metadata.Debug;
