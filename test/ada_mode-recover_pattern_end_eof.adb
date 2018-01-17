--  Test recover_pattern_end_eof

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)

package body Ada_Mode.Recover_Pattern_End_EOF is

   procedure Quote_Unquote
   is
   begin
      Put_Line (Test_File, "8" & Tab & """nine");
      Close (Test_File);
   end;

   Open : Virtual_Type_Identifier
     (Csv_File,
      Csv_File_Name,
      Max_Row_Size       => 15,
      Delimiter          => Tab,
      Combine_Delimiters => False);

end Quote_Unquote;

end Ada_Mode.Recover_Pattern_End_EOF;
