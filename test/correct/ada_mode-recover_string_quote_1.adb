package body Ada_Mode.Recover_String_Quote_1 is

   procedure Quote_Unquote (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Put_Line (Test_File, "8" & Tab & """nine");

      Put_Line ("ten" & Tab & """ eleven""");

      Close (Test_File);

   end Quote_Unquote;

end Ada_Mode.Recover_String_Quote_1;
