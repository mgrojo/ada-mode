--  From a real editing session. extra 'begin'
--
-- Used to cause recover to fail; now finds a good solution quickly.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)
procedure Ada_Mode.Recover_14
is
   procedure Create_Ada_Test_Main
   is begin --  extra 'begin'
      use Wisi.Utils;

   begin
      Create (Body_File, Out_File, File_Name);
      Set_Output (Body_File);
      Indent := 1;
      Common_Data.Table_Actions_Count := 0;

   end Create_Ada_Test_Main;

begin
   case Common_Data.Lexer is
      when Re2c_Lexer =>
         null;

      when Elisp_Lexer =>
         raise User_Error with Wisitoken.Generate.Error_Message
           (Input_Data.Lexer.File_Name, 1, "Ada output language does not support " &
              Lexer_Names (Common_Data.Lexer).all & " lexer");
   end case;

exception
   when others =>
      Set_Output (Standard_Output);
      raise;
end Ada_Mode.Recover_14;
