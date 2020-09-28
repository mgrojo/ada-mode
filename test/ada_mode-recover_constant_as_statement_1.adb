--EMACSCMD:(setq skip-recase-test t)
--EMACSDEBUG:(setq wisi-trace-mckenzie 1)
procedure Ada_Mode.Recover_Constant_As_Statement_1
is
begin
   Push_Back_Check (New_Config, +IDENTIFIER_ID);

   --  Copied next line, intending to make it an assignment.
Delete_Index : WisiToken.Token_Index := Config.Current_Shared_Token;

      Delete_Check (Terminals, New_Config, Delete_Index, +COLON_ID);

   end Ada_Mode.Recover_Constant_As_Statement_1;
