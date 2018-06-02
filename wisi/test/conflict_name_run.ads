with Gen_Parser_No_Recover_Run;
with Conflict_Name_Actions;
with Conflict_Name_Main;
with WisiToken.Syntax_Trees;
procedure Conflict_Name_Run is new Gen_Parser_No_Recover_Run
  (WisiToken.Syntax_Trees.User_Data_Type, Conflict_Name_Actions.Descriptor, Conflict_Name_Main.Create_Parser);
