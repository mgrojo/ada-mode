with Gen_Parser_No_Recover_Run;
with Empty_Production_6_Actions;
with Empty_Production_6_Main;
with WisiToken.Syntax_Trees;
procedure Empty_Production_6_Run is new Gen_Parser_No_Recover_Run
  (WisiToken.Syntax_Trees.User_Data_Type, Empty_Production_6_Actions.Descriptor, Empty_Production_6_Main.Create_Parser);
