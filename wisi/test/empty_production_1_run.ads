with Gen_Parser_No_Recover_Run;
with Empty_Production_1;
with WisiToken.Syntax_Trees;
procedure Empty_Production_1_Run is new Gen_Parser_No_Recover_Run
  (WisiToken.Syntax_Trees.User_Data_Type, Empty_Production_1.Descriptor, Empty_Production_1.Create_Parser);
