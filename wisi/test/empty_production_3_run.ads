with Gen_Parser_No_Recover_Run;
with Empty_Production_3;
with WisiToken.Syntax_Trees;
procedure Empty_Production_3_Run is new Gen_Parser_No_Recover_Run
  (WisiToken.Syntax_Trees.User_Data_Type, Empty_Production_3.Descriptor, Empty_Production_3.Create_Parser);
