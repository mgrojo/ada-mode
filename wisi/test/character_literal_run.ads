with Gen_Parser_No_Recover_Run;
with Character_Literal_Actions;
with Character_Literal_Main;
with WisiToken.Syntax_Trees;
procedure Character_Literal_Run is new Gen_Parser_No_Recover_Run
  (WisiToken.Syntax_Trees.User_Data_Type, Character_Literal_Actions.Descriptor, Character_Literal_Main.Create_Parser);
