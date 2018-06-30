with Gen_LR_Parser_No_Recover_Run;
with Character_Literal_Actions;
with Character_Literal_Main;
with WisiToken.Syntax_Trees;
procedure Character_Literal_LR_Run is new Gen_LR_Parser_No_Recover_Run
  (Character_Literal_Actions.Descriptor, Character_Literal_Main.Create_Parser);
