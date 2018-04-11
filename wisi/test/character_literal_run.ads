with Gen_Parser_No_Recover_Run;
with Character_Literal;
with WisiToken.Syntax_Trees;
procedure Character_Literal_Run is new Gen_Parser_No_Recover_Run
  (WisiToken.Syntax_Trees.User_Data_Type, Character_Literal.Descriptor, Character_Literal.Create_Parser);
