with Gen_Parser_Run;
with Empty_Production_8;
with WisiToken.Syntax_Trees;
procedure Empty_Production_8_Run is new Gen_Parser_Run
  (WisiToken.Syntax_Trees.User_Data_Type, Empty_Production_8.Descriptor, Empty_Production_8.Create_Parser,
   LR1 => False);
