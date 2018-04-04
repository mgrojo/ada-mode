with Gen_Parser_Run;
with Empty_Production_2;
with WisiToken.Syntax_Trees;
procedure Empty_Production_2_Run is new Gen_Parser_Run
  (WisiToken.Syntax_Trees.User_Data_Type, Empty_Production_2.Descriptor, Empty_Production_2.Create_Parser,
   LR1 => False);
