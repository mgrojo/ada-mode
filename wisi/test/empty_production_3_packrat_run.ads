with Gen_Packrat_Parser_Run;
with Empty_Production_3_Actions;
with Empty_Production_3_Packrat_Main;
with WisiToken.Syntax_Trees;
procedure Empty_Production_3_Packrat_Run is new Gen_Packrat_Parser_Run
  (Empty_Production_3_Actions.Descriptor, Empty_Production_3_Packrat_Main.Create_Parser);
