with Gen_Packrat_Parser_Run;
with Conflict_Name_Actions;
with Conflict_Name_Packrat_Main;
with WisiToken.Syntax_Trees;
procedure Conflict_Name_Packrat_Run is new Gen_Packrat_Parser_Run
  (Conflict_Name_Actions.Descriptor, Conflict_Name_Packrat_Main.Create_Parser);
