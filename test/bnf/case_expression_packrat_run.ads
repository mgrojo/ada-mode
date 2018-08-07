with Gen_Packrat_Parser_Run;
with Case_Expression_Actions;
with Case_Expression_Packrat_Main;
with WisiToken.Syntax_Trees;
procedure Case_Expression_Packrat_Run is new Gen_Packrat_Parser_Run
  (Case_Expression_Actions.Descriptor, Case_Expression_Packrat_Main.Create_Parser);
