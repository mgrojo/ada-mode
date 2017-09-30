with Gen_Parser_Run;
with Empty_Production_4;
procedure Empty_Production_4_Run is new Gen_Parser_Run (Empty_Production_4.Create_Parser, LR1 => True);
