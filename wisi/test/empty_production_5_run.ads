with Gen_Parser_Run;
with Empty_Production_5;
procedure Empty_Production_5_Run is new Gen_Parser_Run (Empty_Production_5.Create_Parser, LR1 => True);
