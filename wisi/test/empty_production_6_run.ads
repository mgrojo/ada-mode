with Gen_Parser_Run;
with Empty_Production_6;
procedure Empty_Production_6_Run is new Gen_Parser_Run (Empty_Production_6.Create_Parser, LR1 => False);
