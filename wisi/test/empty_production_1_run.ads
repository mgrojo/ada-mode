with Gen_Parser_Run;
with Empty_Production_1;
procedure Empty_Production_1_Run is new Gen_Parser_Run
  (Empty_Production_1.Create_Parser, LR1 => True);
