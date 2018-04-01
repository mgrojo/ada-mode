with Gen_Parser_Run;
with Character_Literal;
procedure Character_Literal_Run is new Gen_Parser_Run
  (Character_Literal.Descriptor, Character_Literal.Create_Parser, LR1 => False);
