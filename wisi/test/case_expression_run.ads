with Gen_Parser_Run_Counted_GNAT_OS_Lib;
with Case_Expression;
procedure Case_Expression_Run is new Gen_Parser_Run_Counted_GNAT_OS_Lib
  (Case_Expression.Create_Parser);
