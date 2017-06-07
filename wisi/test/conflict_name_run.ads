with Gen_Parser_Run_Counted_GNAT_OS_Lib;
with Conflict_Name;
procedure Conflict_Name_Run is new Gen_Parser_Run_Counted_GNAT_OS_Lib
  (Conflict_Name.Create_Parser);
