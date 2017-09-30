with Ada_Lite;
with Gen_Run_Wisi_Parser;
procedure Run_Ada_Lite_Parser is new Gen_Run_Wisi_Parser
  (Name          => "Ada_Lite",
   Create_Parser => Ada_Lite.Create_Parser,
   Errors        => Ada_Lite.State.Errors,
   Descriptor    => Ada_Lite.Descriptor);
