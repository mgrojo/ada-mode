with Ada_Lite;
with Gen_Run_Wisi_Parser;
with ada_lite_dfa;
with ada_lite_io;
procedure Run_Ada_Lite_Parser is new Gen_Run_Wisi_Parser
  (Name          => "Ada_Lite",
   Create_Parser => Ada_Lite.Create_Parser,
   aflex_debug   => ada_lite_dfa.aflex_debug,
   Aflex_Feeder  => ada_lite_io.Feeder,
   Errors        => Ada_Lite.State.Errors,
   Descriptor    => Ada_Lite.Descriptor);
