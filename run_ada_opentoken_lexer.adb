with Ada.Text_IO;
with Ada_Grammar;
with GNAT.OS_Lib;
with Ada.Command_Line;
with OpenToken.Text_Feeder.Counted_GNAT_OS_Lib;
with OpenToken;
procedure Run_Ada_OpenToken_Lexer
is
   --  command_line : <buffer_size> <file byte count>

   Analyzer : constant Ada_Grammar.Analyzers.Handle := Ada_Grammar.Analyzers.Initialize
     (Ada_Grammar.Create_Syntax,
      OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Create (GNAT.OS_Lib.Standin),
      Max_Buffer_Size => Integer'Value (Ada.Command_Line.Argument (1)));

   Feeder : OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Instance renames
     OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Instance (Analyzer.Feeder.all);

   Count : Integer := 0;
begin
   Feeder.Reset (Integer'Value (Ada.Command_Line.Argument (2)));

   loop
      exit when Analyzer.End_Of_Text;
      Analyzer.Find_Next;
      Count := Count + 1;
   end loop;

   Ada.Text_IO.Put_Line (Integer'Image (Count) & " tokens," & Integer'Image (Feeder.Get_Count) & " feeder gets");
end Run_Ada_OpenToken_Lexer;
