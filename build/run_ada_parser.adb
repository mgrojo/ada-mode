with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada_Grammar;
with GNAT.Traceback.Symbolic;
with WisiToken.Parser.LR.Parser;
with WisiToken.Text_Feeder.Text_IO;
with WisiToken.Token_Region;
with ada_grammar_dfa;
procedure Run_Ada_Parser
is
   use all type Ada.Containers.Count_Type;

   procedure Put_Usage
   is begin
      Put_Line
        ("run_ada_parser <verbosity> <enable_mckenzie> <mckenzie_cost_limit> <file_name>");
      Put_Line ("<enable_*>: 0 or 1");
   end Put_Usage;

   File_Name : Unbounded_String;

   Feeder : WisiToken.Text_Feeder.Text_Feeder_Ptr;
   Parser : WisiToken.Parser.LR.Parser.Instance;
begin
   if Argument_Count /= 4 then
      Put_Usage;
      Set_Exit_Status (Failure);
      return;
   end if;

   File_Name             := To_Unbounded_String (Argument (4));
   WisiToken.Trace_Parse := Integer'Value (Argument (1));

   Feeder := WisiToken.Text_Feeder.Text_IO.Create (To_String (File_Name));

   Parser := Ada_Grammar.Create_Parser
     (Algorithm   => WisiToken.LALR,
      Text_Feeder => Feeder);

   if WisiToken.Trace_Parse > 3 then
      ada_grammar_dfa.aflex_debug := True;
   end if;

   Parser.Lexer.Enable_Line_Numbers := True;
   Parser.Enable_McKenzie_Recover   := Argument (2) /= "0";

   if Argument (3) /= "-1" then
      Parser.Table.McKenzie.Cost_Limit := Integer'Value (Argument (3));
   end if;

   Parser.Parse;

   WisiToken.Text_Feeder.Text_IO.Instance (Feeder.all).Close;
   if Ada_Grammar.State.Errors.Length > 0 then
      New_Line;
      Put_Line ("Errors:");
      WisiToken.Token_Region.Put (To_String (File_Name), Ada_Grammar.State.Errors, Ada_Grammar.Descriptor);
   end if;
exception
when E : WisiToken.Parse_Error | WisiToken.Syntax_Error =>
   New_Line;
   --  Exception message starts with ":<line>:<column>: "
   Put_Line (To_String (File_Name) & Ada.Exceptions.Exception_Message (E));

when E : others =>
   New_Line;
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Run_Ada_Parser;
