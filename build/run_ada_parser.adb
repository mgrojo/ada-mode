with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada_Grammar;
with ada_grammar_dfa;
with GNAT.Traceback.Symbolic;
with WisiToken.Parser.LR.Parser;
with WisiToken.Text_Feeder.Text_IO;
with WisiToken.Token_Region;
procedure Run_Ada_Parser
is
   procedure Put_Usage
   is begin
      Put_Line
        ("run_ada_parser <verbosity> <enable_panic_mode> <enable_mckenzie> <mckenzie_enqueue_limit> <file_name>");
      Put_Line ("<enable_*>: 0 or 1");
   end Put_Usage;

   File_Name : Unbounded_String;

   Feeder : WisiToken.Text_Feeder.Text_Feeder_Ptr;
   Parser : WisiToken.Parser.LR.Parser.Instance;
begin
   if Argument_Count /= 5 then
      Put_Usage;
      Set_Exit_Status (Failure);
      return;
   end if;

   File_Name             := To_Unbounded_String (Argument (5));
   WisiToken.Trace_Parse := Integer'Value (Argument (1));

   Feeder := WisiToken.Text_Feeder.Text_IO.Create (To_String (File_Name));

   Parser := Ada_Grammar.Create_Parser
     (Algorithm   => WisiToken.LALR,
      Text_Feeder => Feeder);

   if WisiToken.Trace_Parse > 3 then
      ada_grammar_dfa.aflex_debug := True;
   end if;

   Parser.Enable_Panic_Recover    := Argument (2) /= "0";
   Parser.Enable_McKenzie_Recover := Argument (3) /= "0";

   if Argument (4) /= "-1" then
      Parser.Table.McKenzie.Enqueue_Limit := Integer'Value (Argument (4));
   end if;

   Parser.Parse;

   WisiToken.Text_Feeder.Text_IO.Instance (Feeder.all).Close;
exception
when WisiToken.Parse_Error | WisiToken.Syntax_Error =>
   WisiToken.Token_Region.Put (To_String (File_Name), Ada_Grammar.State.Errors, Ada_Grammar.Descriptor);

when E : others =>
   New_Line;
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Run_Ada_Parser;
