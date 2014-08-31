with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;
with OpenToken.Text_Feeder.Text_IO;
with Empty_Production_5; use Empty_Production_5;
procedure Empty_Production_5_Run
is
   procedure Put_Usage
   is begin
      Put_Line ("usage: empty_production_5_run [-v] filename");
      Put_Line ("  outputs grammar actions");
      Put_Line ("  -v : output trace of states while parsing");
   end Put_Usage;

   File   : File_Type;
   Parser : LALR_Parsers.Instance := Create_Parser;

   procedure Use_File (File_Name : in String)
   is begin
      Open (File, In_File, File_Name);
   exception
   when Name_Error =>
      Put_Line (File_Name & " cannot be opened");
      raise OpenToken.User_Error;
   end Use_File;
begin
   declare
      use Ada.Command_Line;
   begin
      case Argument_Count is
      when 1 =>
         Use_File (Argument (1));

      when 2 =>
         if Argument (1) = "-v" then
            OpenToken.Trace_Parse := True;

         else
            Set_Exit_Status (Failure);
            Put_Usage;
            return;
         end if;

         Use_File (Argument (2));

      when others =>
         Set_Exit_Status (Failure);
         Put_Usage;
         return;
      end case;
   exception
   when others =>
      Set_Exit_Status (Failure);
      Put_Usage;
      return;
   end;

   Set_Input (File);
   LALR_Parsers.Set_Text_Feeder (Parser, OpenToken.Text_Feeder.Text_IO.Create (Current_Input));
   LALR_Parsers.Parse (Parser);
exception
when E : others =>
   New_Line;
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
end Empty_Production_5_Run;
