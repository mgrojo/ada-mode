--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2021 Free Software Foundation, Inc.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Real_Time;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with GNATCOLL.Mmap;
with SAL;
with System.Multiprocessors;
with WisiToken.Parse.LR.McKenzie_Recover;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
package body Run_Wisi_Common_Parse is

   procedure Usage_1 (Parse_Data : Wisi.Parse_Data_Type'Class)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("usage: parse_partial <post_parse_action> <file_name> [partial parse params] [options]");
      Put_Line ("   or: parse_incremental <post_parse_action> <file_name> <changes> ");
      Put_Line ("          <action_begin_byte> <action_end_byte> <action_begin_indent> [options]");
      Put_Line ("   or: refactor <refactor_action> <file_name> <edit_begin> [options]");
      Put_Line ("parse_action: {Navigate | Face | Indent}");
      Put_Line ("refactor_action:");
      Parse_Data.Refactor_Help;
   end Usage_1;

   procedure Usage
     (Parser : in out WisiToken.Parse.LR.Parser.Parser)
   is
      use all type WisiToken.Parse.LR.Parse_Table_Ptr;
      use Ada.Text_IO;
   begin
      Usage_1 (Wisi.Parse_Data_Type'Class (Parser.User_Data.all));
      Put_Line ("partial parse params: begin_byte_pos end_byte_pos goal_byte_pos begin_char_pos begin_line" &
                  " end_line begin_indent");
      Put_Line ("options:");
      Put_Line ("--verbosity <trace config>");
      Put_Line ("   0 - only report parse errors");
      Put_Line ("   1 - shows spawn/terminate parallel parsers, error recovery enter/exit");
      Put_Line ("   2 - add each parser cycle, error recovery enqueue/check");
      Put_Line ("   3 - parse stack in each cycle, error recovery parse actions");
      Put_Line ("   4 - add lexer debug, dump syntax tree");
      Put_Line ("--save_text <file_name> : write edited file text to file_name");
      Put_Line ("--zombie_limit n  : set error recover token zombie limit" &
                  (if Parser.Table = null then ""
                   else "; default" & Parser.Table.McKenzie_Param.Zombie_Limit'Image));
      Put_Line ("--check_limit n  : set error recover token check limit" &
                  (if Parser.Table = null then ""
                   else "; default" & Parser.Table.McKenzie_Param.Check_Limit'Image));
      Put_Line ("--check_delta n  : set error recover delta check limit" &
                  (if Parser.Table = null then ""
                   else "; default" & Parser.Table.McKenzie_Param.Check_Delta_Limit'Image));
      Put_Line ("--enqueue_limit n  : set error recover token enqueue limit" &
                  (if Parser.Table = null then ""
                   else "; default" & Parser.Table.McKenzie_Param.Enqueue_Limit'Image));
      Put_Line ("--max_parallel n  : set maximum count of parallel parsers" &
                  (if Parser.Table = null then ""
                   else "; default" & Parser.Table.Max_Parallel'Image));
      Put_Line ("--task_count n : worker tasks in error recovery");
      Put_Line ("--lang_params <language-specific params>");
      Put_Line ("--repeat_count n : repeat parse count times, for profiling; default 1");
      New_Line;
   end Usage;

   function Command_File_Name
     (Parse_Data : in     Wisi.Parse_Data_Type'Class;
      Next_Arg   :    out Integer)
     return Command_Line_Params
   is
      use Ada.Command_Line;
      use WisiToken;
      Command : Command_Type;
   begin
      if Argument_Count < 3 then
         Usage_1 (Parse_Data);
         Set_Exit_Status (Failure);
         raise Finish;
      end if;

      Command := Command_Type'Value (Ada.Command_Line.Argument (1));

      return Result : Command_Line_Params (Command) do
         case Command is
         when Parse_Partial =>
            Result.Partial_Post_Parse_Action := Wisi.Post_Parse_Action_Type'Value (Ada.Command_Line.Argument (2));
            Result.Source_File_Name  := +Ada.Command_Line.Argument (3);
            Next_Arg := 4;

         when Parse_Incremental =>
            Result.Inc_Post_Parse_Action := Wisi.Post_Parse_Action_Type'Value (Ada.Command_Line.Argument (2));
            Result.Source_File_Name      := +Ada.Command_Line.Argument (3);
            Next_Arg                     := 4;

         when Refactor =>
            Result.Refactor_Action  := Integer'Value (Argument (2));
            Result.Source_File_Name := +Ada.Command_Line.Argument (3);
            Next_Arg := 4;
         end case;
      end return;
   exception
   when Finish =>
      raise;

   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
      Usage_1 (Parse_Data);
      Set_Exit_Status (Failure);
      raise SAL.Parameter_Error;
   end Command_File_Name;

   Save_File_Name : Ada.Strings.Unbounded.Unbounded_String;

   procedure Remaining_Command_Params
     (Parser : in out WisiToken.Parse.LR.Parser.Parser;
      Params : in out Command_Line_Params;
      Arg    : in out Integer)
   --  Command_File_Name reads the first few command line arguments
   is
      use Ada.Command_Line;
      use WisiToken;
   begin
      if Argument (Arg) = "--help" then
         Usage (Parser);
         raise Finish;
      end if;

      case Params.Command is
      when Parse_Partial =>
         if Argument_Count >= 4 and then Argument (4)(1) /= '-' then
            Params.Partial_Begin_Byte_Pos := WisiToken.Buffer_Pos'Value (Argument (4));
            Params.Partial_End_Byte_Pos   := WisiToken.Buffer_Pos'Value (Argument (5)) - 1; -- match emacs region
            Params.Partial_Goal_Byte_Pos  := WisiToken.Buffer_Pos'Value (Argument (6));
            Params.Partial_Begin_Char_Pos := WisiToken.Buffer_Pos'Value (Argument (7));
            Params.Partial_Begin_Line     := WisiToken.Line_Number_Type'Value (Argument (8));
            Params.End_Line               := WisiToken.Line_Number_Type'Value (Argument (9));
            Params.Partial_Begin_Indent   := Integer'Value (Argument (10));
            Arg                           := 11;
         else
            Params.Partial_Begin_Byte_Pos := WisiToken.Invalid_Buffer_Pos;
            Params.Partial_End_Byte_Pos   := WisiToken.Invalid_Buffer_Pos;
            Params.Partial_Begin_Char_Pos := WisiToken.Buffer_Pos'First;
            Params.Partial_Begin_Line     := WisiToken.Line_Number_Type'First;
         end if;

      when Parse_Incremental =>
         declare
            Text : constant String := Argument (4);
            Last : Integer := Text'First - 1;
         begin
            Params.Changes := Wisi.Get_Emacs_Change_List (Text, Last, Handle_String_Escapes => True);
         end;

         Params.Inc_Begin_Byte_Pos := WisiToken.Buffer_Pos'Value (Argument (5));
         Params.Inc_End_Byte_Pos   := WisiToken.Buffer_Pos'Value (Argument (6)) - 1; -- match emacs region
         Params.Inc_Begin_Indent   := Integer'Value (Argument (7));
         Arg                       := 8;

      when Refactor =>
         Params.Edit_Begin := WisiToken.Buffer_Pos'Value (Argument (4));
         Arg               := 5;
      end case;

   exception
   when Finish | SAL.Parameter_Error =>
      raise;

   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
      Usage (Parser);
      Set_Exit_Status (Failure);
      raise SAL.Parameter_Error;
   end Remaining_Command_Params;

   procedure Command_Options
     (Parser : in out WisiToken.Parse.LR.Parser.Parser;
      Params : in out Command_Line_Params;
      Arg    : in out Integer)
   --  Command_File_Name reads the first few command line arguments
   is
      use Ada.Command_Line;
      use WisiToken;
   begin
      loop
         exit when Arg > Argument_Count;

         if Argument (Arg) = "--verbosity" then
            WisiToken.Enable_Trace (Argument (Arg + 1));
            Arg := @ + 2;

         elsif Argument (Arg) = "--save_text" then
            Save_File_Name := +Argument (Arg + 1);
            Arg := @ + 2;

         elsif Argument (Arg) = "--zombie_limit" then
            Parser.Table.McKenzie_Param.Zombie_Limit := WisiToken.Syntax_Trees.Node_Index'Value
              (Argument (Arg + 1));
            Arg := @ + 2;

         elsif Argument (Arg) = "--check_limit" then
            Parser.Table.McKenzie_Param.Check_Limit := WisiToken.Syntax_Trees.Node_Index'Value
              (Argument (Arg + 1));
            Arg := @ + 2;

         elsif Argument (Arg) = "--check_delta" then
            Parser.Table.McKenzie_Param.Check_Delta_Limit := Integer'Value (Argument (Arg + 1));
            Arg := @ + 2;

         elsif Argument (Arg) = "--enqueue_limit" then
            Parser.Table.McKenzie_Param.Enqueue_Limit := Integer'Value (Argument (Arg + 1));
            Arg := @ + 2;

         elsif Argument (Arg) = "--lang_params" then
            Params.Language_Params := +Argument (Arg + 1);
            Arg := @ + 2;

         elsif Argument (Arg) = "--max_parallel" then
            Parser.Table.Max_Parallel := SAL.Base_Peek_Type'Value (Argument (Arg + 1));
            Arg := @ + 2;

         elsif Argument (Arg) = "--mckenzie_full_explore" then
            WisiToken.Parse.LR.McKenzie_Recover.Force_Full_Explore := True;
            Arg := @ + 1;

         elsif Argument (Arg) = "--mckenzie_high_cost" then
            WisiToken.Parse.LR.McKenzie_Recover.Force_High_Cost_Solutions := True;
            Arg := @ + 1;

         elsif Argument (Arg) = "--mckenzie_task_count" then
            Parser.Table.McKenzie_Param.Task_Count := System.Multiprocessors.CPU_Range'Value (Argument (Arg + 1));
            Arg := @ + 2;

         elsif Argument (Arg) = "--repeat_count" then
            Params.Repeat_Count := Integer'Value (Argument (Arg + 1));
            Arg := @ + 2;

         else
            Ada.Text_IO.Put_Line ("unrecognized option: '" & Argument (Arg) & "'");
            Usage (Parser);
            Set_Exit_Status (Failure);
            raise SAL.Parameter_Error;
         end if;
      end loop;
   exception
   when SAL.Parameter_Error =>
      raise;

   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
      Usage (Parser);
      Set_Exit_Status (Failure);
      raise SAL.Parameter_Error;
   end Command_Options;

   Trace : aliased WisiToken.Text_IO_Trace.Trace;

   procedure Parse_File (Language : in Wisi_Parse_Context.Language)
   is
      use Ada.Text_IO;
      use WisiToken;

      Log_File : Ada.Text_IO.File_Type; -- unused

      Start : Ada.Real_Time.Time;
   begin
      declare
         Arg       : Integer;
         Cl_Params : Command_Line_Params := Command_File_Name (Language.Parse_Data_Template.all, Arg);

         Parse_Context : constant Wisi_Parse_Context.Parse_Context_Access := Wisi_Parse_Context.Find_Create
           (-Cl_Params.Source_File_Name, Language, Trace'Access);

         Parser : WisiToken.Parse.LR.Parser.Parser renames Parse_Context.Parser;

         Parse_Data : Wisi.Parse_Data_Type'Class renames Wisi.Parse_Data_Type'Class (Parser.User_Data.all);
      begin
         Remaining_Command_Params (Parser, Cl_Params, Arg);

         Parser.Trace.Set_Prefix (";; "); -- so we get the same debug messages as Emacs_Wisi_Common_Parse

         begin
            case Cl_Params.Command is
            when Parse_Partial =>
               Parser.Tree.Lexer.Reset_With_File
                 (-Cl_Params.Source_File_Name, Cl_Params.Partial_Begin_Byte_Pos, Cl_Params.Partial_End_Byte_Pos,
                  Cl_Params.Partial_Begin_Char_Pos, Cl_Params.Partial_Begin_Line);

            when Refactor =>
               Parser.Tree.Lexer.Reset_With_File (-Cl_Params.Source_File_Name);

            when Parse_Incremental =>
               declare
                  use GNATCOLL.Mmap;
                  File   : Mapped_File   := Open_Read (-Cl_Params.Source_File_Name);
                  Region : Mapped_Region := Read (File);

               begin
                  Parse_Context.Text_Buffer := new String'(Data (Region) (1 .. Last (Region)));
                  Parse_Context.Text_Buffer_Byte_Last := Parse_Context.Text_Buffer'Last;
                  Parse_Context.Text_Buffer_Char_Last := Parse_Context.Text_Buffer'Last;
                  --  wrong if non-ascii, used in Edit_Source to check KMN. FIXME: get from lexer when it counts lines.

                  Parser.Tree.Lexer.Reset_With_String_Access (Parse_Context.Text_Buffer, Cl_Params.Source_File_Name);
                  Free (Region);
                  Close (File);
               end;
            end case;
         exception
         when Ada.IO_Exceptions.Name_Error =>
            Put_Line (Standard_Error, "'" & (-Cl_Params.Source_File_Name) & "' cannot be opened");
            return;
         end;

         if Cl_Params.End_Line = Invalid_Line_Number then
            --  User did not provide; run lexer to get end line.
            declare
               Token       : Base_Token;
               Lexer_Error : Boolean;
               pragma Unreferenced (Lexer_Error);
            begin
               loop
                  Lexer_Error := Parser.Tree.Lexer.Find_Next (Token);
                  exit when Token.ID = Parser.Tree.Lexer.Descriptor.EOI_ID;
               end loop;
               Cl_Params.End_Line := Token.Line;
            end;
         end if;

         case Cl_Params.Command is
         when Parse_Partial        =>
            Parse_Data.Initialize_Partial_Parse
              (Trace               => Parser.Trace,
               Post_Parse_Action   => Cl_Params.Partial_Post_Parse_Action,
               Action_Region_Bytes => (Cl_Params.Partial_Begin_Byte_Pos, Cl_Params.Partial_Goal_Byte_Pos),
               Begin_Line          => Cl_Params.Partial_Begin_Line,
               End_Line            => Cl_Params.End_Line,
               Begin_Indent        => Cl_Params.Partial_Begin_Indent);

            Command_Options (Parser, Cl_Params, Arg);
            Parse_Data.Parse_Language_Params (-Cl_Params.Language_Params);

            if Cl_Params.Repeat_Count > 1 then
               Start := Ada.Real_Time.Clock;
            end if;

            for I in 1 .. Cl_Params.Repeat_Count loop
               declare
                  procedure Clean_Up
                  is
                     use all type SAL.Base_Peek_Type;
                  begin
                     Parser.Tree.Lexer.Discard_Rest_Of_Input;
                     if Cl_Params.Repeat_Count = 1 and Parser.Parsers.Count > 0 then
                        Parse_Data.Put
                          (Parser.Tree.Lexer.Errors,
                           Parser.Parsers.First.State_Ref.Errors,
                           Parser.Parsers.First.State_Ref.Recover_Insert_Delete,
                           Parser.Tree);
                     end if;
                  end Clean_Up;

               begin
                  Parse_Data.Reset;
                  Parser.Tree.Lexer.Reset;

                  begin
                     Parser.Parse (Log_File);
                  exception
                  when WisiToken.Partial_Parse =>
                     null;
                  end;

                  Parser.Execute_Actions;

                  if Cl_Params.Repeat_Count = 1 then
                     Parse_Data.Put (Parser);
                     Parse_Data.Put
                       (Parser.Tree.Lexer.Errors,
                        Parser.Parsers.First.State_Ref.Errors,
                        Parser.Parsers.First.State_Ref.Recover_Insert_Delete,
                        Parser.Tree);
                  end if;

               exception
               when WisiToken.Syntax_Error =>
                  Clean_Up;
                  Put_Line ("(parse_error)");

               when E : WisiToken.Parse_Error =>
                  Clean_Up;
                  Put_Line ("(parse_error """ & Ada.Exceptions.Exception_Name (E) & " " &
                              Ada.Exceptions.Exception_Message (E) & """)");

               when E : others => -- includes Fatal_Error
                  Clean_Up;
                  Put_Line ("(error """ & Ada.Exceptions.Exception_Name (E) & " " &
                              Ada.Exceptions.Exception_Message (E) & """)");
               end;
            end loop;

            if Cl_Params.Repeat_Count > 1 then
               declare
                  use Ada.Real_Time;
                  Finish : constant Time := Clock;
               begin
                  Put_Line ("Total time:" & Duration'Image (To_Duration (Finish - Start)));
                  Put_Line
                    ("per iteration:" & Duration'Image (To_Duration ((Finish - Start) / Cl_Params.Repeat_Count)));
               end;
            end if;

         when Parse_Incremental | Refactor =>
            --  First do a full parse with default params to get the syntax tree
            Parse_Data.Initialize_Full_Parse (Trace'Access, Cl_Params.End_Line);
            Parser.Tree.Lexer.Reset;
            Parser.Parse (Log_File);

            Command_Options (Parser, Cl_Params, Arg);

            case Cl_Params.Command is
            when Parse_Partial =>
               null;

            when Parse_Incremental =>
               Parse_Data.Parse_Language_Params (-Cl_Params.Language_Params);
               declare
                  KMN_List : WisiToken.Parse.KMN_Lists.List;
               begin
                  Wisi.Edit_Source
                    (Trace,
                     Parse_Context.Text_Buffer,
                     Parse_Context.Text_Buffer_Byte_Last,
                     Parse_Context.Text_Buffer_Char_Last,
                     Cl_Params.Changes,
                     KMN_List);

                  if -Save_File_Name /= "" then
                     declare
                        use Ada.Directories;
                        Save_File : File_Type;
                     begin
                        if Exists (-Save_File_Name) then
                           Delete_File (-Save_File_Name);
                        end if;
                        Create (Save_File, Out_File, -Save_File_Name);
                        Put (Save_File, Parse_Context.Text_Buffer (1 .. Parse_Context.Text_Buffer_Byte_Last));
                        Close (Save_File);
                     end;
                  end if;

                  Parse_Data.Edit (KMN_List, -Cl_Params.Language_Params);

                  Parser.Tree.Lexer.Reset_With_String_Access (Parse_Context.Text_Buffer, Cl_Params.Source_File_Name);

                  Parser.Parse (Log_File, KMN_List);

                  Parse_Data.Reset_Post_Parse
                    (Cl_Params.Inc_Post_Parse_Action,
                     Action_Region_Bytes => (Cl_Params.Inc_Begin_Byte_Pos, Cl_Params.Inc_End_Byte_Pos),
                     Begin_Indent        => Cl_Params.Inc_Begin_Indent,
                     Language_Params     => -Cl_Params.Language_Params);

                  Parser.Execute_Actions;

                  Parse_Data.Put (Parser);
               exception
               when WisiToken.Syntax_Error =>
                  Put_Line ("(parse_error)");

               when E : WisiToken.Parse_Error =>
                  Put_Line ("(parse_error """ & Ada.Exceptions.Exception_Name (E) & " " &
                              Ada.Exceptions.Exception_Message (E) & """)");
               end;

            when Refactor =>
               Parse_Data.Refactor
                 (Parser.Tree,
                  Cl_Params.Refactor_Action, Cl_Params.Edit_Begin);
            end case;
         end case;
      end;
   exception
   when SAL.Parameter_Error | Finish =>
      --  From Get_CL_Params; already handled.
      null;

   when E : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      New_Line (2);
      Put_Line
        ("(error ""unhandled exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
           Ada.Exceptions.Exception_Message (E) & """)");

      if WisiToken.Debug_Mode then
         Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E)); -- includes Prefix
         Trace.New_Line;
      end if;
   end Parse_File;

end Run_Wisi_Common_Parse;
