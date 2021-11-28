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
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;
with SAL;
with System.Multiprocessors;
with System.Storage_Elements;
with WisiToken.Lexer;
with WisiToken.Parse.LR.Parser;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
package body Emacs_Wisi_Common_Parse is

   procedure Usage (Name : in String)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("usage: " & Name & "[--recover-log <file-name>]");
      Put_Line ("enters a loop waiting for commands:");
      Put_Line ("Prompt is '" & Prompt & "'");
      Put_Line ("commands are case sensitive");
      Put_Line ("See wisi-process-parse.el *--send-parse, *--send-noop for arguments.");
   end Usage;

   Trace : aliased WisiToken.Text_IO_Trace.Trace;

   Trace_Protocol : Natural := 0;

   procedure Read_Input (A : System.Address; N : Integer)
   is
      use System.Storage_Elements;

      B         : System.Address := A;
      Remaining : Integer        := N;
      Read      : Integer;
   begin
      --  We use GNAT.OS_Lib because it does not buffer input, so it runs
      --  under Emacs nicely; GNAT Text_IO does not return text until
      --  some fairly large buffer is filled.
      --
      --  With GNAT GPL 2016, GNAT.OS_Lib.Read does _not_ wait for all N
      --  bytes or EOF; it returns as soon as it gets some bytes.
      loop
         Read := GNAT.OS_Lib.Read (GNAT.OS_Lib.Standin, B, Remaining);
         if Read = 0 then
            --  Pipe closed; probably parent Emacs crashed. Force exit.
            raise SAL.Programmer_Error with "input pipe closed";
         end if;
         Remaining := Remaining - Read;
         exit when Remaining <= 0;
         B := B + Storage_Offset (Read);
      end loop;
   end Read_Input;

   function Get_Command_Length return Integer
   is
      --  Length must match wisi-process-parse.el
      --  wisi-process-parse--add-cmd-length. Initialize for error message.
      Temp : aliased String (1 .. 4) := (others => ' ');
   begin
      Read_Input (Temp'Address, Temp'Length);
      return Integer'Value (Temp);
   exception
   when Constraint_Error =>
      --  From Integer'Value
      raise Wisi.Protocol_Error with "invalid command byte count; '" & Temp & "'";
   end Get_Command_Length;

   procedure Check_Command_Length (Command_Length, Last : in Integer)
   is begin
      if Last /= Command_Length then
         raise Wisi.Protocol_Error with "command length expected" & Command_Length'Image & " got" & Last'Image;
      end if;
   end Check_Command_Length;

   function Get_Process_Start_Params return Process_Start_Params
   is
      use Ada.Command_Line;
      procedure Put_Usage
      is
         use Ada.Text_IO;
      begin
         Put_Line (Standard_Error, "process start args:");
         Put_Line (Standard_Error, "--help : put this help");
         Put_Line (Standard_Error, "--recover-log <file_name> : log recover actions to file");
         Put_Line (Standard_Error, "--trace_protocol <n> : 0 = none, 1 = echo commands");
      end Put_Usage;

      Next_Arg : Integer := 1;
   begin
      return Result : Process_Start_Params do
         loop
            exit when Next_Arg > Argument_Count;

            if Next_Arg <= Argument_Count and then Argument (Next_Arg) = "--help" then
               Put_Usage;
               raise Finish;

            elsif Next_Arg + 1 <= Argument_Count and then Argument (Next_Arg) = "--recover-log" then
               Result.Recover_Log_File_Name := Ada.Strings.Unbounded.To_Unbounded_String (Argument (Next_Arg + 1));
               Next_Arg := Next_Arg + 2;

            elsif Next_Arg + 1 <= Argument_Count and then Argument (Next_Arg) = "--trace_protocol" then
               Trace_Protocol := Integer'Value (Argument (Next_Arg + 1));
               Next_Arg := Next_Arg + 2;

            else
               raise Wisi.Protocol_Error with "invalid process arg '" & Argument (Next_Arg) & "'";
            end if;
         end loop;
      end return;
   end Get_Process_Start_Params;

   function Get_Parse_Params (Command_Line : in String; Last : in out Integer) return Parse_Params
   is
      use Wisi;
      use WisiToken;
      Kind : constant Parse_Kind := Parse_Kind'Val (Get_Integer (Command_Line, Last));
   begin
      return Result : Parse_Params (Kind) do
         --  We don't use an aggregate, to enforce execution order.
         --  Match wisi-process-parse.el wisi-process-parse--send-parse, wisi-process-parse--send-incremental-parse
         case Kind is
         when Partial =>
            Result.Post_Parse_Action := Wisi.Post_Parse_Action_Type'Val (Get_Integer (Command_Line, Last));
            Result.Source_File_Name  := +Get_String (Command_Line, Last);
            Result.Begin_Byte_Pos    := Get_Integer (Command_Line, Last);
            Result.End_Byte_Pos      := Get_Integer (Command_Line, Last) - 1;
            --  Emacs end is after last byte.
            Result.Goal_Byte_Pos        := Get_Integer (Command_Line, Last);
            Result.Begin_Char_Pos       := Buffer_Pos (Get_Integer (Command_Line, Last));
            Result.End_Char_Pos         := Buffer_Pos (Get_Integer (Command_Line, Last)) - 1;
            Result.Begin_Line           := Line_Number_Type (Get_Integer (Command_Line, Last));
            Result.Begin_Indent         := Get_Integer (Command_Line, Last);
            Result.Partial_Parse_Active := 1 = Get_Integer (Command_Line, Last);
            Result.Verbosity            := +Get_String (Command_Line, Last);
            Result.Task_Count           := Get_Integer (Command_Line, Last);
            Result.Zombie_Limit         := Get_Integer (Command_Line, Last);
            Result.Enqueue_Limit        := Get_Integer (Command_Line, Last);
            Result.Max_Parallel         := Get_Integer (Command_Line, Last);

         when Incremental | Full =>
            Result.Source_File_Name := +Get_String (Command_Line, Last);
            Result.Verbosity        := +Get_String (Command_Line, Last);
            Result.Task_Count       := Get_Integer (Command_Line, Last);
            Result.Zombie_Limit     := Get_Integer (Command_Line, Last);
            Result.Enqueue_Limit    := Get_Integer (Command_Line, Last);
            Result.Max_Parallel     := Get_Integer (Command_Line, Last);

            case Kind is
            when Partial => null;
            when Incremental =>
               Result.Changes := Wisi.Parse_Context.Get_Emacs_Change_List (Command_Line, Last);

            when Full =>
               Result.Byte_Count        := Get_Integer (Command_Line, Last);
               Result.Full_End_Char_Pos := Buffer_Pos (Get_Integer (Command_Line, Last)) - 1;
            end case;
         end case;

         Result.Language_Params := +Get_String (Command_Line, Last);

         Enable_Trace (-Result.Verbosity);

         Check_Command_Length (Command_Line'Last, Last);
      end return;
   exception
   when Protocol_Error =>
      raise;
   when E : others =>
      raise Protocol_Error with "at" & Last'Image & ": " & Ada.Exceptions.Exception_Message (E);
   end Get_Parse_Params;

   function Get_Post_Parse_Params (Command_Line : in String; Last : in out Integer) return Post_Parse_Params
   is
      use Wisi;
      use WisiToken;
   begin
      return Result : Post_Parse_Params do

         Result.Source_File_Name  := +Get_String (Command_Line, Last);
         Result.Verbosity         := +Get_String (Command_Line, Last);
         Result.Post_Parse_Action := Wisi.Post_Parse_Action_Type'Val (Get_Integer (Command_Line, Last));
         Result.Begin_Byte_Pos    := Get_Integer (Command_Line, Last);
         Result.Begin_Char_Pos    := Get_Integer (Command_Line, Last);

         --  Emacs end is after last char. FIXME: if last char is
         --  multibyte, this is wrong; subtract 1 char in elisp.
         Result.End_Byte_Pos      := Get_Integer (Command_Line, Last) - 1;
         Result.End_Char_Pos      := Get_Integer (Command_Line, Last) - 1;

         Result.Language_Params   := +Get_String (Command_Line, Last);

         Enable_Trace (-Result.Verbosity);
         Check_Command_Length (Command_Line'Last, Last);
      end return;
   exception
   when Protocol_Error =>
      raise;
   when E : others =>
      raise Protocol_Error with "at" & Last'Image & ": " & Ada.Exceptions.Exception_Message (E);
   end Get_Post_Parse_Params;

   function Get_Refactor_Params (Command_Line : in String; Last : in out Integer) return Refactor_Params
   is
      use Wisi;
      use WisiToken;
   begin
      return Result : Refactor_Params do
         --  We don't use an aggregate, to enforce execution order.
         --  Match wisi-process-parse.el wisi-process--send-refactor

         Result.Source_File_Name   := +Get_String (Command_Line, Last);
         Result.Refactor_Action    := Refactor_Action (Get_Integer (Command_Line, Last));

         Result.Edit_Begin := Buffer_Pos (Get_Integer (Command_Line, Last));
         Result.Verbosity  := +Get_String (Command_Line, Last);

         Enable_Trace (-Result.Verbosity);
         Check_Command_Length (Command_Line'Last, Last);
      end return;
   exception
   when Protocol_Error =>
      raise;
   when E : others =>
      raise Protocol_Error with "at" & Last'Image & ": " & Ada.Exceptions.Exception_Message (E);
   end Get_Refactor_Params;

   procedure Process_Stream
     (Name                      : in     String;
      Language_Protocol_Version : in     String;
      Params                    : in     Process_Start_Params;
      Language                  : in     Wisi.Parse_Context.Language)
   is
      use Ada.Text_IO;
      use WisiToken; -- "+", "-" Unbounded_string
      use all type Ada.Strings.Unbounded.String_Access;
      use all type Wisi.Parse_Context.Parse_Context_Access;

      Recover_Log_File : Ada.Text_IO.File_Type;

      procedure Cleanup
      is begin
         if Is_Open (Recover_Log_File) then
            Close (Recover_Log_File);
         end if;
      end Cleanup;

   begin
      declare
         use Ada.Directories;
         use Ada.Strings.Unbounded;
      begin
         if Length (Params.Recover_Log_File_Name) > 0 then
            Put_Line (";; logging to '" & (-Params.Recover_Log_File_Name) & "'");
            --  to Current_Output, visible from Emacs

            if Exists (-Params.Recover_Log_File_Name) then
               Open (Recover_Log_File, Append_File, -Params.Recover_Log_File_Name);
            else
               Create (Recover_Log_File, Out_File, -Params.Recover_Log_File_Name);
            end if;
         end if;
      end;

      Trace.Set_Prefix (";; "); -- so debug messages don't confuse Emacs.

      Put_Line
        (Name & " protocol: process version " & Protocol_Version & " language version " & Language_Protocol_Version);

      --  Read commands and tokens from standard_input via GNAT.OS_Lib,
      --  send results to standard_output.
      loop
         Put (Prompt); Flush;
         declare
            Command_Length : constant Integer := Get_Command_Length;
            Command_Line   : aliased String (1 .. Command_Length);
            Last           : Integer;

            function Match (Target : in String) return Boolean
            is
               use Ada.Strings.Fixed;
            begin
               Last := Index (Source => Command_Line, Pattern => " ");
               if Last = 0 then
                  Last := Command_Line'Last;
               else
                  Last := Last - 1;
               end if;

               return Last = Target'Length and then Command_Line (Command_Line'First .. Last) = Target;
            end Match;
         begin
            Read_Input (Command_Line'Address, Command_Length);

            if Trace_Protocol > WisiToken.Outline then
               Trace.Put_Line ("'" & Command_Line & "' length:" & Command_Length'Image);
            end if;

            if Match ("compare_tree_text_auto") then
               --  Args: source_file_name
               --  Input: <none>
               --  Response:
               --  message, prompt
               --
               --  Compare tree to fresh full parse after each incremental edit.
               declare
                  Source_File_Name : constant String := Wisi.Get_String (Command_Line, Last);

                  Parse_Context : constant Wisi.Parse_Context.Parse_Context_Access := Wisi.Parse_Context.Find
                    (Source_File_Name, Language);
               begin
                  Check_Command_Length (Command_Length, Last);

                  Parse_Context.Compare_Tree_Text_Auto := True;

                  Put_Line ("(message ""auto compare tree text enabled"")");
               end;

            elsif Match ("kill-context") then
               Wisi.Parse_Context.Kill (File_Name => Wisi.Get_String (Command_Line, Last));

            elsif Match ("parse") then
               --  Args: see wisi-process-parse.el wisi-process-parse--send-parse,
               --    wisi-process-parse--send-incremental-parse
               --  Input: <source text>
               --  Response:
               --  [response elisp vector]...
               --  [elisp error form]...
               --  prompt
               declare
                  Params : constant Parse_Params := Get_Parse_Params (Command_Line, Last);

                  Parse_Context : constant Wisi.Parse_Context.Parse_Context_Access :=
                    (case Params.Kind is
                     when Full | Partial => Wisi.Parse_Context.Find_Create
                       (-Params.Source_File_Name, Language, Trace'Access),
                     when Incremental => Wisi.Parse_Context.Find
                       (-Params.Source_File_Name, Language, Have_Text => True));

                  Parser     : Parse.LR.Parser.Parser renames Parse_Context.Parser;
                  Parse_Data : Wisi.Parse_Data_Type'Class renames Wisi.Parse_Data_Type'Class (Parser.User_Data.all);
               begin
                  if Params.Task_Count > 0 then
                     Parser.Table.McKenzie_Param.Task_Count := System.Multiprocessors.CPU_Range (Params.Task_Count);
                  end if;
                  if Params.Zombie_Limit > 0 then
                     Parser.Table.McKenzie_Param.Zombie_Limit := Params.Zombie_Limit;
                  end if;
                  if Params.Enqueue_Limit > 0 then
                     Parser.Table.McKenzie_Param.Enqueue_Limit := Params.Enqueue_Limit;
                  end if;
                  if Params.Max_Parallel > 0 then
                     Parser.Table.Max_Parallel := SAL.Base_Peek_Type (Params.Max_Parallel);
                  end if;

                  case Params.Kind is
                  when Partial =>
                     Parser.Partial_Parse_Active.all    := Params.Partial_Parse_Active;
                     Parser.Partial_Parse_Byte_Goal.all := Buffer_Pos (Params.Goal_Byte_Pos);

                     Parse_Data.Initialize (Trace'Access);

                     Parse_Data.Parse_Language_Params (-Params.Language_Params);

                     Ada.Strings.Unbounded.Free (Parse_Context.Text_Buffer);
                     Parse_Context.Text_Buffer := new String (Params.Begin_Byte_Pos .. Params.End_Byte_Pos);
                     Parse_Context.Text_Buffer_Byte_Last := Params.End_Byte_Pos;
                     Parse_Context.Text_Buffer_Char_Last := Integer (Params.End_Char_Pos);

                     Read_Input
                       (Parse_Context.Text_Buffer (Params.Begin_Byte_Pos)'Address,
                        Parse_Context.Text_Buffer'Length);

                     if Ada.Strings.Unbounded.Length (Parse_Context.Root_Save_Edited_Name) /= 0 then
                        Parse_Context.Save_Text_Auto;
                     end if;

                     Parser.Tree.Lexer.Reset_With_String_Access
                       (Parse_Context.Text_Buffer, Parse_Context.Text_Buffer_Byte_Last, Params.Source_File_Name,
                        Params.Begin_Char_Pos, Params.Begin_Line);

                     --  Parser.Line_Begin_Token First, Last set by Lex_All in Parse.

                     Parser.Parse (Recover_Log_File);
                     --  Raises Parse_Error for ambiguous parse and similar errors.

                     Parse_Data.Reset_Post_Parse
                       (Parser.Tree, Params.Post_Parse_Action,
                        Action_Region_Bytes =>
                          (Base_Buffer_Pos (Params.Begin_Byte_Pos), Base_Buffer_Pos (Params.End_Byte_Pos)),
                        Action_Region_Chars => (Params.Begin_Char_Pos, Params.End_Char_Pos),
                        Begin_Indent        => Params.Begin_Indent);

                     Parser.Execute_Actions (Action_Region_Bytes => Parse_Data.Action_Region_Bytes);
                     Parse_Data.Put (Parser);

                  when Incremental =>

                     if Parse_Context.Text_Buffer = null then
                        raise Wisi.Parse_Context.Not_Found;
                     end if;

                     --  IMPROVEME: could do incremental parse after partial parse, to
                     --  expand the parsed region.
                     Parser.Partial_Parse_Active.all := False;

                     declare
                        KMN_List : Parse.KMN_Lists.List;
                     begin
                        Wisi.Parse_Context.Edit_Source (Trace, Parse_Context.all, Params.Changes, KMN_List);

                        if Ada.Strings.Unbounded.Length (Parse_Context.Root_Save_Edited_Name) /= 0 then
                           Parse_Context.Save_Text_Auto;
                        end if;

                        Parse_Data.Parse_Language_Params (-Params.Language_Params);

                        Parser.Tree.Lexer.Reset_With_String_Access
                          (Parse_Context.Text_Buffer,
                           Parse_Context.Text_Buffer_Byte_Last,
                           Params.Source_File_Name);

                        if Parser.Tree.Editable then
                           Parser.Parse (Recover_Log_File, KMN_List);

                           if Parse_Context.Compare_Tree_Text_Auto then
                              Parse_Context.Compare_Tree_Text;
                           end if;
                        else
                           --  Last parse failed; can't edit tree, so do full parse.
                           --
                           --  FIXME: Edit_Tree should handle a partially parsed tree
                           Parser.Parse (Recover_Log_File, Parse.KMN_Lists.Empty_List);
                        end if;

                        --  No Execute_Actions here; that's done in "post-parse" command
                     end;

                  when Full =>
                     Parser.Partial_Parse_Active.all := False;

                     Parse_Data.Initialize (Trace'Access);

                     Parse_Data.Parse_Language_Params (-Params.Language_Params);

                     Ada.Strings.Unbounded.Free (Parse_Context.Text_Buffer);
                     Parse_Context.Text_Buffer := new String (Integer (Buffer_Pos'First) .. Params.Byte_Count);
                     Parse_Context.Text_Buffer_Byte_Last := Params.Byte_Count;
                     Parse_Context.Text_Buffer_Char_Last := Integer (Params.Full_End_Char_Pos);
                     Read_Input
                       (Parse_Context.Text_Buffer (Parse_Context.Text_Buffer'First)'Address,
                        Params.Byte_Count);

                     Parser.Tree.Lexer.Reset_With_String_Access
                       (Parse_Context.Text_Buffer, Parse_Context.Text_Buffer_Byte_Last, Params.Source_File_Name);

                     declare
                        KMN_List : Parse.KMN_Lists.List;
                        --  Leave KMN_List empty to do full parse.
                     begin
                        Parser.Parse (Recover_Log_File, KMN_List);
                        --  No Execute_Actions here; that's done in "post-parse" command
                     end;
                  end case;

                  Parse_Data.Put
                    (Parser.Parsers.First.State_Ref.Recover_Insert_Delete,
                     Parser.Tree);
               exception
               when Wisi.Parse_Context.Not_Found =>
                  raise;

               when WisiToken.Syntax_Error | WisiToken.Parse_Error =>
                  Parser.Tree.Lexer.Discard_Rest_Of_Input;
                  if Parser.Tree.Stream_Count >= 2 then
                     WisiToken.Parse.Put_Errors (Parser, Parser.Tree.First_Parse_Stream);
                  else
                     --  Probably an error in Edit_Tree
                     Parser.Put_Errors (Parser.Tree.Shared_Stream);
                  end if;
                  raise;

               when others =>
                  Parser.Tree.Lexer.Discard_Rest_Of_Input;
                  raise;
               end;

            elsif Match ("post-parse") then
               --  Args: see wisi-process-parse.el wisi-process-parse--send-action
               --  Input: none
               --  Response:
               --  [response elisp vector]...
               --  [elisp error form]...
               --  prompt
               declare
                  Params : constant Post_Parse_Params := Get_Post_Parse_Params (Command_Line, Last);

                  Parse_Context : constant Wisi.Parse_Context.Parse_Context_Access := Wisi.Parse_Context.Find
                    (-Params.Source_File_Name, Language, Have_Text => True);

                  Parser     : Parse.LR.Parser.Parser renames Parse_Context.Parser;
                  Parse_Data : Wisi.Parse_Data_Type'Class renames Wisi.Parse_Data_Type'Class (Parser.User_Data.all);
               begin
                  Parse_Data.Reset_Post_Parse
                    (Parser.Tree, Params.Post_Parse_Action,
                     Action_Region_Bytes =>
                       (Base_Buffer_Pos (Params.Begin_Byte_Pos), Base_Buffer_Pos (Params.End_Byte_Pos)),
                     Action_Region_Chars =>
                       (Base_Buffer_Pos (Params.Begin_Char_Pos), Base_Buffer_Pos (Params.End_Char_Pos)),
                     Begin_Indent        => 0);

                  Parse_Data.Parse_Language_Params (-Params.Language_Params);

                  Parser.Execute_Actions (Action_Region_Bytes => Parse_Data.Action_Region_Bytes);
                  Parse_Data.Put (Parser);
               end;

            elsif Match ("refactor") then
               --  Args: see wisi-process-parse.el wisi-process-parse--send-refactor
               --  Input: <none>
               --  Response:
               --  [edit elisp vector]...
               --  prompt
               declare
                  Params : constant Refactor_Params := Get_Refactor_Params (Command_Line, Last);

                  Parse_Context : constant Wisi.Parse_Context.Parse_Context_Access := Wisi.Parse_Context.Find
                    (-Params.Source_File_Name, Language);

                  Parse_Data : Wisi.Parse_Data_Type'Class renames Wisi.Parse_Data_Type'Class
                    (Parse_Context.Parser.User_Data.all);
               begin
                  Parse_Data.Refactor (Parse_Context.Parser.Tree, Params.Refactor_Action, Params.Edit_Begin);
               end;

            elsif Match ("query-tree") then
               --  Args: see wisi-process-parse.el wisi-process-parse--send-query
               --  Input: <none>
               --  Response:
               --  [elisp vector]...
               --  prompt
               declare
                  use Wisi;
                  Source_File_Name : constant Ada.Strings.Unbounded.Unbounded_String :=
                    +Wisi.Get_String (Command_Line, Last);

                  Label : constant Wisi.Query_Label := Wisi.Query_Label'Val (Wisi.Get_Integer (Command_Line, Last));

                  Parse_Context : constant Wisi.Parse_Context.Parse_Context_Access := Wisi.Parse_Context.Find
                    (-Source_File_Name, Language);

                  Parse_Data : Wisi.Parse_Data_Type'Class renames Wisi.Parse_Data_Type'Class
                    (Parse_Context.Parser.User_Data.all);
               begin
                  case Label is
                  when Point_Query =>
                     declare
                        Point : constant WisiToken.Buffer_Pos := WisiToken.Buffer_Pos
                          (Wisi.Get_Integer (Command_Line, Last));
                        IDs : constant WisiToken.Token_ID_Arrays.Vector :=
                          (case Point_Query'(Label) is
                           when Node | Containing_Statement => WisiToken.Token_ID_Arrays.Empty_Vector,
                           when Ancestor => Wisi.Get_Token_IDs (Parse_Data, Command_Line, Last));
                        Query : constant Wisi.Query :=
                          (case Point_Query'(Label) is
                           when Node => (Node, Point),
                           when Containing_Statement => (Containing_Statement, Point),
                           when Ancestor => (Ancestor, Point, IDs));
                     begin
                        Check_Command_Length (Command_Length, Last);

                        Wisi.Query_Tree (Parse_Data, Parse_Context.Parser.Tree, Query);
                     end;

                  when Parent | Child =>
                     declare
                        Address : constant String := Wisi.Get_String (Command_Line, Last);
                        Node    : constant WisiToken.Syntax_Trees.Valid_Node_Access := Wisi.To_Node_Access (Address);
                        N       : constant Integer := Wisi.Get_Integer (Command_Line, Last);
                     begin
                        Check_Command_Length (Command_Length, Last);

                        Wisi.Query_Tree (Parse_Data, Parse_Context.Parser.Tree, (Node_Query'(Label), Node, N));
                     end;

                  when Print =>
                     Check_Command_Length (Command_Length, Last);

                     Wisi.Query_Tree (Parse_Data, Parse_Context.Parser.Tree, (Label => Print));
                  end case;
               end;

            elsif Match ("save_text") then
               --  Args: source_file_name save_file_name
               --  Input: <none>
               --  Response:
               --  (message "text saved ...)
               --  prompt
               declare
                  Source_File_Name : constant String := Wisi.Get_String (Command_Line, Last);
                  Save_File_Name   : constant String := Wisi.Get_String (Command_Line, Last);

                  Parse_Context : constant Wisi.Parse_Context.Parse_Context_Access := Wisi.Parse_Context.Find
                    (Source_File_Name, Language);
               begin
                  Check_Command_Length (Command_Length, Last);

                  Parse_Context.Save_Text (Save_File_Name);
               end;

            elsif Match ("save_text_auto") then
               --  Args: source_file_name root_save_file_name
               --  Input: <none>
               --  Response:
               --  prompt
               --
               --  Save text after each incremental edit, to
               --  <root_save_file_name_nnn>, where 'nnn' is a three-digit number
               --  that increments.
               declare
                  Source_File_Name : constant String := Wisi.Get_String (Command_Line, Last);
                  Save_File_Name   : constant String := Wisi.Get_String (Command_Line, Last);

                  --  We need "create" here for partial parse.
                  Parse_Context : constant Wisi.Parse_Context.Parse_Context_Access := Wisi.Parse_Context.Find_Create
                    (Source_File_Name, Language, Trace'Access);
               begin
                  Check_Command_Length (Command_Length, Last);

                  Parse_Context.Root_Save_Edited_Name := +Save_File_Name;
                  Parse_Context.Save_Edited_Count     := 0;

                  Put_Line ("(message ""auto text save enabled, to '" & Save_File_Name & "_nnn'"")");
               end;

            elsif Match ("quit") then
               exit;

            else
               raise Wisi.Protocol_Error with  "invalid command: '" & Command_Line & "'";
            end if;
         exception
         when Wisi.Parse_Context.Not_Found =>
            --  Tell Emacs to send full text
            Put_Line ("(file_not_found)");

         when E : Syntax_Error | Parse_Error =>
            Put_Line ("(parse_error """ & Ada.Exceptions.Exception_Message (E) & """)");

         when E : Wisi.Protocol_Error =>
            --  don't exit the loop; allow debugging bad elisp
            Put_Line ("(error ""protocol error " & Ada.Exceptions.Exception_Message (E) & """)");

         when E : others => -- includes Fatal_Error
            if WisiToken.Debug_Mode then
               Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
            end if;
            Put_Line
              ("(error ""error: " & Ada.Exceptions.Exception_Name (E) & " : " &
                 Ada.Exceptions.Exception_Message (E) & """)");
         end;
      end loop;
      Cleanup;
   exception
   when Finish =>
      null;

   when E : others =>
      Cleanup;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      New_Line (2);
      Put_Line
        ("(error ""unhandled exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
           Ada.Exceptions.Exception_Message (E) & """)");

      if Debug_Mode then
         Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      end if;
   end Process_Stream;

end Emacs_Wisi_Common_Parse;
