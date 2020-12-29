--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2020 Free Software Foundation, Inc.
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
      Temp : aliased String (1 .. 3) := (others => ' '); -- initialize for error message
   begin
      Read_Input (Temp'Address, Temp'Length);
      return Integer'Value (Temp);
   exception
   when Constraint_Error =>
      --  From Integer'Value
      raise Protocol_Error with "invalid command byte count; '" & Temp & "'";
   end Get_Command_Length;

   function Get_String
     (Source : in     String;
      Last   : in out Integer)
     return String
   is
      use Ada.Strings.Fixed;
      First : constant Integer := Index
        (Source  => Source,
         Pattern => """",
         From    => Last + 1);
   begin
      Last := Index
        (Source  => Source,
         Pattern => """",
         From    => First + 1);

      if First = 0 or Last = 0 then
         raise Protocol_Error with "at" & Last'Image & ": no '""' found for string";
      end if;

      return Source (First + 1 .. Last - 1);
   end Get_String;

   function Get_Integer
     (Source : in     String;
      Last   : in out Integer)
     return Integer
   is
      use Ada.Strings.Fixed;
      First : constant Integer := Last + 1;
   begin
      Last := Index
        (Source  => Source,
         Pattern => " ",
         From    => First + 1); -- Skip a leading space if present.

      if Last = 0 then
         Last := Source'Last;
      else
         Last := Last - 1;
      end if;

      return Integer'Value (Source (First .. Last));
   exception
   when others =>
      raise Protocol_Error with "at" & First'Image & ": bad integer '" & Source (First .. Last) & "'";
   end Get_Integer;

   procedure Skip
     (Source : in     String;
      Last   : in out Integer;
      Char   : in     Character)
   is begin
      loop
         if Last = Source'Last then
            raise Protocol_Error with "at" & Last'Image & ": expecting '" & Char & "' found EOI";

         elsif Source (Last + 1) = ' ' then
            Last := Last + 1;

         elsif Source (Last + 1) = Char then
            Last := Last + 1;
            exit;
         else
            raise Protocol_Error with
              "at" & Last'Image & ": expecting '" & Char & "' found '" & Source (Last + 1) & "'";
         end if;
      end loop;
   end Skip;

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
            end if;
         end loop;
      end return;
   end Get_Process_Start_Params;

   procedure Edit_Source
     (Source           : in out Ada.Strings.Unbounded.String_Access;
      Source_Byte_Last : in out Integer;
      Source_Char_Last : in out Integer;
      Changes          : in     Change_Lists.List;
      KMN_List         :    out WisiToken.Parse.KMN_Lists.List)
   is
      use Ada.Containers;
      use WisiToken;

      --  Changes is in time order (ie _not_ in buffer pos order); KMN_List
      --  is in buffer pos order.

      Gap_First : Integer := Source_Byte_Last + 1;
      Gap_Last  : Integer := Source'Last;

      function Gap_Invariant return Boolean
      is (Gap_Last - Gap_First = Source'Last - (Source_Byte_Last + 1));

      Total_Inserted_Bytes : Integer := 0;

      function Reallocate return Boolean
      is
         Last_Begin : Base_Buffer_Pos := 0;
         Result     : Boolean         := False;
      begin
         if Changes.Length = 0 then
            return False;
         end if;

         for Change of Changes loop
            --  We loop thru all changes to compute Total_Inserted_Bytes.

            Total_Inserted_Bytes := @ + Ada.Strings.Unbounded.Length (Change.Inserted_Text);

            if Change.Begin_Byte_Pos < Last_Begin then
               Result := True;
            end if;
            Last_Begin := Change.Begin_Byte_Pos;
         end loop;

         if Source_Byte_Last + Total_Inserted_Bytes > Source'Last then
            return True;
         else
            return Result;
         end if;
      end Reallocate;

      procedure Move_Gap (New_Gap_First : in Integer)
      with Pre => New_Gap_First /= Gap_First and Gap_Invariant,
        Post => Gap_Invariant
      is
         --  Examples:
         --  gap_first : 15
         --  gap_last  : 19
         --
         --  new_gap_first: 5
         --     new_gap_last := 9
         --     source (10 .. 19) := source (5 .. 14)
         --
         --  new_gap_first: 25
         --  new_gap_last : 29
         --      source (15 .. 24) := source (20 .. 29)

         New_Gap_Last : constant Integer := New_Gap_First + Gap_Last - Gap_First;
      begin
         if New_Gap_First < Gap_First then
            Source (New_Gap_Last + 1 .. Gap_Last) := Source (New_Gap_First .. Gap_First - 1);
         else
            Source (Gap_First .. New_Gap_First - 1) := Source (Gap_Last + 1 .. New_Gap_Last);
         end if;

         Gap_First := New_Gap_First;
         Gap_Last  := New_Gap_Last;
      end Move_Gap;

      procedure Edit_Text (Change : in Emacs_Wisi_Common_Parse.Change)
      with Pre => Gap_Invariant, Post => Gap_Invariant
      --  Apply Change to Source. Leaves Gap at edit point.
      is
         use Ada.Strings.Unbounded;
         Inserted_Bytes : constant Integer := Integer (Change.Inserted_End_Byte_Pos - Change.Begin_Byte_Pos);
      begin
         if Gap_First /= Integer (Change.Begin_Byte_Pos) then
            Move_Gap (Integer (Change.Begin_Byte_Pos));
         end if;

         if Change.Deleted_Bytes > 0 then
            Gap_Last         := @ + Change.Deleted_Bytes;
            pragma Assert (Gap_Last <= Source'Last);
            Source_Byte_Last := @ - Change.Deleted_Bytes;
            Source_Char_Last := @ - Change.Deleted_Chars;
         end if;

         if Inserted_Bytes > 0 then
            pragma Assert (Gap_Last - Gap_First > Inserted_Bytes);
            Source (Gap_First .. Gap_First + Inserted_Bytes - 1) := -Change.Inserted_Text;

            Gap_First        := Gap_First + Inserted_Bytes;
            Source_Byte_Last := @ + Inserted_Bytes;
            Source_Char_Last := @ + Integer (Change.Inserted_End_Char_Pos - Change.Begin_Char_Pos);
         end if;
      end Edit_Text;

      procedure Insert_KMN (Change : in Emacs_Wisi_Common_Parse.Change)
      is
         use Parse.KMN_Lists;
         Cur : Cursor := KMN_List.First;

         KMN_Last_Byte : Base_Buffer_Pos := 0; --  Last byte of prev KMN.
         KMN_Last_Char : Base_Buffer_Pos := 0; --  Last char of prev KMN.

         function Last_KMN_Invariant return Boolean
         is
            KMN : Parse.KMN renames KMN_List (KMN_List.Last);
         begin
            return KMN.Stable_Bytes = Base_Buffer_Pos (Source_Byte_Last) - KMN_Last_Byte and
              KMN.Stable_Chars = Base_Buffer_Pos (Source_Char_Last) - KMN_Last_Char and
              KMN.Deleted_Bytes = 0 and
              KMN.Deleted_Chars = 0 and
              KMN.Inserted_Bytes = 0 and
              KMN.Inserted_Chars = 0;
         end Last_KMN_Invariant;

         function Max_Byte_Pos (Item : in Emacs_Wisi_Common_Parse.Change) return Buffer_Pos
         is begin
            return Buffer_Pos'Max
              (Item.Begin_Byte_Pos + Base_Buffer_Pos (Item.Deleted_Bytes),
               Item.Inserted_End_Byte_Pos);
         end Max_Byte_Pos;

         function To_KMN (Item : in Emacs_Wisi_Common_Parse.Change) return Parse.KMN
         is (Stable_Bytes   => Item.Begin_Byte_Pos - KMN_Last_Byte - 1, -- Begin_Byte_Pos is deleted or inserted
             Stable_Chars   => Item.Begin_Char_Pos - KMN_Last_Char - 1,
             Deleted_Bytes  => Base_Buffer_Pos (Item.Deleted_Bytes),
             Deleted_Chars  => Base_Buffer_Pos (Item.Deleted_Chars),
             Inserted_Bytes => Item.Inserted_End_Byte_Pos - Item.Begin_Byte_Pos, -- End_Byte_Pos is after last inserted
             Inserted_Chars => Item.Inserted_End_Char_Pos - Item.Begin_Char_Pos);
      begin
         loop
            declare
               Cur_KMN : Parse.KMN renames KMN_List (Cur);
            begin
               pragma Assert (KMN_Last_Byte < Change.Begin_Byte_Pos);

               if Max_Byte_Pos (Change) < KMN_Last_Byte + Cur_KMN.Stable_Bytes + 1 then
                  --  Change is entirely within Cur_KMN.Stable_Bytes
                  declare
                     KMN : constant Parse.KMN := To_KMN (Change);
                  begin
                     Cur_KMN.Stable_Bytes := @ - (KMN.Stable_Bytes + KMN.Deleted_Bytes);
                     Cur_KMN.Stable_Chars := @ - (KMN.Stable_Chars + KMN.Deleted_Chars);

                     KMN_List.Insert (Before => Cur, Element => KMN);

                     KMN_Last_Byte := @ + Cur_KMN.Stable_Bytes + KMN.Inserted_Bytes;
                     KMN_Last_Char := @ + Cur_KMN.Stable_Chars + KMN.Inserted_Chars;

                     Edit_Text (Change);
                     return;
                  end;

               elsif Change.Begin_Byte_Pos = KMN_Last_Byte + Cur_KMN.Stable_Bytes + 1 then
                  --  Change edit point = Cur_KMN edit point; merge them
                  declare
                     Next_KMN : Parse.KMN renames KMN_List (Next (Cur));
                     --  FIXME: fails if Change inserts text at end of Source
                  begin
                     Next_KMN.Stable_Bytes := @ - Base_Buffer_Pos (Change.Deleted_Bytes);
                     Next_KMN.Stable_Chars := @ - Base_Buffer_Pos (Change.Deleted_Chars);
                  end;

                  Cur_KMN.Inserted_Bytes := @ + Change.Inserted_End_Byte_Pos - Change.Begin_Byte_Pos;
                  Cur_KMN.Inserted_Chars := @ + Change.Inserted_End_Char_Pos - Change.Begin_Char_Pos;

                  Cur_KMN.Deleted_Bytes := @ + Base_Buffer_Pos (Change.Deleted_Bytes);
                  Cur_KMN.Deleted_Chars := @ + Base_Buffer_Pos (Change.Deleted_Chars);

                  KMN_Last_Byte := @ + Change.Begin_Byte_Pos - Change.Inserted_End_Byte_Pos -
                    Base_Buffer_Pos (Change.Deleted_Bytes);
                  KMN_Last_Char := @ + Change.Begin_Char_Pos - Change.Inserted_End_Char_Pos -
                    Base_Buffer_Pos (Change.Deleted_Chars);

                  Edit_Text (Change);
                  return;

               elsif Change.Begin_Byte_Pos < KMN_Last_Byte + Cur_KMN.Stable_Bytes + Cur_KMN.Inserted_Bytes then
                  --  Change starts in Cur_KMN inserted text.
                  KMN_Last_Byte := @ + Cur_KMN.Stable_Bytes;
                  KMN_Last_Char := @ + Cur_KMN.Stable_Chars;

                  Cur_KMN.Inserted_Bytes := @ - (Change.Begin_Byte_Pos - KMN_Last_Byte + Cur_KMN.Stable_Bytes);
                  Cur_KMN.Inserted_Chars := @ - (Change.Begin_Char_Pos - KMN_Last_Char + Cur_KMN.Stable_Chars);

                  declare
                     KMN : constant Parse.KMN := To_KMN (Change);
                  begin
                     KMN_List.Insert (Before => Next (Cur), Element => KMN);

                     KMN_Last_Byte := @ + KMN.Inserted_Bytes;
                     KMN_Last_Char := @ + KMN.Inserted_Chars;

                     Edit_Text (Change);
                     return;
                  end;

               else
                  --  Change is entirely after Cur_KMN
                  KMN_Last_Byte := @ + Cur_KMN.Stable_Bytes + Cur_KMN.Inserted_Bytes;
                  KMN_Last_Char := @ + Cur_KMN.Stable_Chars + Cur_KMN.Inserted_Chars;

                  Cur := Next (Cur);

                  if not Has_Element (Cur) then
                     --  Since KMN_List starts with one KMN covering all of Source, we
                     --  should never get here. FIXME: not true if insert text at end of Source!
                     raise SAL.Programmer_Error;

                  elsif Cur = KMN_List.Last then
                     pragma Assert (Last_KMN_Invariant);
                     null;
                  end if;
               end if;
            end;
         end loop;
      end Insert_KMN;

   begin
      if Reallocate then
         declare
            New_Source : constant Ada.Strings.Unbounded.String_Access := new String
              (Source'First .. Source_Byte_Last + Total_Inserted_Bytes);
         begin
            New_Source (Source'First .. Source_Byte_Last) := Source (Source'First .. Source_Byte_Last);
            Ada.Strings.Unbounded.Free (Source);
            Source := New_Source;
         end;

         Gap_Last := Source'Last;
      end if;

      --  Start with one KMN with stable region = entire source. Insert_KMN
      --  edits this, leaving it to cover the final stable region or insert
      --  after Source end.
      KMN_List.Append
        ((Stable_Bytes   => Base_Buffer_Pos (Source_Byte_Last),
          Stable_Chars   => Base_Buffer_Pos (Source_Char_Last),
          Deleted_Bytes  => 0,
          Deleted_Chars  => 0,
          Inserted_Bytes => 0,
          Inserted_Chars => 0));

      for Change of Changes loop
         Insert_KMN (Change);
      end loop;

      if Gap_Last /= Source'Last then
         --  Remove the gap
         Source (Gap_First .. Source_Byte_Last) := Source (Gap_Last + 1 .. Source'Last);
      end if;
   end Edit_Source;

   function Get_Parse_Params (Command_Line : in String; Last : in out Integer) return Parse_Params
   is
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
            Result.End_Byte_Pos      := Get_Integer (Command_Line, Last) - 1; --  Emacs end is after last char.

            Result.Goal_Byte_Pos        := Get_Integer (Command_Line, Last);
            Result.Begin_Char_Pos       := WisiToken.Buffer_Pos (Get_Integer (Command_Line, Last));
            Result.Begin_Line           := WisiToken.Line_Number_Type (Get_Integer (Command_Line, Last));
            Result.End_Line             := WisiToken.Line_Number_Type (Get_Integer (Command_Line, Last));
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
            Result.Byte_Count       := Get_Integer (Command_Line, Last);

            case Kind is
            when Partial => null;
            when Incremental =>
               Skip (Command_Line, Last, '('); --  start of changes list
               loop
                  exit when Last = Command_Line'Last;
                  exit when Command_Line (Last + 1) = ')';

                  declare
                     Item : Change;
                  begin
                     Skip (Command_Line, Last, '(');
                     Item.Begin_Byte_Pos        := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
                     Item.Begin_Char_Pos        := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
                     Item.Inserted_End_Byte_Pos := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
                     Item.Inserted_End_Char_Pos := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
                     Item.Deleted_Bytes         := Get_Integer (Command_Line, Last);
                     Item.Deleted_Chars         := Get_Integer (Command_Line, Last);
                     Item.Inserted_Text         := +Get_String (Command_Line, Last);
                     Skip (Command_Line, Last, ')');

                     Result.Changes.Append (Item);
                  end;
               end loop;
               Skip (Command_Line, Last, ')'); --  end of edits list

            when Full =>
               Result.End_Char_Pos  := Get_Integer (Command_Line, Last);
               Result.Full_End_Line := Line_Number_Type (Get_Integer (Command_Line, Last));
            end case;
         end case;

         Result.Language_Params := +Get_String (Command_Line, Last);

         WisiToken.Enable_Trace (-Result.Verbosity);
      end return;
   exception
   when E : others =>
      raise Protocol_Error with "at" & Last'Image & ": " & Ada.Exceptions.Exception_Message (E);
   end Get_Parse_Params;

   function Get_Post_Parse_Params (Command_Line : in String; Last : in out Integer) return Post_Parse_Params
   is
      use WisiToken;
   begin
      return Result : Post_Parse_Params do

         Result.Source_File_Name  := +Get_String (Command_Line, Last);
         Result.Verbosity         := +Get_String (Command_Line, Last);
         Result.Post_Parse_Action := Wisi.Post_Parse_Action_Type'Val (Get_Integer (Command_Line, Last));
         Result.Begin_Byte_Pos    := Get_Integer (Command_Line, Last);
         Result.End_Byte_Pos      := Get_Integer (Command_Line, Last) - 1; --  Emacs end is after last char.

         WisiToken.Enable_Trace (-Result.Verbosity);
      end return;
   exception
   when E : others =>
      raise Protocol_Error with "at" & Last'Image & ": " & Ada.Exceptions.Exception_Message (E);
   end Get_Post_Parse_Params;

   function Get_Refactor_Params (Command_Line : in String; Last : in out Integer) return Refactor_Params
   is
      use WisiToken;
   begin
      return Result : Refactor_Params do
         --  We don't use an aggregate, to enforce execution order.
         --  Match wisi-process-parse.el wisi-process--send-refactor

         Result.Refactor_Action    := Get_Integer (Command_Line, Last);
         Result.Source_File_Name   := +Get_String (Command_Line, Last);
         Result.Parse_Region.First := WisiToken.Buffer_Pos (Get_Integer (Command_Line, Last));
         Result.Parse_Region.Last  := WisiToken.Buffer_Pos (Get_Integer (Command_Line, Last) - 1);

         Result.Edit_Begin           := WisiToken.Buffer_Pos (Get_Integer (Command_Line, Last));
         Result.Parse_Begin_Char_Pos := WisiToken.Buffer_Pos (Get_Integer (Command_Line, Last));
         Result.Parse_Begin_Line     := WisiToken.Line_Number_Type (Get_Integer (Command_Line, Last));
         Result.Parse_End_Line       := WisiToken.Line_Number_Type (Get_Integer (Command_Line, Last));
         Result.Parse_Begin_Indent   := Get_Integer (Command_Line, Last);
         Result.Verbosity            := +Get_String (Command_Line, Last);
         Result.Max_Parallel         := Get_Integer (Command_Line, Last);
         Result.Byte_Count           := Get_Integer (Command_Line, Last);

         WisiToken.Enable_Trace (-Result.Verbosity);
      end return;
   exception
   when E : others =>
      raise Protocol_Error with "at" & Last'Image & ": " & Ada.Exceptions.Exception_Message (E);
   end Get_Refactor_Params;

   procedure Process_Stream
     (Name                      : in     String;
      Language_Protocol_Version : in     String;
      Params                    : in     Process_Start_Params;
      Language                  : in     Wisi_Parse_Context.Language)
   is
      use Ada.Text_IO;
      use WisiToken; -- "+", "-" Unbounded_string
      use all type Ada.Strings.Unbounded.String_Access;

      Recover_Log_File : Ada.Text_IO.File_Type;

      procedure Check_Command_Length (Command_Length, Last : in Integer)
      is begin
         if Last /= Command_Length then
            raise Protocol_Error with "command length expected" & Command_Length'Image & " got" & Last'Image;
         end if;
      end Check_Command_Length;

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
            is begin
               Last := Command_Line'First + Target'Length - 1;
               return Last <= Command_Line'Last and then Command_Line (Command_Line'First .. Last) = Target;
            end Match;
         begin
            Read_Input (Command_Line'Address, Command_Length);

            Put_Line (";; " & Command_Line);

            if Match ("parse") then
               --  Args: see wisi-process-parse.el wisi-process-parse--send-parse,
               --    wisi-process-parse--send-incremental-parse
               --  Input: <source text>
               --  Response:
               --  [response elisp vector]...
               --  [elisp error form]...
               --  prompt
               declare
                  Params : constant Parse_Params := Get_Parse_Params (Command_Line, Last);

                  Parse_Context : constant Wisi_Parse_Context.Parse_Context_Access := Wisi_Parse_Context.Find_Create
                    (-Params.Source_File_Name, Language, Trace'Access);

                  Parser     : WisiToken.Parse.LR.Parser.Parser renames Parse_Context.Parser;
                  Parse_Data : Wisi.Parse_Data_Type'Class renames Wisi.Parse_Data_Type'Class (Parser.User_Data.all);

                  procedure Clean_Up
                  is
                     use all type SAL.Base_Peek_Type;
                  begin
                     Parser.Tree.Lexer.Discard_Rest_Of_Input;
                     if Parser.Parsers.Count > 0 then
                        Parse_Data.Put
                          (Parser.Tree.Lexer.Errors,
                           Parser.Parsers.First.State_Ref.Errors,
                           Parser.Parsers.First.State_Ref.Recover_Insert_Delete,
                           Parser.Tree);
                     end if;
                  end Clean_Up;

               begin
                  if Last /= Command_Line'Last then
                     raise Protocol_Error with "command length expected" & Command_Length'Image & " got" &
                       Integer'Image (Command_Line'Last);
                  end if;

                  if Params.Task_Count > 0 then
                     Parser.Table.McKenzie_Param.Task_Count := System.Multiprocessors.CPU_Range (Params.Task_Count);
                  end if;
                  if Params.Zombie_Limit > 0 then
                     Parser.Table.McKenzie_Param.Zombie_Limit :=
                       WisiToken.Syntax_Trees.Node_Index (Params.Zombie_Limit);
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
                     Parser.Partial_Parse_Byte_Goal.all := WisiToken.Buffer_Pos (Params.Goal_Byte_Pos);

                     Parse_Data.Initialize_Partial_Parse
                       (Trace               => Parser.Trace,
                        Post_Parse_Action   => Params.Post_Parse_Action,
                        Action_Region_Bytes =>
                          (Base_Buffer_Pos (Params.Begin_Byte_Pos), Base_Buffer_Pos (Params.Goal_Byte_Pos)),
                        Begin_Line          => Params.Begin_Line,
                        End_Line            => Params.End_Line,
                        Begin_Indent        => Params.Begin_Indent);

                     Parse_Data.Parse_Language_Params (-Params.Language_Params);

                     Ada.Strings.Unbounded.Free (Parse_Context.Text_Buffer);
                     Parse_Context.Text_Buffer := new String (Params.Begin_Byte_Pos .. Params.End_Byte_Pos);
                     Parse_Context.Text_Buffer_Byte_Last := Params.Byte_Count;
                     Parse_Context.Text_Buffer_Char_Last := Params.End_Char_Pos;

                     Read_Input (Parse_Context.Text_Buffer (Params.Begin_Byte_Pos)'Address, Params.Byte_Count);

                     Parser.Tree.Lexer.Reset_With_String_Access
                       (Parse_Context.Text_Buffer, Params.Source_File_Name, Params.Begin_Char_Pos, Params.Begin_Line);

                     --  Parser.Line_Begin_Token First, Last set by Lex_All
                     begin
                        Parser.Parse (Recover_Log_File);
                     exception
                     when WisiToken.Partial_Parse =>
                        null;
                     end;

                     Parser.Execute_Actions;
                     Parse_Data.Put (Parser);

                  when Incremental =>

                     --  IMPROVEME: could do incremental parse after partial parse, to
                     --  expand the parsed region.
                     Parser.Partial_Parse_Active.all := False;

                     if Parse_Context.Text_Buffer = null then
                        raise Protocol_Error with "incremental parse with no preceding full parse";
                     end if;

                     declare
                        KMN_List : WisiToken.Parse.KMN_Lists.List;
                     begin
                        Edit_Source
                          (Parse_Context.Text_Buffer,
                           Parse_Context.Text_Buffer_Byte_Last,
                           Parse_Context.Text_Buffer_Char_Last,
                           Params.Changes,
                           KMN_List);

                        Parse_Data.Edit (KMN_List, -Params.Language_Params);

                        Parser.Tree.Lexer.Reset_With_String_Access (Parse_Context.Text_Buffer, Params.Source_File_Name);

                        Parser.Parse (Recover_Log_File, KMN_List);
                        --  No Execute_Actions here; that's done in "post-parse" command
                     end;

                  when Full =>
                     Parser.Partial_Parse_Active.all := False;

                     Parse_Data.Initialize_Full_Parse
                       (Trace    => Parser.Trace,
                        End_Line => Params.Full_End_Line);

                     Parse_Data.Parse_Language_Params (-Params.Language_Params);

                     Ada.Strings.Unbounded.Free (Parse_Context.Text_Buffer);
                     Parse_Context.Text_Buffer := new String (Integer (Buffer_Pos'First) .. Params.Byte_Count);
                     Parse_Context.Text_Buffer_Byte_Last := Params.Byte_Count;
                     Parse_Context.Text_Buffer_Char_Last := Params.End_Char_Pos;
                     Read_Input
                          (Parse_Context.Text_Buffer (Parse_Context.Text_Buffer'First)'Address,
                           Params.Byte_Count);

                     declare
                        KMN_List : WisiToken.Parse.KMN_Lists.List;
                        --  Leave KMN_List empty to do full parse.
                     begin
                        Parse_Data.Initialize_Full_Parse (Trace'Access, Params.Full_End_Line);
                        Parse_Data.Parse_Language_Params (-Params.Language_Params);

                        Parser.Tree.Lexer.Reset_With_String_Access (Parse_Context.Text_Buffer, Params.Source_File_Name);

                        Parser.Parse (Recover_Log_File, KMN_List);
                        --  No Execute_Actions here; that's done in "post-parse" command
                     end;
                  end case;

                  Clean_Up;
               exception
               when Syntax_Error =>
                  Clean_Up;
                  Put_Line ("(parse_error)");

               when E : Parse_Error =>
                  Clean_Up;
                  Put_Line ("(parse_error """ & Ada.Exceptions.Exception_Message (E) & """)");

               when E : Fatal_Error =>
                  Clean_Up;
                  Put_Line ("(error """ & Ada.Exceptions.Exception_Message (E) & """)");
               end;

            elsif Match ("post-parse") then
               --  Args: see wisi-process-parse.el wisi-process-parse--send-action
               --  Input: none
               --  Response:
               --  [response elisp vector]...
               --  [elisp error form]...
               --  prompt
               declare
                  use all type Wisi_Parse_Context.Parse_Context_Access;

                  Params : constant Post_Parse_Params := Get_Post_Parse_Params (Command_Line, Last);

                  Parse_Context : constant Wisi_Parse_Context.Parse_Context_Access := Wisi_Parse_Context.Find
                    (-Params.Source_File_Name, Language);
               begin
                  Check_Command_Length (Command_Length, Last);

                  if Parse_Context = null then
                     raise Protocol_Error with "post-parse without previous parse";
                  end if;
                  declare
                     Parser     : WisiToken.Parse.LR.Parser.Parser renames Parse_Context.Parser;
                     Parse_Data : Wisi.Parse_Data_Type'Class renames Wisi.Parse_Data_Type'Class (Parser.User_Data.all);
                  begin
                     Parse_Data.Reset_Post_Parse
                       (Params.Post_Parse_Action,
                        Action_Region_Bytes =>
                          (Base_Buffer_Pos (Params.Begin_Byte_Pos), Base_Buffer_Pos (Params.End_Byte_Pos)),
                        Language_Params     => -Params.Language_Params);
                     Parser.Execute_Actions;
                     Parse_Data.Put (Parser);
                  end;
               exception
               when E : others =>
                  Put_Line ("(error """ & Ada.Exceptions.Exception_Message (E) & """)");
               end;

            elsif Match ("refactor") then
               --  Args: see wisi-process-parse.el wisi-process-parse--send-refactor
               --  Input: <none>
               --  Response:
               --  [edit elisp vector]...
               --  prompt
               declare
                  Params : constant Refactor_Params := Get_Refactor_Params (Command_Line, Last);

                  Parse_Context : constant Wisi_Parse_Context.Parse_Context_Access := Wisi_Parse_Context.Find_Create
                    (-Params.Source_File_Name, Language, Trace'Access);

                  Parse_Data : Wisi.Parse_Data_Type'Class renames Wisi.Parse_Data_Type'Class
                    (Parse_Context.Parser.User_Data.all);
               begin
                  Check_Command_Length (Command_Length, Last);

                  Parse_Data.Refactor (Parse_Context.Parser.Tree, Params.Refactor_Action, Params.Edit_Begin);
               exception
               when Syntax_Error =>
                  Put_Line ("(parse_error ""refactor " & Params.Parse_Region.First'Image &
                              Params.Parse_Region.Last'Image & ": syntax error"")");

               when E : Parse_Error =>
                  Put_Line ("(parse_error ""refactor " & Params.Parse_Region.First'Image &
                              Params.Parse_Region.Last'Image & ": " & Ada.Exceptions.Exception_Message (E) & """)");

               when E : others => -- includes Fatal_Error
                  Put_Line ("(error """ & Ada.Exceptions.Exception_Message (E) & """)");
               end;

            elsif Match ("quit") then
               exit;

            else
               raise Protocol_Error with  "invalid command: '" & Command_Line & "'";
            end if;
         exception
         when E : Protocol_Error =>
            --  don't exit the loop; allow debugging bad elisp
            Put_Line ("(error ""protocol error " & Ada.Exceptions.Exception_Message (E) & """)");
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

      if WisiToken.Debug_Mode then
         Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      end if;
   end Process_Stream;

end Emacs_Wisi_Common_Parse;
