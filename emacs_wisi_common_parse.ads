--  Abstract :
--
--  Common utilities for Gen_Emacs_Wisi_*_Parse
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
with System;
with Wisi;
with WisiToken.Parse;
with Wisi_Parse_Context;
package Emacs_Wisi_Common_Parse is

   Protocol_Version : constant String := "6";
   --  Protocol_Version defines the data sent between elisp and the
   --  background process, except for the language-specific parameters,
   --  which are defined by the Language_Protocol_Version parameter to
   --  Parse_Stream, below.
   --
   --  This value must match wisi-process-parse.el
   --  wisi-process-parse-protocol-version.
   --
   --  See wisi-process-parse.el functions, and this package body, for
   --  the implementation of the protocol.
   --
   --  Only changes once per wisi release. Increment as soon as required,
   --  record new version in NEWS-wisi.text. If working on a branch and
   --  main has already incremented, increment again, in case main is
   --  released before branch is merged; leave two "increment protocol"
   --  lines in NEWS-wisi.text to indicate the issue.

   Prompt : constant String := ";;> ";

   Protocol_Error : exception;
   Finish         : exception;

   procedure Usage (Name : in String);

   procedure Read_Input (A : System.Address; N : Integer);

   function Get_Command_Length return Integer;

   function Get_String
     (Source : in     String;
      Last   : in out Integer)
     return String;

   function Get_Integer
     (Source : in     String;
      Last   : in out Integer)
     return Integer;

   procedure Skip
     (Source : in     String;
      Last   : in out Integer;
      Char   : in     Character);
   --  Check that Source (Last + 1) = Char. If so, increment Last.
   --  If not, raise Protocol_Error.

   type Process_Start_Params is record
      Recover_Log_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      --  log enabled if non-empty.
   end record;

   function Get_Process_Start_Params return Process_Start_Params;
   --  Get from Ada.Command_Line. Handles --help by outputing help,
   --  raising Finish.

   procedure Process_Stream
     (Name                      : in     String;
      Language_Protocol_Version : in     String;
      Params                    : in     Process_Start_Params;
      Language                  : in     Wisi_Parse_Context.Language);

   ----------
   --  Parse command

   type Change is record
      Begin_Byte_Pos : WisiToken.Buffer_Pos;
      Begin_Char_Pos : WisiToken.Buffer_Pos;
      End_Byte_Pos   : WisiToken.Buffer_Pos;
      End_Char_Pos   : WisiToken.Buffer_Pos; --  emacs convention: end is _after_ last inserted char
      Inserted_Text  : Ada.Strings.Unbounded.Unbounded_String;
      Deleted_Bytes  : Integer;
      Deleted_Chars  : Integer;
   end record;

   package Change_Lists is new Ada.Containers.Doubly_Linked_Lists (Change);

   procedure Edit_Source
     (Source           : in out Ada.Strings.Unbounded.String_Access;
      Source_Byte_Last : in out Integer;
      Source_Char_Last : in out Integer;
      Changes          : in     Change_Lists.List;
      KMN_List         :    out WisiToken.Parse.KMN_Lists.List);

   type Parse_Params (Incremental : Boolean) is record

      Source_File_Name  : Ada.Strings.Unbounded.Unbounded_String;

      Verbosity     : Ada.Strings.Unbounded.Unbounded_String;
      Task_Count    : Integer;
      Zombie_Limit  : Integer;
      Enqueue_Limit : Integer;
      Max_Parallel  : Integer;

      case Incremental is
      when False =>
         Post_Parse_Action : Wisi.Post_Parse_Action_Type;
         Begin_Byte_Pos : Integer;
         --  Source file byte position of first char sent; start parse here.

         End_Byte_Pos : Integer;
         --  Byte position of last char sent.

         Goal_Byte_Pos : Integer;
         --  Byte position of end of desired parse region; terminate parse at
         --  or after here.

         Begin_Char_Pos : WisiToken.Buffer_Pos;
         --  Char position of first char sent.

         Begin_Line : WisiToken.Line_Number_Type;
         End_Line   : WisiToken.Line_Number_Type;
         --  Line number of line containing Begin_Byte_Pos, End_Byte_Pos

         Begin_Indent : Integer;
         --  Indentation of Line_Begin

         Partial_Parse_Active : Boolean;
         Byte_Count           : Integer;
         --  Count of bytes of source file sent.

      when True =>
         Initial_Full_Parse : Boolean;
         Changes            : Change_Lists.List;

      end case;
   end record;

   function Get_Parse_Params (Command_Line : in String; Last : in out Integer) return Parse_Params;

   ----------
   --  Post-Parse command

   type Post_Parse_Params is record
      Source_File_Name  : Ada.Strings.Unbounded.Unbounded_String;
      Verbosity         : Ada.Strings.Unbounded.Unbounded_String;
      Post_Parse_Action : Wisi.Post_Parse_Action_Type;
      Begin_Byte_Pos    : Integer;
      End_Byte_Pos      : Integer;
      --  Region to execute action in.
   end record;

   function Get_Post_Parse_Params (Command_Line : in String; Last : in out Integer) return Post_Parse_Params;

   ----------
   --  Refactor command

   type Refactor_Params is record
      Refactor_Action  : Positive; -- Language-specific
      Source_File_Name : Ada.Strings.Unbounded.Unbounded_String;

      Parse_Region : WisiToken.Buffer_Region;
      --  Source file byte region to parse.

      Edit_Begin : WisiToken.Buffer_Pos;
      --  Source file byte position at start of expression to refactor.

      Parse_Begin_Char_Pos : WisiToken.Buffer_Pos;
      --  Char position of first char sent.

      Parse_Begin_Line : WisiToken.Line_Number_Type;
      Parse_End_Line   : WisiToken.Line_Number_Type;
      --  Line numbers of lines containing Parse_Begin_Byte_Pos, Parse_End_Byte_Pos

      Parse_Begin_Indent : Integer;
      --  Indentation of Parse_Begin_Line

      Verbosity    : Ada.Strings.Unbounded.Unbounded_String;
      Max_Parallel : Integer;
      Byte_Count   : Integer;
      --  Count of bytes of source file sent.
   end record;

   function Get_Refactor_Params (Command_Line : in String; Last : in out Integer) return Refactor_Params;

end Emacs_Wisi_Common_Parse;
