--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2020, 2021 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with Ada.Directories;
with Ada.Finalization;
with Ada.Text_IO;
with GNAT.OS_Lib;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
package body Wisi_Parse_Context is

   function Source_File_Name (Item : in Parse_Context_Access) return String
   is (Ada.Strings.Unbounded.To_String (Item.File_Name));

   package File_Parse_Context_Maps is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type => Parse_Context_Access,
      Key_Type     => String,
      Key          => Source_File_Name,
      Key_Compare  => SAL.String_Compare);

   Map : File_Parse_Context_Maps.Tree;

   function Find_Create
     (File_Name : in String;
      Language  : in Wisi_Parse_Context.Language;
      Trace     : in WisiToken.Trace_Access)
     return Parse_Context_Access
   is begin
      if File_Name'Length = 0 then
         raise Wisi.Protocol_Error with "no file name given";
      end if;

      declare
         use File_Parse_Context_Maps;
         use WisiToken;

         Found : constant Cursor := Map.Find (File_Name);
      begin
         if Has_Element (Found) then
            return Result : constant Parse_Context_Access := Element (Found) do
               if Language.Descriptor /= Result.Parser.Tree.Lexer.Descriptor then
                  raise WisiToken.User_Error with "language does not match for buffer '" & File_Name & "'";
               end if;
               if Trace_Incremental_Parse > Outline then
                  Trace.Put_Line ("parse_context found");
               end if;
            end return;
         end if;

         return Result : constant Parse_Context_Access :=
           (new Parse_Context'
              (File_Name                         => +File_Name,
               Text_Buffer                       => null,
               Text_Buffer_Byte_Last             => 0,
               Text_Buffer_Char_Last             => 0,
               Parser                            => WisiToken.Parse.LR.Parser.Parser'
                 (Ada.Finalization.Limited_Controlled with
                  Trace                          => Trace,
                  User_Data                      => Wisi.New_User_Data (Language.Parse_Data_Template.all),
                  Table                          => Language.Table,
                  Language_Fixes                 => Language.Fixes,
                  Language_Matching_Begin_Tokens => Language.Matching_Begin_Tokens,
                  Language_String_ID_Set         => Language.String_ID_Set,
                  Partial_Parse_Active           => Language.Partial_Parse_Active,
                  Partial_Parse_Byte_Goal        => Language.Partial_Parse_Byte_Goal,
                  others                         => <>),
               Root_Save_Edited_Name             => <>,
               Save_Edited_Count                 => <>))
         do
            Result.Parser.Tree.Lexer := Language.Lexer;
            Map.Insert (Result);
            if Trace_Incremental_Parse > Outline then
               Trace.Put_Line ("parse_context created");
            end if;
         end return;
      end;
   end Find_Create;

   function Find
     (File_Name : in String;
      Language  : in Wisi_Parse_Context.Language)
     return Parse_Context_Access
   is begin
      if File_Name'Length = 0 then
         raise Wisi.Protocol_Error with "no file name given";
      end if;

      declare
         use File_Parse_Context_Maps;
         use WisiToken;
         use all type WisiToken.Descriptor_Access_Constant;

         Found : constant Cursor := Map.Find (File_Name);
      begin
         if Has_Element (Found) then
            return Result : constant Parse_Context_Access := Element (Found) do
               if Language.Descriptor /= Result.Parser.Tree.Lexer.Descriptor then
                  raise WisiToken.User_Error with "language does not match for buffer '" & File_Name & "'";
               end if;
               if Trace_Incremental_Parse > Outline then
                  Result.Parser.Trace.Put_Line ("parse_context found");
               end if;
            end return;
         else
            if Trace_Incremental_Parse > Outline then
               Ada.Text_IO.Put_Line ("parse_context not found");
            end if;
            raise Not_Found;
         end if;
      end;
   end Find;

   procedure Clear
   is begin
      Map.Clear;
   end Clear;

   procedure Save_Text
     (Context       : in Parse_Context;
      File_Name     : in String;
      Emacs_Message : in Boolean)
   is
      use GNAT.OS_Lib;
      File : File_Descriptor;
      Written : Integer;
      pragma Unreferenced (Written);
   begin
      if Ada.Directories.Exists (File_Name) then
         Ada.Directories.Delete_File (File_Name);
      end if;
      File := Create_New_File (File_Name, Fmode => Binary);
      Written := Write (File, Context.Text_Buffer (Context.Text_Buffer'First)'Address,
             N => Context.Text_Buffer_Byte_Last - Context.Text_Buffer'First + 1);
      --  Written /= N on disk full; we don't check for that, because there's
      --  nothing to do.
      Close (File);

      if Emacs_Message then
         Ada.Text_IO.Put_Line ("(message ""text saved to '" & File_Name & "'"")");
      else
         Ada.Text_IO.Put_Line ("text saved to '" & File_Name & "'");
      end if;
   end Save_Text;

   procedure Save_Text_Auto
     (Context       : in out Parse_Context;
      Emacs_Message : in     Boolean)
   is begin
      Context.Save_Edited_Count := @ + 1;

      declare
         Save_File_Name : constant String :=
           Ada.Strings.Unbounded.To_String (Context.Root_Save_Edited_Name) & "_" &
           Wisi.Integer_Filled_Image (Item => Context.Save_Edited_Count, Width => 3);
      begin
         Save_Text (Context, Save_File_Name, Emacs_Message);
      end;
   end Save_Text_Auto;

end Wisi_Parse_Context;
