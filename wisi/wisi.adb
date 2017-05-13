--  Abstract :
--
--  see spec
--
--  Copyright (C) 2012 - 2015, 2017 Stephen Leake.  All Rights Reserved.
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
package body Wisi is

   function Count (Tokens : in Token_Lists.List) return Integer
   is
      Result : Integer := 0;
   begin
      for Kind of Tokens loop
         Result := Result + Integer (Kind.Tokens.Length);
      end loop;
      return Result;
   end Count;

   procedure Add_Token
     (Tokens : in out Token_Lists.List;
      Kind   : in     String;
      Name   : in     String;
      Value  : in     String)
   is
      use type Standard.Ada.Strings.Unbounded.Unbounded_String;
   begin
      for Token_Kind of Tokens loop
         if Token_Kind.Kind = Kind then
            Token_Kind.Tokens.Append ((+Name, +Value));
            return;
         end if;
      end loop;

      --  Kind not found; add it
      declare
         Temp : String_Pair_Lists.List;
      begin
         Temp.Append ((+Name, +Value));
         Tokens.Append ((+Kind, Temp));
      end;
   end Add_Token;

   function Is_In (Tokens : in Token_Lists.List; Kind : in String) return Boolean
   is begin
      for Token of Tokens loop
         if -Token.Kind = Kind then
            return True;
         end if;
      end loop;
      return False;
   end Is_In;

   function Is_In
     (Tokens : in Token_Lists.List;
      Kind   : in String;
      Value  : in String)
     return Boolean
   is begin
      for Token of Tokens loop
         if -Token.Kind = Kind then
            for Item of Token.Tokens loop
               if -Item.Value = Value then
                  return True;
               end if;
            end loop;
         end if;
      end loop;
      return False;
   end Is_In;

   function Strip_Quotes (Item : in String) return String
   is begin
      if Item'Length < 2 then
         return Item;
      else
         return Item
           ((if Item (Item'First) = '"' then Item'First + 1 else Item'First) ..
              (if Item (Item'Last) = '"' then Item'Last - 1 else Item'Last));
      end if;
   end Strip_Quotes;

   function "+" (List : in String_Lists.List; Item : in String) return String_Lists.List
   is
      Result : String_Lists.List := List;
   begin
      Result.Append (Item);
      return Result;
   end "+";

   function String_To_String_List (Item : in String) return String_Lists.List
   is
      Result : String_Lists.List;
   begin
      Result.Append (Item);
      return Result;
   end String_To_String_List;

   function RHS_To_RHS_List (Item : in RHS_Type) return RHS_Lists.List
   is
      Result : RHS_Lists.List;
   begin
      Result.Append (Item);
      return Result;
   end RHS_To_RHS_List;

   function "+" (List : in RHS_Lists.List; Item : in RHS_Type) return RHS_Lists.List
   is
      Result : RHS_Lists.List := List;
   begin
      Result.Append (Item);
      return Result;
   end "+";

   procedure Put_Command_Line (Comment_Prefix : in String)
   is
      use Ada.Command_Line;
      Max_Line_Length : constant := 120;
      Col : Integer := 0;

      procedure Put (Item : in String; Leading_Space : in Boolean)
      is begin
         if Col > 0 and Col + Item'Length + 1 > Max_Line_Length then
            Ada.Text_IO.New_Line;
            Col := Comment_Prefix'Length;
            Ada.Text_IO.Put (Comment_Prefix);
         else
            if Leading_Space then
               Ada.Text_IO.Put (" ");
               Col := Col + 1;
            end if;
         end if;

         Col := Col + Item'Length;
         Ada.Text_IO.Put (Item);
      end Put;
   begin
      Put (Comment_Prefix & "with command line:", False);
      Put (Ada.Directories.Simple_Name (Command_Name), True);
      for I in 1 .. Argument_Count loop
         Put (Argument (I), True);
      end loop;
      Ada.Text_IO.New_Line;
   end Put_Command_Line;

end Wisi;
