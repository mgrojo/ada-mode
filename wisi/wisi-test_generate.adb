--  Abstract :
--
--  Run LALR_Generators.Generate, for testing. Exceptions raised by
--  Generate are propagated.
--
--  Copyright (C) 2013, 2014 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;
with Wisi.Gen_Generate_Utils;
procedure Wisi.Test_Generate
  (Input_File_Name : in String;
   Keywords        : in String_Pair_Lists.List;
   Tokens          : in Token_Lists.List;
   Start_Token     : in Standard.Ada.Strings.Unbounded.Unbounded_String;
   Conflicts       : in Conflict_Lists.List;
   Rules           : in Rule_Lists.List)
is
   EOI_Image              : constant String := "$EOI";
   OpenToken_Accept_Image : constant String := "opentoken_accept";

   function To_Token_Image (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
   is begin
      return -Item;
   end To_Token_Image;

   package Generate_Utils is new Wisi.Gen_Generate_Utils
     (Keywords, Tokens, Conflicts, Rules, EOI_Image, OpenToken_Accept_Image,
      First_State_Index => 0, -- Match elisp array indexing
      To_Token_Image    => To_Token_Image);

   Shift_Reduce_Conflict_Count  : Integer;
   Reduce_Reduce_Conflict_Count : Integer;

   Grammar : constant Generate_Utils.Production_Lists.Instance := Generate_Utils.To_Grammar
     (Input_File_Name, -Start_Token);

   Parser : constant Generate_Utils.LALRs.Parse_Table_Ptr := Generate_Utils.LALR_Generators.Generate
     (Grammar,
      Generate_Utils.To_Conflicts (Shift_Reduce_Conflict_Count, Reduce_Reduce_Conflict_Count),
      Trace                    => Verbosity > 1,
      Put_Parse_Table          => Verbosity > 0,
      Ignore_Unused_Tokens     => Verbosity > 1,
      Ignore_Unknown_Conflicts => Verbosity > 1);

   pragma Unreferenced (Parser);

begin
   if Verbosity > 0 then
      declare
         use Standard.Ada.Text_IO;
         use Generate_Utils;
      begin
         Put_Line ("Tokens:");
         for I in Token_IDs'Range loop
            Put_Line (Token_IDs'Image (I) & " => " & Token_Image (I));
         end loop;
         New_Line;

         Put_Line ("Grammar:");
         Print_Production_Lists.Print (Grammar);
         New_Line;
      end;
   end if;

end Wisi.Test_Generate;
