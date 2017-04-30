--  Abstract :
--
--  Run LALR_Generators.Generate, for testing. Exceptions raised by
--  Generate are propagated.
--
--  Copyright (C) 2013-2015, 2017 Stephen Leake.  All Rights Reserved.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Text_IO;
with Wisi.Gen_Generate_Utils;
procedure Wisi.Test_Generate
  (Input_File_Name   : in String;
   Keywords          : in String_Pair_Lists.List;
   Tokens            : in Token_Lists.List;
   Start_Token       : in Standard.Ada.Strings.Unbounded.Unbounded_String;
   Conflicts         : in Conflict_Lists.List;
   Rules             : in Rule_Lists.List;
   First_State_Index : in Integer)
is
   EOI_Name              : constant Ada.Strings.Unbounded.Unbounded_String := +"$EOI";
   OpenToken_Accept_Name : constant Ada.Strings.Unbounded.Unbounded_String := +"opentoken_accept";

   function To_Token_Image (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
   is begin
      return -Item;
   end To_Token_Image;

   package Generate_Utils is new Wisi.Gen_Generate_Utils
     (Keywords, Tokens, Conflicts, Rules, EOI_Name, OpenToken_Accept_Name,
      First_State_Index,
      To_Token_Image => To_Token_Image);

   Accept_Reduce_Conflict_Count : Integer;
   Shift_Reduce_Conflict_Count  : Integer;
   Reduce_Reduce_Conflict_Count : Integer;

   Grammar : constant Generate_Utils.Production.List.Instance := Generate_Utils.To_Grammar
     (Input_File_Name, -Start_Token);

   Parser : constant Generate_Utils.LR.Parse_Table_Ptr := Generate_Utils.LALR_Generator.Generate
     (Grammar,
      Generate_Utils.To_Conflicts
        (Accept_Reduce_Conflict_Count, Shift_Reduce_Conflict_Count, Reduce_Reduce_Conflict_Count),
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
         for I in Token_ID'Range loop
            Put_Line (Token_ID'Image (I) & " => " & Token_Image (I));
         end loop;
         New_Line;

         Put_Line ("Grammar:");
         Put_Trace_Production.Put_Trace (Grammar);
         New_Line;
      end;
   end if;

end Wisi.Test_Generate;
