--  Abstract :
--
--  Utilities for translating input file structures to FastToken
--  structures needed for LALR.Generate.
--
--  Copyright (C) 2014, 2015, 2017  All Rights Reserved.
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

with FastToken.Parser.LR.Generator_Utils;
with FastToken.Production;
generic
   Keywords              : in Wisi.String_Pair_Lists.List;
   Tokens                : in Wisi.Token_Lists.List;
   Conflicts             : in Wisi.Conflict_Lists.List;
   Rules                 : in Wisi.Rule_Lists.List;
   EOI_Name              : in Standard.Ada.Strings.Unbounded.Unbounded_String; -- without trailing _ID
   FastToken_Accept_Name : in Standard.Ada.Strings.Unbounded.Unbounded_String;

   with function To_Token_Out_Image (Item : in Standard.Ada.Strings.Unbounded.Unbounded_String) return String;
   --  Name of token in output file
package Wisi.Gen_Generate_Utils is
   use FastToken;

   function Count_Non_Reporting return Integer;

   EOF_ID : constant Token_ID := Token_ID (Count (Tokens)) + Token_ID (Keywords.Length) + 1;

   LR1_Descriptor : FastToken.Descriptor
     (First_Terminal    => (if Count_Non_Reporting > 0
                            then Token_ID (Count_Non_Reporting) + Token_ID'First
                            else Token_ID'First),
      Last_Terminal     => EOF_ID,
      EOF_ID            => EOF_ID,
      Accept_ID         => EOF_ID + 1,
      First_Nonterminal => EOF_ID + 1,
      Last_Nonterminal  => EOF_ID + Token_ID (Rules.Length) + 1);
   --  Image, Image_Width set by Set_Token_Images

   LALR_Descriptor : FastToken.LALR_Descriptor
     (First_Terminal    => LR1_Descriptor.First_Terminal,
      Last_Terminal     => LR1_Descriptor.Last_Terminal,
      First_Nonterminal => EOF_ID + 1,
      Last_Nonterminal  => LR1_Descriptor.Last_Nonterminal,
      EOF_ID            => LR1_Descriptor.EOF_ID,
      Accept_ID         => EOF_ID + 1,
      Propagate_ID      => EOF_ID + 1);
   --  Image, Image_Width set by Set_Token_Images

   subtype Nonterminal_ID is Token_ID range LR1_Descriptor.Last_Terminal + 1 .. LR1_Descriptor.Last_Nonterminal;

   Token_Output_Image_Width : Integer := 0; -- set by Set_Token_Images

   function Set_Token_Images return Token_Array_String;

   Token_Output_Image : constant Token_Array_String := Set_Token_Images;

   function Token_WY_Image (ID : in Token_ID) return String is (LR1_Descriptor.Image (ID).all);
   function Token_Out_Image (ID : in Token_ID) return String is (Token_Output_Image (ID).all);

   First_Rule_Line : constant Standard.Ada.Text_IO.Positive_Count := Rules.First_Element.Source_Line;

   function Find_Token_ID (Token : in String) return Token_ID;

   type Token_Cursor is tagged private;
   --  Iterate thru Tokens in a canonical order.

   function First return Token_Cursor;
   procedure Next (Cursor : in out Token_Cursor);
   function Is_Done (Cursor : in out Token_Cursor) return Boolean;
   function Token_Name (Cursor : in out Token_Cursor) return Standard.Ada.Strings.Unbounded.Unbounded_String;
   --  Return the token name from the .wy file:
   --  token: Tokens.name
   --  keyword: Keywords.name
   --  nonterminal: Rules.Left_Hand_Side

   procedure Put_Tokens;
   --  Put user readable token list to Standard_Output

   function To_Conflicts
     (Accept_Reduce_Conflict_Count : out Integer;
      Shift_Reduce_Conflict_Count  : out Integer;
      Reduce_Reduce_Conflict_Count : out Integer)
     return FastToken.Parser.LR.Generator_Utils.Conflict_Lists.List;

   function To_Grammar
     (Descriptor       : in FastToken.Descriptor;
      Source_File_Name : in String;
      Start_Token      : in String)
     return Production.List.Instance;
   --  Descriptor, Source_File_Name used in error messages.

   function To_Nonterminal_ID_Set (Item : in String_Lists.List) return Token_ID_Set;

private

   type Token_Cursor_State is
     (Non_Reporting, Terminals_Keywords, Terminals_Others, EOI, FastToken_Accept, Nonterminal, Done);

   type Token_Cursor is tagged record
      State       : Token_Cursor_State;
      Token_Kind  : Wisi.Token_Lists.Cursor;
      Token_Item  : String_Pair_Lists.Cursor;
      Keyword     : String_Pair_Lists.Cursor;
      Nonterminal : Rule_Lists.Cursor;
   end record;

end Wisi.Gen_Generate_Utils;
