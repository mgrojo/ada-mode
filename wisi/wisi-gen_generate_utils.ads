--  Abstract :
--
--  Utilities for translating input file structures to WisiToken
--  structures needed for LALR.Generate.
--
--  Copyright (C) 2014, 2015, 2017 Stephen Leake All Rights Reserved.
--
--  The WisiToken package is free software; you can redistribute it
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

with Ada.Iterator_Interfaces;
with WisiToken.Parser.LR.Generator_Utils;
with WisiToken.Production;
generic
   Keywords              : in Wisi.String_Pair_Lists.List;
   Tokens                : in Wisi.Token_Lists.List;
   Conflicts             : in Wisi.Conflict_Lists.List;
   Rules                 : in Wisi.Rule_Lists.List;
   EOI_Name              : in Standard.Ada.Strings.Unbounded.Unbounded_String; -- without trailing _ID
   WisiToken_Accept_Name : in Standard.Ada.Strings.Unbounded.Unbounded_String;

   with function To_Token_Out_Image (Item : in Standard.Ada.Strings.Unbounded.Unbounded_String) return String;
   --  Name of token in output file
package Wisi.Gen_Generate_Utils is
   use WisiToken;

   function Count_Non_Reporting return Integer;

   EOF_ID : constant Token_ID := Token_ID (Count (Tokens)) + Token_ID (Keywords.Length) + 1;

   LR1_Descriptor : WisiToken.Descriptor
     (First_Terminal    => (if Count_Non_Reporting > 0
                            then Token_ID (Count_Non_Reporting) + Token_ID'First
                            else Token_ID'First),
      Last_Terminal     => EOF_ID,
      EOF_ID            => EOF_ID,
      Accept_ID         => EOF_ID + 1,
      First_Nonterminal => EOF_ID + 1,
      Last_Nonterminal  => EOF_ID + Token_ID (Rules.Length) + 1);
   --  Image, Image_Width set by Set_Token_Images

   LALR_Descriptor : WisiToken.LALR_Descriptor
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
   --  FIXME: duplicates Descriptor.image

   function Token_WY_Image (ID : in Token_ID) return String is (LR1_Descriptor.Image (ID).all);
   function Token_Out_Image (ID : in Token_ID) return String is (Token_Output_Image (ID).all);

   First_Rule_Line : constant Standard.Ada.Text_IO.Positive_Count := Rules.First_Element.Source_Line;

   function Find_Token_ID (Token : in String) return Token_ID;
   --  FIXME: use ID (Token_Cursor) instead.

   type Token_Container is tagged record
      Bogus_Content : Integer; -- need a component to declare an object
   end record
   with
     Constant_Indexing => Constant_Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Standard.Ada.Strings.Unbounded.Unbounded_String;
   --  We need a container type to define an iterator; the actual data is
   --  in generic parameters Keywords, Tokens, and Rules. The
   --  Iterator_Element is given by Token_Name below.

   type Token_Constant_Reference_Type
     (Element : not null access constant Standard.Ada.Strings.Unbounded.Unbounded_String)
     is null record
   with Implicit_Dereference => Element;

   type Token_Cursor is private;
   --  Iterate thru Keywords, Tokens, Rules in a canonical order:
   --
   --  1. Non-reporting tokens from Tokens
   --  2. Keywords
   --  3. Other kinds from Tokens
   --  4. EOI
   --  5. Accept
   --  6. Nonterminals
   --
   --  Within each group, tokens occur in the order they were declared in
   --  the grammar file.

   function Constant_Reference
     (Container : aliased in Token_Container'Class;
      Cursor    :         in Token_Cursor)
     return Token_Constant_Reference_Type;

   function Is_Done (Cursor : in Token_Cursor) return Boolean;
   function Has_Element (Cursor : in Token_Cursor) return Boolean is (not Is_Done (Cursor));
   package Iterator_Interfaces is new Standard.Ada.Iterator_Interfaces (Token_Cursor, Has_Element);
   function Iterate
     (Container     : aliased    Token_Container;
      Non_Reporting :         in Boolean := True;
      Other_Tokens  :         in Boolean := True)
     return Iterator_Interfaces.Forward_Iterator'Class;

   function First (Non_Reporting : in Boolean) return Token_Cursor;
   procedure Next (Cursor : in out Token_Cursor; Other_Tokens : in Boolean);

   function ID (Cursor : in Token_Cursor) return Token_ID;

   function Name (Cursor : in Token_Cursor) return String;
   --  Return the token name from the .wy file:
   --  Keywords: Keywords (i).name
   --  Tokens  : Tokens (i).Tokens (j).name
   --  Rules   : Rules (i).Left_Hand_Side

   function Kind (Cursor : in Token_Cursor) return String;
   --  Return the token kind from the .wy file:
   --  Keywords: "keyword"
   --  Tokens  : Tokens (i).Kind
   --  Rules   : "nonterminal"

   function Value (Cursor : in Token_Cursor) return String;
   --  Return the token value from the .wy file:
   --  Keywords: Keywords (i).value
   --  Tokens  : Tokens (i).Tokens (j).Value
   --  Rules   : "" - they have no Value

   All_Tokens : constant Token_Container := (Bogus_Content => 1);

   procedure Put_Tokens;
   --  Put user readable token list to Standard_Output

   function To_Conflicts
     (Accept_Reduce_Conflict_Count : out Integer;
      Shift_Reduce_Conflict_Count  : out Integer;
      Reduce_Reduce_Conflict_Count : out Integer)
     return WisiToken.Parser.LR.Generator_Utils.Conflict_Lists.List;

   function To_Grammar
     (Descriptor       : in WisiToken.Descriptor'Class;
      Source_File_Name : in String;
      Start_Token      : in String)
     return Production.List.Instance;
   --  Descriptor, Source_File_Name used in error messages.

   function To_Nonterminal_ID_Set (Item : in String_Lists.List) return Token_ID_Set;

   function To_McKenzie_Param (Item : in McKenzie_Recover_Param_Type) return WisiToken.Parser.LR.McKenzie_Param_Type;

private

   type Token_Cursor_State is
     (Non_Reporting, Terminals_Keywords, Terminals_Others, EOI, WisiToken_Accept, Nonterminal, Done);

   type Token_Cursor is record
      State       : Token_Cursor_State; --  FIXME: rename to Kind. use separate list for Non_Reporting
      ID          : Token_ID;
      Token_Kind  : Wisi.Token_Lists.Cursor;
      Token_Item  : String_Pair_Lists.Cursor;
      Keyword     : String_Pair_Lists.Cursor;
      Nonterminal : Rule_Lists.Cursor;
   end record;

end Wisi.Gen_Generate_Utils;
