--  Abstract :
--
--  Utilities for translating input file structures to WisiToken
--  structures needed for LALR.Generate.
--
--  Copyright (C) 2014, 2015, 2017, 2018 Stephen Leake All Rights Reserved.
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
with WisiToken.LR.Generate_Utils;
with WisiToken.Productions;
with WisiToken.Wisi_Grammar_Runtime;
package Wisi.Generate_Utils is
   use WisiToken;

   EOI_Name : constant String := "Wisi_EOI";
   --  EOI_Name is used for EOF_ID token; it must match Emacs ada-mode
   --  wisi.el wisi-eoi-term. It must be a valid Ada identifier when
   --  "_ID" is appended.

   WisiToken_Accept_Name : constant String := "wisitoken_accept";

   type LR_Parser_Array is array (Generate_Algorithm range LALR .. LR1) of WisiToken.LR.Parse_Table_Ptr;

   type Generate_Data is limited record
      Tokens          : access constant Wisi.Tokens;
      LR1_Descriptor  : access WisiToken.Descriptor;
      LALR_Descriptor : access WisiToken.LALR_Descriptor;
      Grammar         : WisiToken.Productions.Prod_Arrays.Vector;
      Start_ID        : WisiToken.Token_ID;
      Source_Line_Map : WisiToken.Productions.Source_Line_Maps.Vector;
      Conflicts       : WisiToken.LR.Generate_Utils.Conflict_Lists.List;

      LR_Parsers : LR_Parser_Array;

      --  LR parse table stats
      Table_Actions_Count          : Integer                       := -1; -- parse, not user, actions
      Parser_State_Count           : WisiToken.Unknown_State_Index := 0;
      Accept_Reduce_Conflict_Count : Integer                       := 0;
      Shift_Reduce_Conflict_Count  : Integer                       := 0;
      Reduce_Reduce_Conflict_Count : Integer                       := 0;

   end record;

   function Initialize
     (Source_File_Name :         in String;
      Tokens           : aliased in Wisi.Tokens;
      Start_Token      :         in String)
     return Generate_Data;

   function Find_Token_ID (Data : aliased in Generate_Data; Token : in String) return Token_ID;

   type Token_Container (Data : not null access constant Generate_Data) is tagged null record
   with
     Constant_Indexing => Constant_Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Standard.Ada.Strings.Unbounded.Unbounded_String;
   --  We need a container type to define an iterator; the actual data is
   --  in generic parameters Keywords, Non_Grammar, Tokens, and Rules. The
   --  Iterator_Element is given by Token_Name below.

   function All_Tokens (Data : aliased in Generate_Data) return Token_Container;

   type Token_Constant_Reference_Type
     (Element : not null access constant Standard.Ada.Strings.Unbounded.Unbounded_String)
     is null record
   with Implicit_Dereference => Element;

   type Token_Cursor is private;
   --  Iterate thru Keywords, Tokens, Rules in a canonical order:
   --
   --  1. Non_Grammar
   --  2. Keywords
   --  3. other terminal tokens, in declaration order
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
     (Container    : aliased    Token_Container;
      Non_Grammar  :         in Boolean := True;
      Nonterminals :         in Boolean := True)
     return Iterator_Interfaces.Forward_Iterator'Class;

   function First
     (Data         : aliased in Generate_Data;
      Non_Grammar  :         in Boolean;
      Nonterminals :         in Boolean)
     return Token_Cursor;
   procedure Next (Cursor : in out Token_Cursor; Nonterminals : in Boolean);

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

   function To_Conflicts
     (Data             : aliased in out Generate_Data;
      Conflicts        :         in     Wisi.Conflict_Lists.List;
      Source_File_Name :         in     String)
     return WisiToken.LR.Generate_Utils.Conflict_Lists.List;
   --  Not included in Initialize because algorithms have no conflicts.

   function To_Nonterminal_ID_Set
     (Data : aliased in Generate_Data;
      Item :         in String_Lists.List)
     return Token_ID_Set;

   function To_McKenzie_Param
     (Data : aliased in Generate_Data;
      Item :         in McKenzie_Recover_Param_Type)
     return WisiToken.LR.McKenzie_Param_Type;

   procedure Count_Actions
     (Data : in out Generate_Utils.Generate_Data;
      Alg  : in     LR_Generate_Algorithm);

   procedure Put_Stats
     (Input_Data    : in WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Generate_Data : in Generate_Utils.Generate_Data);

private

   type Token_Cursor_Kind is
     (Non_Grammar_Kind, Terminals_Keywords, Terminals_Others, EOI, WisiToken_Accept, Nonterminal, Done);

   type Token_Cursor is record
      Data        : not null access constant Generate_Data;
      Kind        : Token_Cursor_Kind;
      ID          : Token_ID;
      Token_Kind  : Wisi.Token_Lists.Cursor; -- Non_Grammar or Tokens, depending on Kind
      Token_Item  : String_Pair_Lists.Cursor;
      Keyword     : String_Pair_Lists.Cursor;
      Nonterminal : Rule_Lists.Cursor;
   end record;

end Wisi.Generate_Utils;
