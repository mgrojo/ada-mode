--  Abstract:
--
--  Root of WisiToken lexer/parser generator and exector.
--
--  The token type is an integer subtype, to avoid making this package
--  generic, which would make all other packages generic, and
--  eventually GNAT 2016 chokes.
--
--  To store additional information about a token, see
--  fasttoken-token_regions.ads or fasttoken-token_wisi.ads.
--
--  Copyright (C) 2009, 2010, 2013 - 2015, 2017 Stephe Leake
--  Copyright (C) 1999 FlightSafety International and Ted Dennison
--
--  This file is part of the WisiToken package.
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
--
--  This software was originally developed by the following company,
--  and was released as open-source software as a service to the
--  community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000

pragma License (Modified_GPL);

with Ada.Characters.Latin_1;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with SAL.Gen_Unbounded_Definite_Queues;
with SAL.Gen_Unbounded_Indefinite_Queues;
package WisiToken is

   Syntax_Error : exception; -- no token matching current input could be found.

   Parse_Error : exception; -- Input does not conform to the grammar

   Grammar_Error : exception; -- Grammar is not consistent (ie unused tokens, missing productions)

   User_Error : exception; -- other user error (ie command line parameter)

   Programmer_Error : exception; -- a programming convention has been violated

   --  We use this regardless of OS, since we need a standard way of
   --  representing an end of line in a string buffer. We use
   --  LF to match WisiToken.Lexer.Aflex; Aflex hard-codes LF.
   EOL_Character : constant Character := Ada.Characters.Latin_1.LF;

   --  Similarly, this is independent of OS
   EOF_Character : constant Character := Ada.Characters.Latin_1.EOT;

   ----------
   --  Tokens

   type Token_ID is range 0 .. Integer'Last; -- 0 origin to match elisp array

   subtype Positive_Index_Type is Ada.Containers.Count_Type range 1 .. Ada.Containers.Count_Type'Last;
   package Token_Arrays is new Ada.Containers.Vectors (Positive_Index_Type, Token_ID);
   subtype Token_Array is Token_Arrays.Vector;
   Empty_Token_Array : Token_Array renames Token_Arrays.Empty_Vector;

   Invalid_Token_ID : constant Token_ID := Token_ID'Last;

   package Token_Queues is new SAL.Gen_Unbounded_Definite_Queues (Token_ID);

   type Token_Array_String is array (Token_ID range <>) of access constant String;
   type Token_Array_Float is array (Token_ID range <>) of Float;
   type Token_Array_Natural is array (Token_ID range <>) of Natural;

   type Descriptor
     (First_Terminal    : Token_ID;
      Last_Terminal     : Token_ID;
      First_Nonterminal : Token_ID;
      Last_Nonterminal  : Token_ID;
      EOF_ID            : Token_ID;
      Accept_ID         : Token_ID)
   is tagged record
      --  Tokens in the range Token_ID'First .. First_Terminal - 1 are
      --  non-reporting (comments, whitespace), and thus are not used in
      --  generating parse tables.
      --
      --  Tokens in the range Last_Terminal + 1 .. Last_Nonterminal are
      --  the nonterminals of a grammar.
      --
      --  Components are discriminants if they can be specified statically.

      New_Line_ID : Token_ID;
      Comment_ID  : Token_ID;
      --  If the tokens do not include a reporting New_Line or Comment
      --  token, set New_Line_ID or Comment_ID to Invalid_Token_ID.

      Image : Token_Array_String (Token_ID'First .. Last_Nonterminal);
      --  User names for tokens.

      Terminal_Image_Width : Integer;
      Image_Width          : Integer; --  max width of Image
   end record;

   function Image (Desc : in Descriptor'Class; Item : in Token_ID; Pad : in Boolean := False) return String;
   --  Return Desc.Image (Item), possibly padded to
   --  Terminal_Image_Width (if Item is a terminal) or to Image_Width.

   function Int_Image (Item : in Token_ID) return String;
   --  Return Token_ID'Image, leading space stripped.

   type Token_ID_Set is array (Token_ID range <>) of Boolean;

   function Any (Item : in Token_ID_Set) return Boolean;

   function Count (Item : in Token_ID_Set) return Integer;
   --  Count of True elements.

   function Image
     (Desc      : in Descriptor'Class;
      Item      : in Token_ID_Set;
      Max_Count : in Integer := Integer'Last)
     return String;

   type Token_Array_Token_Set is array (Token_ID range <>, Token_ID range <>) of Boolean;

   function Slice (Item : in Token_Array_Token_Set; I : in Token_ID) return Token_ID_Set;
   function Any (Item : in Token_Array_Token_Set; I : in Token_ID) return Boolean;
   function Any (Item : in Token_Array_Token_Set) return Boolean;
   procedure Or_Slice (Item : in out Token_Array_Token_Set; I : in Token_ID; Value : in Token_ID_Set);

   function Image (Descriptor : in WisiToken.Descriptor'Class; Item : in Token_Array) return String;

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in Token_Array);
   procedure Put (Descriptor : in WisiToken.Descriptor; Item : in Token_ID_Set);
   procedure Put (Descriptor : in WisiToken.Descriptor; Item : in Token_Array_Token_Set);
   --  Put Item to Ada.Text_IO.Current_Output, using valid Ada aggregate syntax

   function To_Lookahead (Descriptor : in WisiToken.Descriptor; Item : in Token_ID) return Token_ID_Set;
   --  Base implementation returns (Descriptor.First_Terminal ..
   --  Descriptor.Last_Terminal), with Item = True, others False. LALR
   --  child type adds Propagate_ID.

   function Lookahead_Image (Descriptor : in WisiToken.Descriptor; Item : in Token_ID_Set) return String;
   --  Base implementation just returns aggregate syntax for Item.
   --  LALR child includes '#' for Propagate_ID.

   type LALR_Descriptor
     (First_Terminal    : Token_ID;
      Last_Terminal     : Token_ID;
      First_Nonterminal : Token_ID;
      Last_Nonterminal  : Token_ID;
      EOF_ID            : Token_ID;
      Accept_ID         : Token_ID;
      Propagate_ID      : Token_ID)
     is new Descriptor
       (First_Terminal    => First_Terminal,
        Last_Terminal     => Last_Terminal,
        First_Nonterminal => First_Nonterminal,
        Last_Nonterminal  => Last_Nonterminal,
        EOF_ID            => EOF_ID,
        Accept_ID         => Accept_ID)
     with null record;

   overriding
   function To_Lookahead (Descriptor : in LALR_Descriptor; Item : in Token_ID) return Token_ID_Set;

   overriding
   function Lookahead_Image (Descriptor : in LALR_Descriptor; Item : in Token_ID_Set) return String;

   ----------
   --  Trace

   Trace_Parse : Integer := 0;
   --  If Trace_Parse > 0, Parse prints messages helpful for debugging
   --  the grammar and/or the parser; higher value prints more.
   --
   --  1: shows each parser cycle, spawn/terminate parallel parsers, error recovery enter/exit
   --  2: add parse stack in each cycle, error recovery enqueue/check
   --  3: add pending semantic state operations, error recovery parse actions
   --  4: add lexer debug

   type Trace (Descriptor : not null access constant WisiToken.Descriptor'Class) is abstract tagged limited null record;
   --  Derived types should support Ada.Text_IO for tests/debugging,
   --  and a protocol for inter-process communication for running a
   --  parser as a subprocess of an IDE.

   procedure Put (Trace : in out WisiToken.Trace; Item : in String) is abstract;
   --  Put Item to the Trace object

   procedure Put_Line (Trace : in out WisiToken.Trace; Item : in String) is abstract;
   --  Accumulate Item in the trace buffer, output the trace buffer to
   --  the display.

   procedure New_Line (Trace : in out WisiToken.Trace) is abstract;
   --  Output the trace buffer to the display.

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Token_ID);
   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Token_Array);
   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Token_Queues.Queue_Type);
   --  Accumulate Item in the trace buffer.

   ----------
   --  Augmented tokens, semantic actions

   type Augmented_Token is abstract tagged record
      ID      : Token_ID := Invalid_Token_ID;
      Virtual : Boolean;
      --  Derived types add various lexical information.
   end record;

   function Image
     (Item       : in Augmented_Token;
      Descriptor : in WisiToken.Descriptor'Class;
      ID_Only    : in Boolean)
     return String is abstract;
   --  Return a string for debug/test messages

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Augmented_Token'Class);
   --  Put Image to Trace.

   package Augmented_Token_Arrays is new Ada.Containers.Indefinite_Vectors (Positive_Index_Type, Augmented_Token'Class);

   subtype Augmented_Token_Array is Augmented_Token_Arrays.Vector;

   package Augmented_Token_Queues is new SAL.Gen_Unbounded_Indefinite_Queues (Augmented_Token'Class);

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Augmented_Token_Queues.Queue_Type);

   type Semantic_Action is access procedure
     (Nonterm : in Augmented_Token'Class;
      Index   : in Natural;
      Tokens  : in Augmented_Token_Array);
   --  Routines of this type are called by the parser when it reduces
   --  a production to Nonterm. Index indicates which production for
   --  Nonterm (0 origin); Tokens is the right hand side tokens.
   --
   --  Nonterm is classwide to avoid freezing rules.

   Null_Action : constant Semantic_Action := null;

   ----------
   --  Misc

   function "+" (Item : in String) return Standard.Ada.Strings.Unbounded.Unbounded_String
     renames Standard.Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (Item : in Standard.Ada.Strings.Unbounded.Unbounded_String) return String
     renames Standard.Ada.Strings.Unbounded.To_String;

   function Int_Image (Item : in Integer) return String;
   --  No leading space

   type Line_Number_Type is range 1 .. Natural'Last; -- Match Emacs buffer line numbers.

   Invalid_Line_Number : constant Line_Number_Type := Line_Number_Type'Last;

   function Error_Message
     (File_Name : in String;
      Line      : in Line_Number_Type;
      Col       : in Ada.Text_IO.Count;
      Message   : in String)
     return String;
   --  Return Gnu-formatted error message.

   procedure Put_Error (Message : in String);
   --  Put Message to Standard_Error.

   type Parser_Algorithm_Type is (LALR, LR1);

   type Buffer_Pos is range 1 .. Integer'Last; -- match Emacs buffer origin.
   type Buffer_Region is record
      First : Buffer_Pos;
      Last  : Buffer_Pos;
   end record;

   Null_Buffer_Region : constant Buffer_Region := (Buffer_Pos'Last, Buffer_Pos'First);

   function Length (Region : in Buffer_Region) return Natural is (Natural (Region.Last - Region.First + 1));

   function Inside (Pos : in Buffer_Pos; Region : in Buffer_Region) return Boolean
     is (Region.First <= Pos and Pos <= Region.Last);

   function Image (Item : in Buffer_Region) return String;

   function "and" (Left, Right : in Buffer_Region) return Buffer_Region;
   --  Return region enclosing both Left and Right.

   package Region_Lists is new Ada.Containers.Doubly_Linked_Lists (Buffer_Region);

end WisiToken;
