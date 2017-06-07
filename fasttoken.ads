--  Abstract:
--
--  Root of FastToken lexer/parser generator and exector.
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
--  This file is part of the FastToken package.
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
package FastToken is

   Syntax_Error : exception; -- no token matching current input could be found.

   Parse_Error : exception; -- Input does not conform to the grammar

   Grammar_Error : exception; -- Grammar is not consistent (ie unused tokens, missing productions)

   User_Error : exception; -- other user error (ie command line parameter)

   Programmer_Error : exception; -- a programming convention has been violated

   --  We use this regardless of OS, since we need a standard way of
   --  representing an end of line in a string buffer. We use
   --  LF to match FastToken.Lexer.Aflex; Aflex hard-codes LF.
   EOL_Character : constant Character := Ada.Characters.Latin_1.LF;

   --  Similarly, this is independent of OS
   EOF_Character : constant Character := Ada.Characters.Latin_1.EOT;

   ----------
   --  Tokens

   type Token_ID is range 1 .. Integer'Last;

   type Token_Array_String is array (Token_ID range <>) of access constant String;

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

      Image : Token_Array_String (1 .. Last_Nonterminal);
      --  User names for tokens.

      Image_Width : Integer; --  max width of Image
   end record;

   function Image (Desc : in Descriptor'Class; Item : in Token_ID) return String;
   --  Return Desc.Image (Item)

   function Int_Image (Item : in Token_ID) return String;
   --  Return Token_ID'Image, leading space stripped.

   type Token_ID_Set is array (Token_ID range <>) of Boolean;

   function Any (Item : in Token_ID_Set) return Boolean;

   function Image (Desc : in Descriptor'Class; Item : in Token_ID_Set) return String;

   type Token_Array_Token_Set is array (Token_ID range <>, Token_ID range <>) of Boolean;

   function Slice (Item : in Token_Array_Token_Set; I : in Token_ID) return Token_ID_Set;
   function Any (Item : in Token_Array_Token_Set; I : in Token_ID) return Boolean;
   function Any (Item : in Token_Array_Token_Set) return Boolean;
   procedure Or_Slice (Item : in out Token_Array_Token_Set; I : in Token_ID; Value : in Token_ID_Set);

   procedure Put (Descriptor : in FastToken.Descriptor; Item : in Token_ID_Set);
   procedure Put (Descriptor : in FastToken.Descriptor; Item : in Token_Array_Token_Set);
   --  Put Item to Ada.Text_IO.Current_Output, using valid Ada aggregate syntax

   function To_Lookahead (Descriptor : in FastToken.Descriptor; Item : in Token_ID) return Token_ID_Set;
   --  Base implementation returns (Descriptor.First_Terminal ..
   --  Descriptor.Last_Terminal), with Item = True, others False. LALR
   --  child type adds Propagate_ID.

   function Lookahead_Image (Descriptor : in FastToken.Descriptor; Item : in Token_ID_Set) return String;
   --  Base implementation just returns aggregate syntas for Item.
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
   --  Augmented tokens

   type Augmented_Token is tagged record
      ID : Token_ID;
   end record;
   --  Derived types add various lexical information.

   subtype Stack_Index_Type is Ada.Containers.Count_Type range 1 .. Ada.Containers.Count_Type'Last;
   package Token_Stacks is new Ada.Containers.Indefinite_Vectors (Stack_Index_Type, Augmented_Token'Class);

   subtype Token_Stack_Type is Token_Stacks.Vector;

   type Semantic_Action is access procedure
     (Nonterm : in Augmented_Token'Class;
      Index   : in Natural;
      Source  : in Token_Stack_Type);
   --  Routines of this type are called by the parser when it reduces
   --  a production to Nonterm. Index indicates which production (0 origin);
   --  Source is the right hand side tokens.
   --
   --  Nonterm is classwide to avoid freezing rules.

   Null_Action : constant Semantic_Action := null;

   ----------
   --  Trace

   Trace_Parse : Integer := 0;
   --  If Trace_Parse > 0, Parse prints helpful messages; higher value
   --  prints more.

   type Trace (Descriptor : access FastToken.Descriptor'Class) is abstract tagged limited null record;
   --  Derived types should support Ada.Text_IO for tests/debugging,
   --  and a protocol for inter-process communication for running a
   --  parser as a subprocess of an IDE.

   procedure Put (Trace : in out FastToken.Trace; Item : in String) is abstract;
   --  Put Item to the Trace object

   procedure Put_Line (Trace : in out FastToken.Trace; Item : in String) is abstract;
   --  Accumulate Item in the trace buffer, output the trace buffer to
   --  the display.

   procedure New_Line (Trace : in out FastToken.Trace) is abstract;
   --  Output the trace buffer to the display.

   procedure Put_Trace (Trace : in out FastToken.Trace'Class; Item : in Token_ID);
   --  Accumulate Item in the trace buffer.

   ----------
   --  Misc

   function Int_Image (Item : in Integer) return String;
   --  No leading space

   type Parser_Algorithm_Type is (LALR, LR1);

   type Buffer_Region is record
      Begin_Pos : Natural;
      End_Pos   : Natural;
   end record;

   Null_Buffer_Region : constant Buffer_Region := (Natural'Last, Natural'First);

   function Image (Item : in Buffer_Region) return String;

   function "and" (Left, Right : in Buffer_Region) return Buffer_Region;
   --  Return region enclosing both Left and Right.

   package Region_Lists is new Ada.Containers.Doubly_Linked_Lists (Buffer_Region);

end FastToken;
