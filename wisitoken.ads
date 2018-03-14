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
--  Copyright (C) 2009, 2010, 2013 - 2015, 2017, 2018 Stephe Leake
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
--  This software was originally developed with the name OpenToken by
--  the following company, and was released as open-source software as
--  a service to the community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000

pragma License (Modified_GPL);

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with SAL.Gen_Unbounded_Definite_Vectors.Gen_Protected;
package WisiToken is

   Syntax_Error : exception; -- no recovery for a syntax error was found

   Parse_Error : exception; -- a non-recoverable non-fatal error was encountered; editing the input can fix the error.

   Fatal_Error : exception; -- Error in code or grammar; editing input cannot fix error.

   Grammar_Error : exception; -- Grammar is not consistent (ie unused tokens, missing productions, invalid actions)

   User_Error : exception; -- other user error (ie command line parameter)

   Programmer_Error : exception; -- a programming convention has been violated

   subtype Positive_Index_Type is SAL.Peek_Type;

   ----------
   --  Token IDs

   type Token_ID is range 0 .. Integer'Last; -- 0 origin to match elisp array

   Invalid_Token_ID : constant Token_ID := Token_ID'Last;

   type Token_ID_Array_String is array (Token_ID range <>) of access constant String;
   type Token_ID_Array_Natural is array (Token_ID range <>) of Natural;

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

      New_Line_ID    : Token_ID;
      Comment_ID     : Token_ID;
      Left_Paren_ID  : Token_ID;
      Right_Paren_ID : Token_ID;
      --  If the language does not define these tokens, set them to
      --  Invalid_Token_ID.

      String_1_ID  : Token_ID; -- delimited by '
      String_2_ID  : Token_ID; -- delimited by "
      --
      --  Support for lexer level missing quote error recovery. If the
      --  language does not have two kinds of string literals, set one or
      --  both of these to Invalid_Token_ID.

      Image : Token_ID_Array_String (Token_ID'First .. Last_Nonterminal);
      --  User names for tokens.

      Terminal_Image_Width : Integer;
      Image_Width          : Integer; --  max width of Image
   end record;

   function Image (Item : in Token_ID; Desc : in Descriptor'Class; Pad : in Boolean := False) return String;
   --  Return Desc.Image (Item), possibly padded to
   --  Terminal_Image_Width (if Item is a terminal) or to Image_Width.

   function Int_Image (Item : in Token_ID) return String;
   --  Return Token_ID'Image, leading space stripped.

   function Find_ID (Descriptor : in WisiToken.Descriptor'Class; Name : in String) return Token_ID;
   --  Return index of Name in Descriptor.Image. If not found, raise Programmer_Error.

   type Token_ID_Array is array (Positive range <>) of Token_ID;

   package Token_ID_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Positive, Token_ID);

   function Image (Item : in Token_ID_Arrays.Vector; Descriptor : in WisiToken.Descriptor'Class) return String;

   type Token_ID_Set is array (Token_ID range <>) of Boolean;

   function Any (Item : in Token_ID_Set) return Boolean;

   function Count (Item : in Token_ID_Set) return Integer;
   --  Count of True elements.

   function Image
     (Item      : in Token_ID_Set;
      Desc      : in Descriptor'Class;
      Max_Count : in Integer := Integer'Last;
      Inverted  : in Boolean := False)
     return String;

   type Token_Array_Token_Set is array (Token_ID range <>, Token_ID range <>) of Boolean;

   function Slice (Item : in Token_Array_Token_Set; I : in Token_ID) return Token_ID_Set;
   function Any (Item : in Token_Array_Token_Set; I : in Token_ID) return Boolean;
   function Any (Item : in Token_Array_Token_Set) return Boolean;
   procedure Or_Slice (Item : in out Token_Array_Token_Set; I : in Token_ID; Value : in Token_ID_Set);

   procedure Put (Descriptor : in WisiToken.Descriptor; Item : in Token_ID_Set);
   procedure Put (Descriptor : in WisiToken.Descriptor; Item : in Token_Array_Token_Set);
   --  Put Item to Ada.Text_IO.Current_Output, using valid Ada aggregate syntax

   function To_Lookahead (Item : in Token_ID; Descriptor : in WisiToken.Descriptor) return Token_ID_Set;
   --  Base implementation returns (Descriptor.First_Terminal ..
   --  Descriptor.Last_Terminal), with Item = True, others False. LALR
   --  child type adds Propagate_ID.

   function Lookahead_Image (Item : in Token_ID_Set; Descriptor : in WisiToken.Descriptor) return String;
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
   function To_Lookahead (Item : in Token_ID; Descriptor : in LALR_Descriptor) return Token_ID_Set;

   overriding
   function Lookahead_Image (Item : in Token_ID_Set; Descriptor : in LALR_Descriptor) return String;

   type Buffer_Pos is range 1 .. Integer'Last; -- match Emacs buffer origin.
   type Buffer_Region is record
      First : Buffer_Pos;
      Last  : Buffer_Pos;
   end record;

   Invalid_Buffer_Pos : constant Buffer_Pos    := Buffer_Pos'Last;
   Null_Buffer_Region : constant Buffer_Region := (Buffer_Pos'Last, Buffer_Pos'First);

   function Length (Region : in Buffer_Region) return Natural is (Natural (Region.Last - Region.First + 1));

   function Inside (Pos : in Buffer_Pos; Region : in Buffer_Region) return Boolean
     is (Region.First <= Pos and Pos <= Region.Last);

   function Image (Item : in Buffer_Region) return String;

   function "and" (Left, Right : in Buffer_Region) return Buffer_Region;
   --  Return region enclosing both Left and Right.

   type Base_Token is tagged record
      --  Base_Token is used in the core parser and error recovery. The cost
      --  of deleting a token in error recovery depends on whether it is
      --  empty or not, so we need Byte_Region. One error recovery algorithm
      --  matches names; those are kept in the syntax tree
      --  (wisitoken-syntax_trees.ads).
      --
      --  We do not include all of the information used by the semantic
      --  actions here, because thousands of copies of the parser stack and
      --  syntax tree can be made during error recovery, so minimizing its
      --  size is important. FIXME: no longer true; only indices into
      --  Parser.All_Tokens are copied.

      ID          : Token_ID      := Invalid_Token_ID;
      Byte_Region : Buffer_Region := Null_Buffer_Region;

      --  FIXME: add Char_Region for Emacs error messages/repair?
   end record;

   function Image
     (Item       : in Base_Token;
      Descriptor : in WisiToken.Descriptor'Class;
      ID_Only    : in Boolean := False)
     return String;

   Invalid_Token : constant Base_Token := (others => <>);

   type Base_Token_Index is range 0 .. Integer'Last;
   subtype Token_Index is Base_Token_Index range 1 .. Base_Token_Index'Last;

   type Token_Index_Array is array (Natural range <>) of Token_Index;

   type Base_Token_Array is array (Positive_Index_Type range <>) of Base_Token;

   package Base_Token_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Token_Index, Base_Token);

   Invalid_Token_Index : constant Base_Token_Index := Base_Token_Arrays.No_Index;

   package Protected_Base_Token_Arrays is new Base_Token_Arrays.Gen_Protected;

   function Image
     (Item       : in Base_Token_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class)
     return String;

   ----------
   --  Trace

   Trace_Parse : Integer  := 0;
   --  If Trace_Parse > 0, Parse prints messages helpful for debugging
   --  the grammar and/or the parser; higher value prints more.
   --
   --  Trace_Parse levels; output info if Trace_Parse > than:
   --
   Outline     : constant := 0; -- spawn/terminate parallel parsers, error recovery enter/exit
   Detail      : constant := 1; -- add each parser cycle
   Extra       : constant := 2; -- add pending semantic state operations
   Lexer_Debug : constant := 3; -- add lexer debug

   Trace_McKenzie : Integer  := 0;
   --  If Trace_McKenzie > 0, Parse prints messages helpful for debugging error recovery.
   --
   --  Outline - error recovery enter/exit
   --  Detail  - add each error recovery configuration
   --  Extra   - add error recovery parse actions

   Trace_Action : Integer := 0;
   --  Output during Execute_Action.

   Trace_Generate : Integer := 0;
   --  Output during grammar generation.

   type Trace (Descriptor : not null access constant WisiToken.Descriptor'Class) is abstract tagged limited null record;
   --  Output for tests/debugging.

   procedure Put (Trace : in out WisiToken.Trace; Item : in String) is abstract;
   --  Put Item to the Trace display.

   procedure Put_Line (Trace : in out WisiToken.Trace; Item : in String) is abstract;
   --  Put Item to the Trace display, followed by a newline.

   procedure New_Line (Trace : in out WisiToken.Trace) is abstract;
   --  Put a newline to the Trace display.

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Token_ID);
   --  Put Item to the Trace display.

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

end WisiToken;
