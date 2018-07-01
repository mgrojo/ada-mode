--  Abstract:
--
--  Root of WisiToken lexer/parser generator and exector.
--
--  The token type is an integer subtype, not an enumeration type, to
--  avoid making this package generic, which would make all other
--  packages generic.
--
--  Additional information about a token can be stored in the
--  'augmented' field of the syntax tree; see
--  wisitoken-syntax_trees.ads.
--
--  References:
--
--  [dragon] "Compilers Principles, Techniques, and Tools" by Aho,
--  Sethi, and Ullman (aka: "The [Red] Dragon Book" due to the dragon
--  on the cover).
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

with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with SAL.Gen_Trimmed_Image;
with SAL.Gen_Unbounded_Definite_Vectors.Gen_Image;
with SAL.Gen_Unbounded_Definite_Vectors.Gen_Image_Aux;
package WisiToken is

   Syntax_Error : exception; -- no recovery for a syntax error was found

   Parse_Error : exception; -- a non-recoverable non-fatal error was encountered; editing the input can fix the error.

   Fatal_Error : exception; -- Error in code or grammar; editing input cannot fix error.

   Grammar_Error : exception;
   --  Grammar file has bad syntax, or grammar is not consistent (ie
   --  unused tokens, missing productions, invalid actions)

   User_Error : exception; -- other user error (ie command line parameter)

   Programmer_Error : exception; -- a programming convention has been violated

   subtype Positive_Index_Type is SAL.Peek_Type;

   type Unknown_State_Index is new Integer range -1 .. Integer'Last;
   subtype State_Index is Unknown_State_Index range 0 .. Unknown_State_Index'Last;
   Unknown_State : constant Unknown_State_Index := -1;

   function Image (Item : in Unknown_State_Index) return String;
   --  no leading space; " " for Unknown_State

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

      Case_Insensitive : Boolean; -- keywords and names
      New_Line_ID      : Token_ID;
      Comment_ID       : Token_ID;
      Left_Paren_ID    : Token_ID;
      Right_Paren_ID   : Token_ID;
      --  If the language does not define these tokens, set them to
      --  Invalid_Token_ID.

      String_1_ID  : Token_ID; -- delimited by ', error if New_Line_ID
      String_2_ID  : Token_ID; -- delimited by ", error if New_Line_ID
      --
      --  Support for missing quote error recovery. If the language does not
      --  have two kinds of string literals, set one or both of these to
      --  Invalid_Token_ID.

      Embedded_Quote_Escape_Doubled : Boolean;
      --  True if quote characters embedded in strings are escaped by
      --  doubling (as in Ada); false if by preceding with backslash (as in
      --  C).

      Image : Token_ID_Array_String (Token_ID'First .. Last_Nonterminal);
      --  User names for tokens.

      Terminal_Image_Width : Integer;
      Image_Width          : Integer; --  max width of Image
   end record;

   type Token_ID_Set is array (Token_ID range <>) of Boolean;

   function To_Lookahead (Item : in Token_ID; Descriptor : in WisiToken.Descriptor) return Token_ID_Set;
   --  Base implementation returns (Descriptor.First_Terminal ..
   --  Descriptor.Last_Terminal), with Item = True, others False. LALR
   --  child type adds Propagate_ID.

   function Lookahead_Image (Item : in Token_ID_Set; Descriptor : in WisiToken.Descriptor) return String;
   --  Base implementation just returns aggregate syntax for Item.
   --  LALR child includes '#' for Propagate_ID.

   --  The following subprograms are intended to _not_ be primitive
   --  operations of Descriptor; hence 'Class.

   function Padded_Image (Item : in Token_ID; Desc : in Descriptor'Class) return String;
   --  Return Desc.Image (Item), padded to Terminal_Image_Width (if Item
   --  is a terminal) or to Image_Width.

   function Image (Item : in Token_ID; Desc : in Descriptor'Class) return String;
   --  Return Desc.Image (Item), or empty string for Invalid_Token_ID.

   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Token_ID);

   procedure Put_Tokens (Descriptor : in WisiToken.Descriptor'Class);
   --  Put user readable token list (token_id'first ..
   --  descriptor.last_nonterminal) to Ada.Text_IO.Current_Output

   function Find_ID (Descriptor : in WisiToken.Descriptor'Class; Name : in String) return Token_ID;
   --  Return index of Name in Descriptor.Image. If not found, raise Programmer_Error.

   type Token_ID_Array is array (Positive range <>) of Token_ID;

   package Token_ID_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Positive, Token_ID);

   function Image is new Token_ID_Arrays.Gen_Image_Aux (WisiToken.Descriptor'Class, Image);

   function Shared_Prefix (A, B : in Token_ID_Arrays.Vector) return Natural;
   --  Return last index in A of a prefix shared between A, B; 0 if none.

   function To_Token_ID_Set (First, Last : in Token_ID; Item : in Token_ID_Array) return Token_ID_Set;
   --  First, Last determine size of result.
   --  For each element in Item, set result (element) True.

   procedure To_Set (Item : in Token_ID_Arrays.Vector; Set : out Token_ID_Set);
   --  For each element of Item, set Set (element) True.

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

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in Token_ID_Set);
   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in Token_Array_Token_Set);
   --  Put Item to Ada.Text_IO.Current_Output, using valid Ada aggregate
   --  syntax.
   --
   --  Descriptor'Class to avoid 'primitive operation declared too late'

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
   --  LALR_Descriptor is only required at grammar generate time; parse
   --  time can always use Descriptor.

   overriding
   function To_Lookahead (Item : in Token_ID; Descriptor : in LALR_Descriptor) return Token_ID_Set;

   overriding
   function Lookahead_Image (Item : in Token_ID_Set; Descriptor : in LALR_Descriptor) return String;

   ----------
   --  Production IDs; see wisitoken-productions.ads for more

   type Production_ID is record
      Nonterm : Token_ID := Invalid_Token_ID;
      RHS     : Natural  := 0;
      --  Index into the production table.
   end record;

   function Image (Item : in Production_ID) return String;
   --  Ada positional aggregate syntax, for code generation.

   function Trimmed_Image (Item : in Production_ID) return String;
   --  Nonterm.rhs_index, both integers, no leading or trailing space;
   --  for parse table output and diagnostics.

   Prod_ID_Image_Width : constant Integer := 7;
   --  Max width of Trimmed_Image

   function Padded_Image (Item : in Production_ID; Width : in Integer) return String;
   --  Trimmed_Image padded with leading spaces to Width

   package Production_ID_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Positive, Production_ID);
   function Image is new Production_ID_Arrays.Gen_Image (Image);
   function Trimmed_Image is new Production_ID_Arrays.Gen_Image (Trimmed_Image);

   type Production_ID_Array is array (Natural range <>) of Production_ID;

   function To_Vector (Item : in Production_ID_Array) return Production_ID_Arrays.Vector;
   function "+" (Item : in Production_ID_Array) return Production_ID_Arrays.Vector renames To_Vector;
   function "+" (Item : in Production_ID) return Production_ID_Arrays.Vector is (To_Vector ((1 => Item)));

   ----------
   --  Tokens

   type Base_Buffer_Pos is range 0 .. Integer'Last;
   subtype Buffer_Pos is Base_Buffer_Pos range 1 .. Base_Buffer_Pos'Last; -- match Emacs buffer origin.
   type Buffer_Region is record
      First : Buffer_Pos;
      Last  : Base_Buffer_Pos; --  allow representing null range.
   end record;

   Invalid_Buffer_Pos : constant Buffer_Pos    := Buffer_Pos'Last;
   Null_Buffer_Region : constant Buffer_Region := (Buffer_Pos'Last, Buffer_Pos'First);

   function Length (Region : in Buffer_Region) return Natural is (Natural (Region.Last - Region.First + 1));

   function Inside (Pos : in Buffer_Pos; Region : in Buffer_Region) return Boolean
     is (Region.First <= Pos and Pos <= Region.Last);

   function Image (Item : in Buffer_Region) return String;

   function "and" (Left, Right : in Buffer_Region) return Buffer_Region;
   --  Return region enclosing both Left and Right.

   type Line_Number_Type is range 1 .. Natural'Last; -- Match Emacs buffer line numbers.

   Invalid_Line_Number : constant Line_Number_Type := Line_Number_Type'Last;

   type Base_Token is tagged record
      --  Base_Token is used in the core parser. The parser only needs ID;
      --  semantic checks need Byte_Region to compare names. Line, Col, and
      --  Char_Region are included for error messages.
      ID : Token_ID := Invalid_Token_ID;

      Byte_Region : Buffer_Region := Null_Buffer_Region;
      --  Index into the Lexer buffer for the token text.

      Line   : Line_Number_Type  := Invalid_Line_Number;
      Column : Ada.Text_IO.Count := 0;
      --  At start of token.

      Char_Region : Buffer_Region := Null_Buffer_Region;
      --  Character position, useful for finding the token location in Emacs
      --  buffers.
   end record;

   type Base_Token_Class_Access is access all Base_Token'Class;
   type Base_Token_Class_Access_Array is array (Positive_Index_Type range <>) of Base_Token_Class_Access;

   function Image
     (Item       : in Base_Token;
      Descriptor : in WisiToken.Descriptor'Class)
     return String;
   --  For debug/test messages.

   procedure Free is new Ada.Unchecked_Deallocation (Base_Token'Class, Base_Token_Class_Access);

   Invalid_Token : constant Base_Token := (others => <>);

   type Base_Token_Index is range 0 .. Integer'Last;
   subtype Token_Index is Base_Token_Index range 1 .. Base_Token_Index'Last;

   type Token_Index_Array is array (Natural range <>) of Token_Index;

   type Base_Token_Array is array (Positive_Index_Type range <>) of Base_Token;

   package Base_Token_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Token_Index, Base_Token);
   type Base_Token_Array_Access is access all Base_Token_Arrays.Vector;

   Invalid_Token_Index : constant Base_Token_Index := Base_Token_Arrays.No_Index;

   package Line_Begin_Token_Vectors is new SAL.Gen_Unbounded_Definite_Vectors (Line_Number_Type, Base_Token_Index);

   function Image is new Base_Token_Arrays.Gen_Image_Aux (WisiToken.Descriptor'Class, Image);

   function Image
     (Token      : in Base_Token_Index;
      Terminals  : in Base_Token_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class)
     return String;

   type Recover_Token is record
      --  Maintaining a syntax tree during recover is too slow, so we store
      --  enough information in the recover stack to perform semantic_checks
      --  and to apply the solution to the main parser state. We make
      --  thousands of copies of the parse stack during recover, so
      --  minimizing size is critical.
      ID : Token_ID := Invalid_Token_ID;

      Byte_Region : Buffer_Region := Null_Buffer_Region;
      --  Byte_Region is used to detect empty tokens, for cost and other issues.

      Min_Terminal_Index : Base_Token_Index := Invalid_Token_Index;
      --  For terminals, index of this token in Shared_Parser.Terminals. For
      --  nonterminals, minimum of contained tokens. For virtuals,
      --  Invalid_Token_Index. Used for push_back of nonterminals.

      Name : Buffer_Region := Null_Buffer_Region;
      --  Set and used by semantic_checks.

      Virtual : Boolean := False;
      --  For terminals, True if inserted by recover. For nonterminals, True
      --  if any contained token has Virtual = True. Used by Semantic_Checks
      --  and push_back.
   end record;

   function Image
     (Item       : in Recover_Token;
      Descriptor : in WisiToken.Descriptor'Class)
     return String;

   type Recover_Token_Array is array (Positive_Index_Type range <>) of Recover_Token;

   package Recover_Token_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Token_Index, Recover_Token);

   function Image is new Recover_Token_Arrays.Gen_Image_Aux (WisiToken.Descriptor'Class, Image);

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
   --  Output during Execute_Action, and unit tests.

   Trace_Generate : Integer := 0;
   --  Output during grammar generation.

   type Trace (Descriptor : not null access constant WisiToken.Descriptor) is abstract tagged limited null record;
   --  Output for tests/debugging. Not used during grammar generation;
   --  that just outputs to Text_IO.Standard_Output.

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

   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Integer);
   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Ada.Containers.Count_Type);

   function Error_Message
     (File_Name : in String;
      Line      : in Line_Number_Type;
      Column    : in Ada.Text_IO.Count;
      Message   : in String)
     return String;
   --  Return Gnu-formatted error message.

   type Generator_Algorithm_Type is (LALR, LR1);

end WisiToken;
