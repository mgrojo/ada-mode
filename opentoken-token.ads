-------------------------------------------------------------------------------
--
--  Copyright (C) 2009, 2014 - 2015 Stephe Leake
--  Copyright (C) 1999, 2000 Ted Dennison
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
-------------------------------------------------------------------------------

-----------------------------------------------------------------------------
--  This package defines an abstract enumerated token type, an
--  abstract token source (lexer), and a recursive descent parse
--  operation.

with Ada.Unchecked_Deallocation;
with OpenToken.Recognizer;
with OpenToken.Text_Feeder;
generic

   type Token_ID is (<>);

   First_Terminal : in Token_ID;
   Last_Terminal  : in Token_ID;
   --  Tokens in the range Token_ID'First .. Pred (First_Terminal) are
   --  non-reporting (comments, whitespace), and thus are not used in
   --  generating LALR parse tables.
   --
   --  Tokens in the range Succ (Last_Terminal) .. Token_ID'Last are
   --  the nonterminals of a grammar.

   with function Token_Image (Item : in Token_ID) return String;

package OpenToken.Token is

   subtype Terminal_ID is Token_ID range First_Terminal .. Last_Terminal;
   --  We can't define Nonterminal_ID here, because if Last_Terminal = Token_ID'last, there are no nonterminals.

   type Token_Array_Boolean is array (Token_ID range First_Terminal .. Token_ID'Last) of Boolean;

   type Buffer_Range is record
      Begin_Pos : Integer;
      End_Pos   : Integer;
   end record;

   Null_Buffer_Range : constant Buffer_Range := (Integer'Last, Integer'First);

   type Recognizer_Handle is access all OpenToken.Recognizer.Class;
   --  Defined here rather than in Opentoken.Recognizer to allow
   --  access's of objects declared at the same level as this
   --  package's instantiation.
   --
   --  FIXME: fix this; it's specific to the OpenToken lexer, not
   --  relevant for Aflex. Used in any tests?

   ----------
   --  Token type

   type Instance is tagged private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   procedure Free (Item : in out Handle);

   --  Return a string for debug messages
   function Image (Token : in Instance) return String;

   function Get
     (ID    : in Token_ID := Token_ID'First;
      Name  : in String   := "")
     return Instance'Class;
   --  Get a token with ID, Name. Result is class-wide so derived
   --  types don't have to override Get.

   function "+" (Item : in Token_ID) return Instance'Class;
   --  Calls Get with default Name; for use in LALR Grammar statements.

   procedure Set_Name (Token : in out Class; Name : in String);
   --  If Name is not "", set the token name to Name.

   procedure Set_ID
     (Token : in out Instance'Class;
      ID    : in     Token_ID);
   --  Set Token.ID to ID.
   --
   --  Should not be called after parsing has started; that will
   --  confuse the parser.

   procedure Create
     (Lexeme     : in     String;
      Bounds     : in     Buffer_Range;
      Recognizer : in     Recognizer_Handle;
      New_Token  : in out Instance)
     is null;
   --  Set New_Token components with data from lexer.
   --
   --  Called from Find_Next when a token is returned by the lexer.
   --
   --  Lexeme is the token text.
   --
   --  Bounds is the start and end position of the input text,
   --  relative to the last lexer Reset.
   --
   --  Recognizer is the recognizer that matched the token; allows
   --  getting other data.
   --  FIXME: not meaningful with Aflex lexer; delete this, use
   --  Lexer.Last_Recogizer

   function Copy (Token : in Handle) return Handle;
   --  Return a newly allocated copy of Token, or null if Token is null.

   function ID (Token : in Instance'Class) return Token_ID;
   --  Class-wide so it is consistent with accessing the ID directly.

   function Name (Token : in Instance) return String;

   function Has_Name (Token : in Instance) return Boolean;

   function Name_Dispatch (Token : in Class) return String;
   function Name_Dispatch (Token : access constant Instance'Class) return String;
   --  Dispatching calls to Name

   ----------
   --  Token lists

   package List is

      type Instance is tagged private;

      Null_List : constant Instance;

      function Length (Item : in Instance) return Natural;

      function Only (Subject : in Class) return Instance;
      function Only (Subject : in Handle) return Instance;

      function "&" (Left  : in Class; Right : in Class) return Instance;
      function "&" (Left  : in Class; Right : in Instance) return Instance;
      function "&" (Left  : in Instance; Right : in Class) return Instance;
      function "&" (Left  : in Instance; Right : in Instance) return Instance;
      function "&" (Left  : in Handle; Right : in Handle) return Instance;
      function "&" (Left  : in Instance; Right : in Handle) return Instance;
      function "&" (Left  : in Handle; Right : in Instance) return Instance;

      procedure Clean (List : in out Instance);
      --  Delete and free all elements of List

      type List_Iterator is private;
      Null_Iterator : constant List_Iterator;

      function Initial_Iterator (List : in Instance) return List_Iterator;
      function First (List : in Instance) return List_Iterator renames Initial_Iterator;

      procedure Next_Token (Iterator : in out List_Iterator);
      procedure Next (Iterator : in out List_Iterator) renames Next_Token;
      function Next_Token (Iterator : in List_Iterator) return List_Iterator;
      function Next (Iterator : in List_Iterator) return List_Iterator renames Next_Token;
      --  Null_Iterator if there is no next token.

      function Token_Handle (Iterator : in List_Iterator) return Handle;
      function ID (Iterator : in List_Iterator) return Token_ID;

      function Names (List : in Instance) return String;
      --  Return names of tokens in list, for error messages

      procedure Enqueue (List  : in out Instance; Token : in     Handle);
      procedure Add (List : in out Instance; Token : in OpenToken.Token.Handle) renames Enqueue;
      --  Add Token to List, at the head. Token.all is not copied; it
      --  will be freed by Clean.

      procedure Append (List  : in out Instance; Token : in     Handle);
      --  Append to tail of List, without copying Token.all

      procedure Print (Item : in Instance);
      --  Print Item to Ada.Text_IO.Current_Output.

   private
      type List_Node;
      type List_Node_Ptr is access List_Node;
      type List_Node is record
         Token : Handle;
         Next  : List_Node_Ptr;
      end record;

      type Instance is tagged record
         Head : List_Node_Ptr;
         Tail : List_Node_Ptr;
      end record;

      type List_Iterator is new List_Node_Ptr;
      Null_Iterator : constant List_Iterator := null;

      Null_List : constant Instance := (null, null);

   end List;

   ----------
   --  Abstract lexer interface

   type Source is abstract tagged record
      Feeder : OpenToken.Text_Feeder.Text_Feeder_Ptr;
   end record;

   subtype Source_Class is Source'Class;

   type Source_Handle is access all Source_Class;

   procedure Reset (Analyzer : in out Source; Buffer_Size : in Integer := 1024) is abstract;
   --  Reset Analyzer, to start finding tokens. Reallocate input
   --  buffer to Buffer_Size.
   --
   --  This is appropriate when the Feeder text has been
   --  changed.

   procedure Set_Text_Feeder
     (Analyzer : in out Source;
      Feeder   : in     OpenToken.Text_Feeder.Text_Feeder_Ptr)
     is abstract;

   function End_Of_Text (Analyzer : in Source) return Boolean is abstract;
   --  True if Analyzer's internal buffer is empty, and
   --  Analyzer.Text_Feeder reports End_Of_Text.

   function End_Of_Buffered_Text (Analyzer : in Source) return Boolean is abstract;
   --  True if Analyzer's internal buffer is empty.
   --  FIXME: delete?

   procedure Discard_Buffered_Text (Analyzer : in out Source) is abstract;
   --  Discard text in Analyzer's internal buffer. Do this when a
   --  parse error is encountered, and you want to start over.

   function Lexeme (Analyzer : in Source) return String is abstract;
   --  Returns the actual text of the last token that was matched.

   function Bounds (Analyzer : in Source) return Buffer_Range is abstract;
   --  Returns the position of the start and end of the last token
   --  that was matched, in the internal buffer.
   --
   --  Most useful when the internal buffer holds the entire input
   --  text, as it will for editor parsers.

   function Line (Analyzer : in Source) return Natural is abstract;
   --  Returns the current text line number at which processing will resume.
   --  This is particularly useful for printing error messages when
   --  syntax errors are detected.

   function Column (Analyzer : in Source) return Natural is abstract;
   --  Returns the current text column number at which processing will
   --  resume. This is particularly useful for printing error messages
   --  when syntax errors are detected. First column number is given
   --  in Initialize.

   procedure Find_Next (Analyzer : in out Source) is abstract;
   --  Locate the next token.
   --
   --  Raises Syntax_Error with an appropriate message if no token
   --  is found and there is no default token.

   function Name (Analyzer : in Source; ID : in Token_ID) return String is abstract;
   --  Return the token name from the Analyzer.Syntax_List.

   function Get (Analyzer : in Source) return Class is abstract;
   --  Returns the last token that was matched. This must be a copy of a
   --  token allocated during initialize; it will not be freed.

private
   type Instance is tagged record
      Name  : access String;
      ID    : Token_ID;
   end record;

   procedure Dispose is new Ada.Unchecked_Deallocation (Class, Handle);

end OpenToken.Token;
