-------------------------------------------------------------------------------
--
--  Copyright (C) 2009, 2014 Stephe Leake
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

--  FIXME: limited with OpenToken.Token.Linked_List; hangs gcc. Because 'limited with' doesn't work for generic
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

   type Action is access procedure (Token : in out Instance'Class);

   procedure Free (Item : in out Handle);

   --  Return a string for debug messages
   function Image (Token : in Instance) return String;

   function Get
     (ID    : in Token_ID := Token_ID'First;
      Name  : in String   := "";
      Build : in Action   := null)
     return Instance'Class;
   --  Get a token with ID, Name, and Build. Build will be called by
   --  Parse. Result is class-wide so derived types don't have to
   --  override Get.

   function "+" (Item : in Token_ID) return Instance'Class;
   --  Calls Get with default Name, Build; for use in LALR Grammar statements.

   procedure Set_Name (Token : in out Class; Name : in String);
   --  If Name is not "", set the token name to Name.

   procedure Set_Build (Token : in out Instance'Class; Build : in Action);

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
   --  FIXME: delete this! or change to Lexer, use Lexer.Last_Recogizer
   --
   --  Create is called even when Look_Ahead is true (when the parse
   --  is inactive), so that Lexeme and Recognizer can be preserved in
   --  the lookahead queue if needed; New_Token is pushed on the
   --  lookahead queue if another token is read with Look_Ahead True.
   --
   --  The recognizer is useful in creating tightly coupled pairs of
   --  tokens and recognizers. This allows communication of
   --  user-defined information global to the analyzer instance while
   --  maintaining overall re-entrancy.

   procedure Copy
     (To   : in out Instance;
      From : in     OpenToken.Token.Class)
     is null;
   --  Copy From to To. Called by Parse when a token is returned by
   --  the lexer, if Actively is true.
   --
   --  This is just a dispatching version of ':='; see the comments in
   --  Parse for more rationale.
   --
   --  Parse has verified that From'Tag = To'Tag, and that From.ID =
   --  To.ID.

   --  Return a newly allocated copy of Token, or null
   function Copy (Token : in Handle) return Handle;

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

   procedure Reset (Analyzer : in out Source) is abstract;
   --  Reset Analyzer, to start finding tokens. This is appropriate
   --  when the Feeder text has been changed.

   procedure Set_Text_Feeder
     (Analyzer : in out Source;
      Feeder   : in     OpenToken.Text_Feeder.Text_Feeder_Ptr)
     is abstract;

   function End_Of_Text (Analyzer : in Source) return Boolean is abstract;
   --  True if Analyzer's internal buffer is empty, and
   --  Analyzer.Text_Feeder reports End_Of_Text.

   function End_Of_Buffered_Text (Analyzer : in Source) return Boolean is abstract;
   --  True if Analyzer's internal buffer is empty.

   procedure Discard_Buffered_Text (Analyzer : in out Source) is abstract;
   --  Discard text in Analyzer's internal buffer. Do this when a
   --  parse error is encountered, and you want to start over.

   function Lexeme (Analyzer : in Source) return String is abstract;
   --  Returns the actual text of the last token that was matched.
   --
   --  Raises Programmer_Error when the last token was read from the
   --  lookahead queue.

   function Line (Analyzer : in Source) return Natural is abstract;
   --  Returns the current text line number at which processing will resume.
   --  This is particularly useful for printing error messages when
   --  syntax errors are detected.

   function Column (Analyzer : in Source) return Natural is abstract;
   --  Returns the current text column number at which processing will
   --  resume. This is particularly useful for printing error messages
   --  when syntax errors are detected. First column number is given
   --  in Initialize.

   procedure Find_Next
     (Analyzer   : in out Source;
      Look_Ahead : in     Boolean := False)
      is abstract;
   --  Locate the next token.
   --
   --  If Look_Ahead is set, the next token after the current one will
   --  be returned, but the current one will not be discarded; they
   --  are saved in the lookahead queue. Subsequent Look_Ahead calls
   --  will return later and later tokens.
   --
   --  The very first call to Find_Next, immediately after Analyzer is
   --  created, cannot have Look_Ahead True. No real parser needs
   --  Look_Ahead True on the first call in a real parse, and this lets
   --  the Analyzer assume there is one actively recognized token to
   --  start the lookahead queue with.
   --
   --  Raises Syntax_Error with an appropriate message if no token
   --  is found and there is no default token.

   function Name (Analyzer : in Source; ID : in Token_ID) return String is abstract;
   --  Return the token name from the Analyzer.Syntax_List.

   function Get (Analyzer : in Source) return Class is abstract;
   --  Returns the last token that was matched.

   type Queue_Mark is abstract tagged limited null record;

   function Mark_Push_Back (Analyzer : in Source) return Queue_Mark'Class is abstract;
   --  Mark a point in the lookahead queue.

   procedure Push_Back (Analyzer : in out Source; Mark : in Queue_Mark'Class) is abstract;
   --  Restore the input point in the lookahead queue. Subsequent
   --  calls to Get will return the token that was current when Mark
   --  was set. This allows a recursive descent parser to backtrack.

   ----------
   --  Abstract recursive descent parser interface

   Default_Lookahead : Integer := 1;

   procedure Parse
     (Match    : access Instance;
      Analyzer : access Source_Class;
      Actively : in     Boolean := True);
   --  Verify that token in Analyzer matches the token Match, possibly
   --  take some action.
   --
   --  If not Actively, this should determine as quickly as possible
   --  whether the parse would fail or succeed, and raise Parse_Error
   --  with no message if it would fail. Depending on the grammar
   --  design, only checking the current Analyzer token would be
   --  enough, but it may be necessary to look ahead. In that case,
   --  Default_Lookahead should be used to specify the default
   --  lookahead count, and the user should be able to override it for
   --  specific tokens. When called in this way, a higher level parse
   --  is choosing between options.
   --
   --  If Actively, upon successful completion, update Match with the
   --  value of the token, and advance Analyzer across all matched
   --  tokens.
   --
   --  If not Actively, Match is unchanged, and Analyzer is not
   --  advanced.
   --
   --  Match is 'access' to match Expecting, which needs to store a
   --  pointer to it in some cases.


   procedure Expecting (Token : access Instance; List : in out OpenToken.Token.List.Instance);
   --  Add expected tokens to List, for error messages in Parse.
   --
   --  Default just adds Token.

   ------------------------------------------------------------------
   --  We _don't_ define a 'Print' procedure for tokens, even though
   --  that might be useful in debugging recursive descent parsers.
   --  The problem is that grammars are often recursive, which leads
   --  to infinite loops in Print, and dealing with such loops is too
   --  hard.

private
   type Instance is tagged record
      Name  : access String;
      ID    : Token_ID;
      Build : Action;
   end record;

   procedure Dispose is new Ada.Unchecked_Deallocation (Class, Handle);

end OpenToken.Token;
