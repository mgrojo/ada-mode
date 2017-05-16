--  Abstract :
--
--  An abstract enumerated token type.
--
--  Copyright (C) 2009, 2014 - 2015, 2017 Stephe Leake
--  Copyright (C) 1999, 2000 Ted Dennison
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The FastToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the FastToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
generic

   type Token_ID is (<>);

   First_Terminal : in Token_ID;
   Last_Terminal  : in Token_ID;
   --  Tokens in the range Token_ID'First .. Pred (First_Terminal) are
   --  non-reporting (comments, whitespace), and thus are not used in
   --  generating parse tables.
   --
   --  Tokens in the range Succ (Last_Terminal) .. Token_ID'Last are
   --  the nonterminals of a grammar.

   with function Token_Image (Item : in Token_ID) return String;

   with procedure Put_Trace (Item : in String) is Ada.Text_IO.Put;
   --  Accumulate Item in the trace buffer.
   --
   --  Called when FastToken.Trace_Parse > 0

package FastToken.Token is

   subtype Terminal_ID is Token_ID range First_Terminal .. Last_Terminal;
   subtype Nonterminal_ID is Token.Token_ID range Token.Token_ID'Succ (Token.Last_Terminal) .. Token.Token_ID'Last;
   --  We assume Last_Terminal < Token_ID'last; ie there are some nonterminals.

   subtype Reporting_ID is Token_ID range First_Terminal .. Token.Token_ID'Last;
   --  Leaves out whitespace, comments; tokens the lexer will never return.

   type Token_ID_Set is array (Reporting_ID) of Boolean;
   type Terminal_ID_Set is array (Terminal_ID) of Boolean;
   type Nonterminal_ID_Set is array (Nonterminal_ID) of Boolean;
   type Nonterminal_Array_Terminal_Set is array (Token.Nonterminal_ID) of Terminal_ID_Set;
   type Nonterminal_Array_Token_Set is array (Token.Nonterminal_ID) of Token_ID_Set;

   function Image (Item : in Token_ID_Set) return String;
   function Any is new Gen_Any_1D (Reporting_ID, Token_ID_Set);
   function Any is new Gen_Any_1D (Terminal_ID, Terminal_ID_Set);
   function Any is new Gen_Any_1D (Nonterminal_ID, Nonterminal_ID_Set);
   function Any is new Gen_Any_2D
     (Terminal_ID, Terminal_ID_Set, Nonterminal_ID, Nonterminal_Array_Terminal_Set, Any);
   function Any is new Gen_Any_2D
     (Reporting_ID, Token_ID_Set, Nonterminal_ID, Nonterminal_Array_Token_Set, Any);

   procedure Put is new Gen_Put_1D (Reporting_ID, Token_ID_Set, Token_Image, Any);
   procedure Put is new Gen_Put_1D (Terminal_ID, Terminal_ID_Set, Token_Image, Any);
   procedure Put is new Gen_Put_1D (Nonterminal_ID, Nonterminal_ID_Set, Token_Image, Any);
   procedure Put is new Gen_Put_2D
     (Terminal_ID, Terminal_ID_Set, Nonterminal_ID, Nonterminal_Array_Terminal_Set, Token_Image, Token_Image, Any, Any);
   procedure Put is new Gen_Put_2D
     (Reporting_ID, Token_ID_Set, Nonterminal_ID, Nonterminal_Array_Token_Set, Token_Image, Token_Image, Any, Any);
   --  Put Item to Ada.Text_IO.Current_Output.

   type Buffer_Range is record
      Begin_Pos : Integer;
      End_Pos   : Integer;
   end record;

   Null_Buffer_Range : constant Buffer_Range := (Integer'Last, Integer'First);

   ----------
   --  Token type

   type Instance is tagged private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   procedure Free (Item : in out Handle);

   function Image (Token : in Instance) return String;
   --  Return a string for debug messages

   function Get (ID : in Token_ID) return Instance'Class;
   --  Get a token with ID. Result is class-wide so derived
   --  types don't have to override Get.

   function "+" (Item : in Token_ID) return Instance'Class
     renames Get;

   procedure Create
     (Lexeme    : in     String;
      Bounds    : in     Buffer_Range;
      New_Token : in out Instance)
     is null;
   --  Set New_Token components with data from lexer.
   --
   --  Called from Find_Next when a token is returned by the lexer.
   --
   --  Lexeme is the token text.
   --
   --  Bounds is the start and end position of the input text,
   --  relative to the last lexer Reset.

   function Copy (Token : in Handle) return Handle;
   --  Return a newly allocated copy of Token, or null if Token is null.

   function ID (Token : in Instance'Class) return Token_ID;
   --  Class-wide so it is consistent with accessing the ID directly.

   function ID (Token : in Handle) return Token_ID;

   ----------
   --  Token lists

   package List is

      type Instance is tagged private;

      Null_List : constant Instance;

      function Length (Item : in Instance) return Natural;

      function Only (Subject : in Class) return Instance;
      function Only (Subject : in Handle) return Instance;

      function "&" (Left : in Token_ID; Right : in Token_ID) return Instance;
      function "&" (Left : in Instance; Right : in Token_ID) return Instance;
      function "&" (Left : in Class; Right : in Class) return Instance;
      function "&" (Left : in Class; Right : in Instance) return Instance;
      function "&" (Left : in Instance; Right : in Class) return Instance;
      function "&" (Left : in Instance; Right : in Instance) return Instance;
      function "&" (Left : in Handle; Right : in Handle) return Instance;
      function "&" (Left : in Instance; Right : in Handle) return Instance;
      function "&" (Left : in Handle; Right : in Instance) return Instance;

      procedure Clean (List : in out Instance);
      --  Delete and free all elements of List

      type List_Iterator is private;
      Null_Iterator : constant List_Iterator;

      function First (List : in Instance) return List_Iterator;

      procedure Next_Token (Iterator : in out List_Iterator);
      procedure Next (Iterator : in out List_Iterator) renames Next_Token;
      function Next_Token (Iterator : in List_Iterator) return List_Iterator;
      function Next (Iterator : in List_Iterator) return List_Iterator renames Next_Token;
      --  Null_Iterator if there is no next token.

      function Is_Done (Iterator : in List_Iterator) return Boolean;
      function Is_Null (Iterator : in List_Iterator) return Boolean renames Is_Done;

      function Token_Handle (Iterator : in List_Iterator) return Handle;
      function ID (Iterator : in List_Iterator) return Token_ID;

      procedure Enqueue (List  : in out Instance; Token : in     Handle);
      procedure Add (List : in out Instance; Token : in FastToken.Token.Handle) renames Enqueue;
      --  Add Token to List, at the head. Token.all is not copied; it
      --  will be freed by Clean.

      procedure Append (List  : in out Instance; Token : in     Handle);
      --  Append to tail of List, without copying Token.all

      procedure Put_Trace (Item : in Instance);
      --  Put Item to Put_Trace.

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

private
   type Instance is tagged record
      ID : Token_ID;
   end record;

   procedure Dispose is new Ada.Unchecked_Deallocation (Class, Handle);
   --  Not Free; used to implement visible Free above.

end FastToken.Token;
