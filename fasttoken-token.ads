--  Abstract :
--
--  An enumerated token type.
--
--  To store additional information about a token, see
--  fasttoken-token_regions.ads or fasttoken-token_wisi.ads.
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

with Ada.Containers;
with Ada.Text_IO;
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

   function Image (Item : in Token_ID) return String renames Token_Image;
   --  Visible to clients.

   subtype Terminal_ID is Token_ID range First_Terminal .. Last_Terminal;
   subtype Nonterminal_ID is Token.Token_ID range Token.Token_ID'Succ (Token.Last_Terminal) .. Token.Token_ID'Last;
   --  We assume Last_Terminal < Token_ID'last; ie there are some nonterminals.

   subtype Grammar_ID is Token_ID range First_Terminal .. Token.Token_ID'Last;
   --  Tokens that can be used in a grammar.

   type Token_ID_Set is array (Grammar_ID) of Boolean;
   type Terminal_ID_Set is array (Terminal_ID) of Boolean;
   type Nonterminal_ID_Set is array (Nonterminal_ID) of Boolean;
   type Nonterminal_Array_Terminal_Set is array (Token.Nonterminal_ID) of Terminal_ID_Set;
   type Nonterminal_Array_Token_Set is array (Token.Nonterminal_ID) of Token_ID_Set;

   function Image (Item : in Token_ID_Set) return String;
   function Any is new Gen_Any_1D (Grammar_ID, Token_ID_Set);
   function Any is new Gen_Any_1D (Terminal_ID, Terminal_ID_Set);
   function Any is new Gen_Any_1D (Nonterminal_ID, Nonterminal_ID_Set);
   function Any is new Gen_Any_2D
     (Terminal_ID, Terminal_ID_Set, Nonterminal_ID, Nonterminal_Array_Terminal_Set, Any);
   function Any is new Gen_Any_2D
     (Grammar_ID, Token_ID_Set, Nonterminal_ID, Nonterminal_Array_Token_Set, Any);

   procedure Put is new Gen_Put_1D (Grammar_ID, Token_ID_Set, Token_Image, Any);
   procedure Put is new Gen_Put_1D (Terminal_ID, Terminal_ID_Set, Token_Image, Any);
   procedure Put is new Gen_Put_1D (Nonterminal_ID, Nonterminal_ID_Set, Token_Image, Any);
   procedure Put is new Gen_Put_2D
     (Terminal_ID, Terminal_ID_Set, Nonterminal_ID, Nonterminal_Array_Terminal_Set, Token_Image, Token_Image, Any, Any);
   procedure Put is new Gen_Put_2D
     (Grammar_ID, Token_ID_Set, Nonterminal_ID, Nonterminal_Array_Token_Set, Token_Image, Token_Image, Any, Any);
   --  Put Item to Ada.Text_IO.Current_Output.

   ----------
   --  Token lists

   package List is

      type Instance is tagged private;

      Null_List : constant Instance;

      function Is_Empty (Item : in Instance) return Boolean;

      function Length (Item : in Instance) return Ada.Containers.Count_Type;

      function Only (Subject : in Token_ID) return Instance;

      function "&" (Left : in Token_ID; Right : in Token_ID) return Instance;
      function "&" (Left : in Instance; Right : in Token_ID) return Instance;

      procedure Clean (List : in out Instance);
      --  Delete and free all elements of List

      type List_Iterator is private;
      Null_Iterator : constant List_Iterator;

      function First (List : in Instance) return List_Iterator;

      function Pop (List : in out Instance) return Token_ID;
      --  Delete head of List from List, return it.

      procedure Next (Iterator : in out List_Iterator);
      function Next (Iterator : in List_Iterator) return List_Iterator;
      --  Null_Iterator if there is no next token.

      function Is_Done (Iterator : in List_Iterator) return Boolean;
      function Is_Null (Iterator : in List_Iterator) return Boolean renames Is_Done;

      function Current (Iterator : in List_Iterator) return Token_ID;
      function ID (Iterator : in List_Iterator) return Token_ID renames Current;

      procedure Prepend (List : in out Instance; Item : in Token_ID);
      --  Add Token to the head of List.

      procedure Append (List  : in out Instance; Item : in Token_ID);
      --  Append to tail of List.

      procedure Put_Trace (Item : in Instance);
      --  Put Item to Put_Trace.

   private
      type List_Node;
      type List_Node_Ptr is access List_Node;
      type List_Node is record
         ID   : Token_ID;
         Next : List_Node_Ptr;
      end record;

      type Instance is tagged record
         Head : List_Node_Ptr;
         Tail : List_Node_Ptr;
      end record;

      type List_Iterator is new List_Node_Ptr;
      Null_Iterator : constant List_Iterator := null;

      Null_List : constant Instance := (null, null);
   end List;

   type Semantic_Action is access procedure
     (Nonterm : in Nonterminal_ID;
      Index   : in Natural;
      Source  : in Token.List.Instance);
   --  Routines of this type are called by the parser when it reduces
   --  a production to Nonterm. Index indicates which production (0 origin);
   --  Source is the right hand side tokens.

   procedure Null_Semantic_Action
     (Nonterm : in Nonterminal_ID;
      Index   : in Natural;
      Source  : in Token.List.Instance)
     is null;

   Null_Action : constant Semantic_Action := Null_Semantic_Action'Access;

   type State_Type is null record;

   procedure Null_Merge_Tokens
     (Nonterm : in     Nonterminal_ID;
      Index   : in     Natural;
      Tokens  : in     List.Instance;
      Action  : in     Semantic_Action;
      State   : in out State_Type)
     is null;
   --  For instantiating fasttoken-parser-lr-parser.ads with plain tokens.

end FastToken.Token;
