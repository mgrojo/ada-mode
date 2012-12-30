--  Abstract :
--
--  OpenToken token for Wisent declarations.
--
--  Copyright (C) 2012 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with OpenToken.Token.Enumerated.String;
package Wisi.Declarations_Token is

   type Instance is new Nonterminal.Instance with private;

   procedure Free (Token : in out Instance);
   --  Free storage allocated for Token contents. Must be called when
   --  done with Token.

   Declarations : constant Instance;
   Grammar      : constant Production_List.Instance;

private

   type Instance is new Nonterminal.Instance with record
      List : Token_List.Instance;
      --  This list is not automatically garbage collected. When this
      --  token is finally used in a command, it must be freed
      --  (via Free above). This allows the action procedures to
      --  just copy the list headers, not do a deep copy.
   end record;

   procedure Noop
     (New_Token :    out Nonterminal.Class;
      Source    : in     Token_List.Instance'Class;
      To_ID     : in     Token_IDs);

   procedure Add
     (New_Token :    out Nonterminal.Class;
      Source    : in     Token_List.Instance'Class;
      To_ID     : in     Token_IDs);

   procedure Create
     (New_Token :    out Nonterminal.Class;
      Source    : in     Token_List.Instance'Class;
      To_ID     : in     Token_IDs);

   package String_Literal is new Tokens.String;

   Bracket_Symbol : constant Tokens.Class := Tokens.Get (Bracket_Symbol_ID);
   Keyword        : constant Tokens.Class := Tokens.Get (Keyword_ID);
   Package_Token  : constant Tokens.Class := Tokens.Get (Package_ID);
   Percent        : constant Tokens.Class := Tokens.Get (Percent_ID);
   Start          : constant Tokens.Class := Tokens.Get (Start_ID);
   String         : constant Tokens.Class := String_Literal.Get (String_ID);
   Token          : constant Tokens.Class := Tokens.Get (Token_ID);

   Declaration : constant Instance :=
     (Nonterminal.Instance (Nonterminal.Get (Declaration_ID)) with Token_List.Null_List);

   Declarations : constant Instance :=
     (Nonterminal.Instance (Nonterminal.Get (Declarations_ID)) with Token_List.Null_List);

   Grammar : constant Production_List.Instance :=
     Declarations <= Percent & Declarations + Noop'Access and
     Declarations <= Declaration & Percent & Declarations + Add'Access and
     Declaration  <= Package_Token & Symbol + Create'Access and
     Declaration  <= Keyword & Symbol & String + Create'Access and
     Declaration  <= Token & Bracket_Symbol & Symbol + Create'Access and
     Declaration  <= Start & Symbol + Create'Access;

end Wisi.Declarations_Token;
--  Local Variables:
--  ada-indent-opentoken: t
--  End:
