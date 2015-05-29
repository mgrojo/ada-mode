--  Abstract :
--
--  Type and operations for nonterminal tokens
--
--  Copyright (C) 2009, 2012, 2014, 2015 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
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

generic
package FastToken.Token.Nonterminal is

   type Instance is new FastToken.Token.Instance with null record;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   function Get (ID : in Token_ID := Token_ID'First) return Instance'Class;
   --  Get a nonterminal token with the given ID.
   --
   --  Return type is class-wide to avoid forcing derived types to override this

   function ID (Token : in Handle) return Token_ID;

   type Synthesize is access procedure
     (New_Token :    out Class;
      Source    : in     Token.List.Instance'Class;
      To_ID     : in     Token_ID);
   --  Update New_Token with information from Source, To_ID.
   --
   --  Routines of this type are called by the parser
   --  when it reduces a production to To_ID.

   Synthesize_Self  : constant Synthesize;
   --  Returns a new Nonterminal token with To_ID; ignores Source.

private

   procedure Self_Synthesize
     (New_Token :    out Class;
      Source    : in     Token.List.Instance'Class;
      To_ID     : in     Token_ID);

   Synthesize_Self : constant Synthesize := Self_Synthesize'Access;

end FastToken.Token.Nonterminal;
