--  Abstract :
--
--  Tokens for use by Ada code generated by wisi-output_ada_emacs
--
--  Copyright (C) 2014, 2015  All Rights Reserved.
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

with Ada.Text_IO;
with FastToken.Token;
generic
   type Token_ID is (<>);
   First_Terminal : in Token_ID;
   Last_Terminal  : in Token_ID;

   with function Token_Image (Item : in Token_ID) return String;
   with procedure Put_Trace (Item : in String) is Ada.Text_IO.Put;

   with package Token is new FastToken.Token
     (Token_ID, First_Terminal, Last_Terminal, Token_Image, Put_Trace);
package FastToken.Wisi_Tokens is

   type Instance is new Token.Instance with record
      Buffer_Range : Token.Buffer_Range;
   end record;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   overriding
   function Image (Item : in Instance) return String;

   function Get (ID : in Token_ID) return Token.Instance'Class;
   function Get (ID : in Token_ID) return Token.Handle;
   --  For use in Syntax; sets null Buffer_Range

   overriding
   procedure Create
     (Lexeme    : in     String;
      Bounds    : in     Token.Buffer_Range;
      New_Token : in out Instance);
   --  Callback from Analyzer; stores Buffer_Range in New_Token

   function Get (ID : in Token_ID; Buffer_Range : in Token.Buffer_Range) return Token.Instance'Class;
   --  For use in Actions.

   procedure Self
     (Nonterm : in Token.Nonterminal_ID;
      Source  : in Token.List.Instance);
   --  For use in Actions.

   ----------
   --  Other functions for wisi actions in generated Ada code for
   --  Ada_Emacs target.

   function Total_Buffer_Range
     (Tokens : in Wisi_Tokens.Token.List.Instance'Class)
     return Wisi_Tokens.Token.Buffer_Range;

   function To_Codes (Tokens : in Wisi_Tokens.Token.List.Instance'Class) return String;
   --  Return format for Emacs ada-mode process interface
   --  FIXME: move to child package

end FastToken.Wisi_Tokens;
