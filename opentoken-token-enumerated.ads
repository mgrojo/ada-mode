-------------------------------------------------------------------------------
--
-- Copyright (C) 2009, 2012, 2013 Stephe Leake
-- Copyright (C) 1999 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
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
--  This package is the top of a generic hierarchy. Based on the list
--  of IDs it is instantiated with, a user can create tokens and token
--  analyzers.
--
--  This package declares a type for designating a single token. It
--  is designed to be created by an instance of the Token.Analyzer
--  class when a particular kind of token is recognized.
--
--  Packages implementing a child of this type need to include a
--  constructor for the token analyzer and any nessecary utility
--  routines their parser may require.
-----------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with OpenToken.Recognizer;
generic

   type Token_ID is (<>);

package OpenToken.Token.Enumerated is

   --  Make Token_ID visible in client packages
   subtype Parent_Token_ID is Token_ID;

   type Instance is new OpenToken.Token.Instance with private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   ----------------------------------------------------------------------
   --  Recognizer handle type. Defined here rather than in
   --  Opentoken.Recognizer to allow access's of objects declared at
   --  the same level as this package's instantiation.
   ----------------------------------------------------------------------
   type Recognizer_Handle is access all OpenToken.Recognizer.Class;

   type Action is access procedure (Token : in out Instance'Class);

   ----------------------------------------------------------------------
   --  Get a token with the given ID and Build action. Build will be
   --  called by Parse. Result is class-wide so derived types
   --  don't have to override Get.
   ----------------------------------------------------------------------
   function Get
     (ID    : in Token_ID := Token_ID'First;
      Name  : in String   := "";
      Build : in Action   := null)
     return Instance'Class;

   procedure Set_Build (Token : in out Instance'Class; Build : in Action);

   ----------------------------------------------------------------------
   --  Create will be called from Find_Next when a token is
   --  recognized, whether Look_Ahead is True or not.
   --
   --  Lexeme is the matched input text. Recognizer is the recognizer
   --  that matched it.
   --
   --  New_Token is the token that the analyzer associates with
   --  Recognizer (specified when the syntax is created).
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
   ----------------------------------------------------------------------
   procedure Create
     (Lexeme     : in     String;
      Recognizer : in     Recognizer_Handle;
      New_Token  : in out Instance)
     is null;

   --------------------------------------------------------------------
   --  Copy From to To. Called by Parse when a token matches, whether
   --  Actively is true or not. This is just a dispatching version of
   --  ':='; see the comments in Parse for more rationale.
   --
   --  Parse has verified that From'Tag = To'Tag, and that From.ID =
   --  To.ID.
   --------------------------------------------------------------------
   procedure Copy
     (To   : in out Instance;
      From : in     OpenToken.Token.Class)
     is null;

   --------------------------------------------------------------------------
   --  This function returns the ID of the token. This is made
   --  class-wide so it won't be overridable. That is done because
   --  some child packages access the ID directly, so overriding this
   --  routine would lead to inconsistent results.
   --------------------------------------------------------------------------
   function ID (Token : in Instance'Class) return Token_ID;

   ----------------------------------------------------------------------------
   --  Set the given token's ID to the given value
   ----------------------------------------------------------------------------
   procedure Set_ID
     (Token : in out Instance'Class;
      ID    : in     Token_ID);

   --------------------------------------------------------------------
   --  If Match matches the current token, and Actively is True,
   --  copies the results of the earlier call to Create to Match, and
   --  calls Build (if not null).
   --------------------------------------------------------------------
   overriding procedure Parse
     (Match    : access Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean      := True);

   overriding function Name (Token : in Instance) return String;

   type Source is abstract new OpenToken.Token.Source with null record;

   --------------------------------------------------------------------------
   --  Returns the actual text of the last token that was matched.
   --
   --  Raises Programmer_Error when the last token was read from the
   --  lookahead queue.
   --------------------------------------------------------------------------
   function Lexeme (Analyzer : in Source) return String is abstract;

   --------------------------------------------------------------------------
   --  Returns the recognizer handle of the last token that was matched.
   --
   --  Raises Programmer_Error when the last token was read from the
   --  lookahead queue.
   --------------------------------------------------------------------------
   function Last_Recognizer (Analyzer : in Source) return Recognizer_Handle is abstract;

private
   type Instance is new OpenToken.Token.Instance with record
      ID    : Token_ID;
      Build : Action;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Class, Handle);

end OpenToken.Token.Enumerated;
