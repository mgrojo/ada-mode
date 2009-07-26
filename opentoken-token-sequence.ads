-------------------------------------------------------------------------------
--
-- Copyright (C) 2009 Stephe Leake
-- Copyright (C) 2000 Ted Dennison
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
--  This package defines a reusable token for a simple sequence of
--  tokens. These a quite easy to create yourself, of course. But
--  having a prebuilt one allows you to easily use it in constructors
--  for other tokens.
--
-------------------------------------------------------------------------------

with OpenToken.Token.Linked_List;
package OpenToken.Token.Sequence is

   type Instance is new Token.Instance with private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   Default_Lookahead : Integer := 1;

   -------------------------------------------------------------------
   --  Default_Lookahead should be set to the number of lookaheads
   --  required by the grammar. However, it may be possible to reduce
   --  that for some tokens.
   -------------------------------------------------------------------
   procedure Set_Lookahead (Token : in out Instance; Lookahead : in Integer);

   ----------------------------------------------------------------------
   --  When called with Actively => False, the number of tokens to
   --  lookahead is given by Match.Lookahead, which defaults to
   --  Default_Lookahead, but may be overridden by Set_Lookahead.
   ----------------------------------------------------------------------
   overriding procedure Parse
     (Match    : access Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean := True);

   -----------------------------------------------------------------------
   --  Create a token sequence from a pair of token handles.
   --
   --  If either is a sequence, it is included by reference; the
   --  member list is _not_ examined. Together with returning Instance
   --  rather than Handle, this allows for controlled recursion. It
   --  also requires the use of New_Selection to return an object
   --  compatible with Selection and other tokens, which has the
   --  effect of making it clear when recursion is desired.
   --
   --  These arguments must be 'access OpenToken.Token.Class', rather
   --  than 'in OpenToken.Token.Handle', in order to accept any
   --  derived type Handle (Ada is annoying in this case!). However,
   --  they are immediately converted to OpenToken.Token.Handle in the
   --  body, so they must have library accessibility level.
   ------------------------------------------------------------------------
   function "&"
     (Left  : access OpenToken.Token.Class;
      Right : access OpenToken.Token.Class)
     return Instance;

   ---------------------------------------------------------------------
   --  Create a token sequence from a sequence and a token handle. The
   --  token is added to the sequence.
   ---------------------------------------------------------------------
   function "&"
     (Left  : access OpenToken.Token.Class;
      Right : in     Instance)
     return Instance;
   function "&"
     (Left  : in     Instance;
      Right : access OpenToken.Token.Class)
     return Instance;

   -------------------------------------------------------------------
   --  Create a token sequence from a pair of sequences. The sequences
   --  are combined to return a single sequence.
   -------------------------------------------------------------------
   function "&"
     (Left  : in Instance;
      Right : in Instance)
     return Instance;

   ----------------------------------------------------------------------------
   --  Return a newly allocated instance which is a copy of the given
   --  instance, with an optional new name and lookahead.
   ----------------------------------------------------------------------------
   function New_Instance
     (Old_Instance : in Instance;
      Name         : in String   := "";
      Lookahead    : in Integer  := Default_Lookahead)
     return Handle;

   --------------------------------------------------------------------
   --  Set the name of Token; useful when it is created with "or"
   --  rather than New_Instance.
   --------------------------------------------------------------------
   procedure Set_Name (Token : in out Instance; Name : in String);

   --------------------------------------------------------------------
   --  Return the name specified in New_Instance. If that's null,
   --  return OpenToken.Token.Name (Token).
   --------------------------------------------------------------------
   overriding function Name (Token : in Instance) return String;

   overriding procedure Expecting (Token : access Instance; List : in out Linked_List.Instance);

   ----------------------------------------------------------------------------
   --  This routine is called when an entire sequence has been actively
   --  parsed. Using is the sequence of tokens.
   ----------------------------------------------------------------------------
   procedure Build
     (Match : in out Instance;
      Using : in     Token.Linked_List.Instance)
   is null;

private
   type Instance is new Token.Instance with record
      Lookahead : Integer := Default_Lookahead;
      Members   : Token.Linked_List.Instance;
      Name      : access String;
   end record;

end OpenToken.Token.Sequence;
