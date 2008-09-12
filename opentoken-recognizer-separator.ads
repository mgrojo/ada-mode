-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Christoph Karl Walter Grein
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 2, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Maintainer: Christoph K. W. Grein (Christ-Usch.Grein@T-Online.de)
--
-- Update History:
-- $Log: opentoken-recognizer-separator.ads,v $
-- Revision 1.2  1999/12/27 19:56:04  Ted
-- fix file contents to work w/ new hierarchy
--
-- Revision 1.1  1999/12/27 17:11:39  Ted
-- renamed everything to new hierarchy
--
-- Revision 1.2  1999/09/17 01:59:58  Ted
-- Fix comment typo
--
-- Revision 1.1  1999/08/17 02:50:01  Ted
-- Initial Version
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- This package implements a token recognizer for separators. To match, the
-- token has to match the separator EXACTLY.
-- (The keyword recognizer could have been used as well, but the latter has
-- some traits that are not needed for separators, so these were removed.)
-------------------------------------------------------------------------------
package Opentoken.Recognizer.Separator is

  type Instance is new Opentoken.Recognizer.Instance with private;

  ----------------------------------------------------------------------------
  -- This procedure will be called to create a separator token.
  ----------------------------------------------------------------------------
  function Get (Separator_Literal: in String;
                Reportable       : in Boolean := True) return Instance;

private

  type State_ID is (Text, Done);

  type Instance is new Opentoken.Recognizer.Instance with record

    -- The separator definition
    Literal : Buffers.Bounded_String;

    -- The finite state machine state
    State   : State_ID := Text;
    Substate: Positive := 1;    -- number of the next character to be analyzed

  end record;

  ----------------------------------------------------------------------------
  -- This procedure will be called when analysis on a new candidate string
  -- is started. The Token needs to clear its state (if any).
  ----------------------------------------------------------------------------
  procedure Clear (The_Token: in out Instance);

  ----------------------------------------------------------------------------
  -- This procedure will be called to perform further analysis on a token
  -- based on the given next character.
  ----------------------------------------------------------------------------
  procedure Analyze (The_Token: in out Instance;
                     Next_Char: in     Character;
                     Verdict  :    out Analysis_Verdict);

end Opentoken.Recognizer.Separator;
