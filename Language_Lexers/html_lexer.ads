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
-- $Log: html_lexer.ads,v $
-- Revision 1.1  1999/12/27 21:41:57  Ted
-- Merged into OpenToken baseline
--
-- Revision 1.0  1999/12/22  Grein
-- Initial Version
--
-- Revision 0.0  1999/12/16  Grein
-- PDL
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

package HTML_Lexer is

   ------------------------------------------------------------------------
   -- This ia a lexical analyser for the HTML language.
   --
   --   <Tag Attribute=Value Attribute="String"> Text with &entity; </Tag>
   --
   -- This very first version is not complete. It simply serves as a
   -- demonstration of feasibility.
   ------------------------------------------------------------------------

   type Token_Name is (-- Document Type Declaration <!DOCTYPE ... >
                       Document_Type,
                       -- Tag delimiters
                       Start_Tag_Opener,  -- <
                       End_Tag_Opener,    -- </
                       Tag_Closer,        -- >
                       -- Tags (without delimiters, ... standing for attributes)
                       HTML,              -- <HTML ...>
                       Head,              -- <HEAD ...>
                       Meta,              -- <META ...>
                       HTML_Body,         -- <BODY ...>
                       Heading_1,         -- <H1 ...>
                       Anchor,            -- <A ...>
                       Image,             -- <IMG ...>
                       -- add further tags here
                       -- Attributes (without = and following value)
                       Content,           -- CONTENT=
                       Hyper_Reference,   -- HREF=
                       Name,              -- NAME=
                       Link_Type,         -- TYPE=
                       Title,             -- TITLE=
                       -- add further attributes here
                       -- The assignment character in attributes
                       Assignment,        -- =
                       -- Values (the right side of assignments)
                       Value,             -- unquoted
                       String,            -- "quoted"
                       -- Comments <!-- anything -->
                       Comment,
                       -- Running text and entities like &amp;
                       Text, Entity,
                       -- Syntax error
                       Bad_Token,
                       --
                       End_Of_File);

   procedure Initialize (File_Name: Standard.String);

   type HTML_Token is private;

   function Next_Token return HTML_Token;

   function Name   (Token: HTML_Token) return Token_Name;
   function Lexeme (Token: HTML_Token) return Standard.String;

private

   type HTML_Token is record
      Name : Token_Name;
      Lexeme : Ada.Strings.Unbounded.Unbounded_String;
      -- more components as needed
   end record;

end HTML_Lexer;
