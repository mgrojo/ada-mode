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
-- $Log: html_lexer.adb,v $
-- Revision 1.2  2000/01/27 21:21:39  Ted
-- Fix to work with new text feeder routines
--
-- Revision 1.1  1999/12/27 21:41:57  Ted
-- Merged into OpenToken baseline
--
-- Revision 1.0  1999/12/22  Grein
-- Initial Version
--
-------------------------------------------------------------------------------

with Ada.Text_Io;

with OpenToken.Text_Feeder.Text_IO;
with OpenToken.Text_Feeder.String;

with HTML_Lexer.Basic, HTML_Lexer.Tags;

package body HTML_Lexer is

   -----------------------------------------------------------------------
   -- This package has been implemented as a test of feasibility.
   -- The implementation is open to improvements.
   -----------------------------------------------------------------------
   -- HTML syntax is very different from Ada or Java syntax. This is an
   -- abbreviated excerpt of the HTML 4.0 Reference.
   --
   --    Elements are the structures that describe parts of an HTML
   --    document ... An element has three parts: a start tag, content,
   --    and an end tag. A tag is special text--"markup"--that is
   --    delimited by "<" and ">". An end tag includes a "/" after the
   --    "<" ... The start and end tags surround the content of the
   ---   element:
   --       <EM>This is emphasized text</EM>
   --    ... An element's attributes define various properties for the
   --    element ...
   --
   --       <IMG SRC="wdglogo.gif" ALT="Web Design Group">
   --
   --    An attribute is included in the start tag only--never the end
   --    tag--and takes the form Attribute-name="Attribute-value".
   --
   -- Thus the text between tokens is arbitrary and need not be analysed.
   -- In fact the whole text between tags is treated as a token of its
   -- own.
   -- Inside tags, however, we want to analyse for tag names, attribute
   -- names and attribute values.
   -- Thus we have to analyse the HTML document after an opening "<" and
   -- stop after a closing ">". With OpenToken, however, it is not so easy
   -- to analyse depending on the history.
   --
   -- So the idea is the following:
   -- A basic tokenizer first splits the document into "tags" (everything
   -- between matching delimiters "<" and ">") and "text" (everything
   -- between two consecutive tags).
   -- Then a tag tokenizer analyses the contents of the tags.
   -----------------------------------------------------------------------

   File: Ada.Text_Io.File_Type;

   Within_Tag: Boolean;
   Tag_End   : exception;
   Unexpected_Whitespace : exception; -- This is mainly used to shut the compiler up

   procedure Initialize (File_Name : Standard.String) is
   begin
      Ada.Text_Io.Open (File => File,
                        Mode => Ada.Text_Io.In_File,
                        Name => File_Name);
      Ada.Text_Io.Set_Input (File);
      Basic.Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;

      Within_Tag := False;
   end Initialize;

   function Name (Token : HTML_Token) return Token_Name is
   begin
      return Token.Name;
   end Name;

   function Lexeme (Token : HTML_Token) return Standard.String is
   begin
      return Ada.Strings.Unbounded.To_String (Token.Lexeme);
   end Lexeme;

   function Next_Tag_Token return HTML_Token is
      use Tags;
   begin
      Tags.Tokenizer.Find_Next (Tags.Analyzer);
      case Tags.Tokenizer.Id (Tags.Analyzer) is
         when Start_Tag_Opener =>
            return (Name   => Start_Tag_Opener,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when End_Tag_Opener =>
            return (Name   => End_Tag_Opener,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Tag_Closer =>
            return (Name   => Tag_Closer,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Meta =>
            return (Name   => Meta,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when HTML =>
            return (Name   => HTML,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Head =>
            return (Name   => Head,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when HTML_Body =>
            return (Name   => HTML_Body,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Anchor =>
            return (Name   => Anchor,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Heading_1 =>
            return (Name   => Heading_1,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Image =>
            return (Name   => Image,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Content =>
            return (Name   => Content,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Hyper_Reference =>
            return (Name   => Hyper_Reference,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Link_Type =>
            return (Name   => Link_Type,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Name =>
            return (Name   => Name,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Title =>
            return (Name   => Title,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Assignment =>
            return (Name   => Assignment,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Value =>
            return (Name   => Value,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Tags.String =>
            return (Name   => String,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Bad_Token =>
            return (Name   => Bad_Token,
                    Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                    (Tags.Tokenizer.Lexeme (Tags.Analyzer)));
         when Whitespace =>  -- not reported
            raise Unexpected_Whitespace;
         when End_Of_Tag =>
            raise Tag_End;
      end case;
   end Next_Tag_Token;

   function Next_Token return HTML_Token is
      -- I would like to return Basic.Analyzer or Tags.Analyzer instead of
      -- doing all this copying, but they do not have a common ancestor.
      -- Perhaps derivingTags.Analyzer from Basic.Analyzer could simplify
      -- the algorithm, which currently involves a lot of copying.
      -- Or we could have only one Analyzer and change its syntax as necessary.
      -- Improvements are welcome.
      use Basic;
   begin
      case Within_Tag is
         when False =>
            Basic.Tokenizer.Find_Next (Basic.Analyzer);
            case Basic.Tokenizer.Id (Basic.Analyzer) is
               when Doctype =>
                  return (Name   => Document_Type,
                          Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                          (Basic.Tokenizer.Lexeme (Basic.Analyzer)));
               when HTML_Tag =>
                  Within_Tag := True;
                  OpenToken.Text_Feeder.String.Set (Value  => Basic.Tokenizer.Lexeme (Basic.Analyzer),
                                                    Feeder => Tags.Tag_Input_Feeder);
                  return Next_Tag_Token;
               when Text =>
                  return (Name   => Text,
                          Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                          (Basic.Tokenizer.Lexeme (Basic.Analyzer)));
               when Entity =>
                  return (Name   => Entity,
                          Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                          (Basic.Tokenizer.Lexeme (Basic.Analyzer)));
               when Comment =>
                  return (Name   => Comment,
                          Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                          (Basic.Tokenizer.Lexeme (Basic.Analyzer)));
               when Bad_Token =>
                  return (Name   => Bad_Token,
                          Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                          (Basic.Tokenizer.Lexeme (Basic.Analyzer)));
               when End_Of_File =>
                  return (Name   => End_Of_File,
                          Lexeme => Ada.Strings.Unbounded.To_Unbounded_String
                          (Basic.Tokenizer.Lexeme (Basic.Analyzer)));
               when Whitespace =>  -- not reported
                  raise Unexpected_Whitespace;
            end case;
         when True =>
            return Next_Tag_Token;
      end case;
   exception
      when Tag_End =>
         Within_Tag := False;
         return Next_Token;
   end Next_Token;

end HTML_Lexer;
