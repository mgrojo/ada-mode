--  Abstract :
--
--  WisiToken wrappers for the Libadalang lexer and parser for use
--  with the Wisi indentation engine.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Strings.Unbounded;
with Libadalang.Analysis;
with Libadalang.Lexer;
with WisiToken.Lexer;
with WisiToken.Parse;
with WisiToken.Syntax_Trees;
package Wisi.Libadalang is

   type Lexer is new WisiToken.Lexer.Instance with record
     TDH : access constant Standard.Libadalang.Lexer.Token_Data_Handlers.Token_Data_Handler;
   end record;
   overriding procedure Reset_With_String (Lexer : in out Wisi.Libadalang.Lexer; Input : in String);
   overriding
   procedure Reset_With_String_Access
     (Lexer     : in out Wisi.Libadalang.Lexer;
      Input     : in     Ada.Strings.Unbounded.String_Access;
      File_Name : in     Ada.Strings.Unbounded.Unbounded_String);
   overriding procedure Reset_With_File (Lexer : in out Wisi.Libadalang.Lexer; File_Name : in String);
   overriding procedure Reset (Lexer : in out Wisi.Libadalang.Lexer);
   overriding procedure Discard_Rest_Of_Input (Lexer : in out Wisi.Libadalang.Lexer);
   overriding
   function Buffer_Text
     (Lexer       : in Wisi.Libadalang.Lexer;
      Byte_Region : in WisiToken.Buffer_Region)
     return String;
   overriding function First (Lexer : in Wisi.Libadalang.Lexer) return Boolean
     is (raise SAL.Not_Implemented);
   overriding function Find_Next
     (Lexer : in out Wisi.Libadalang.Lexer;
      Token :    out WisiToken.Base_Token)
     return Boolean
     is (raise SAL.Not_Implemented);
   overriding function File_Name (Lexer : in Wisi.Libadalang.Lexer) return String
     is (raise SAL.Not_Implemented);

   type Parser is new WisiToken.Parse.Base_Parser with record
      Source_File_Name : Ada.Strings.Unbounded.Unbounded_String;

      Unit : aliased Standard.Libadalang.Analysis.Analysis_Unit;
      --  FIXME: Errors : ?

      Base_Tree : aliased WisiToken.Syntax_Trees.Base_Tree;
      Tree      : WisiToken.Syntax_Trees.Tree;
   end record;

   overriding procedure Parse (Parser : aliased in out Wisi.Libadalang.Parser);
   overriding function Any_Errors (Parser : in Wisi.Libadalang.Parser) return Boolean;
   overriding procedure Put_Errors (Parser : in Wisi.Libadalang.Parser);
   overriding procedure Execute_Actions (Parser : in out Wisi.Libadalang.Parser);

end Wisi.Libadalang;
