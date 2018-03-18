--  Abstract :
--
--  Grammar semantic check routines.
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
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

with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
package WisiToken.Semantic_Checks is

   type Check_Status_Label is (Ok, Error);

   type Error_Code is
     (Missing_Name_Error, -- block start has name, required block end name missing
      Extra_Name_Error,   -- block start has no name, end has one
      Match_Names_Error); -- both names present, but don't match

   type Check_Status (Label : Check_Status_Label := Check_Status_Label'First) is record
      case Label is
      when Ok =>
         null;

      when Error =>
         Code : Error_Code;

         Tokens : Recover_Token_Arrays.Vector;
         --  The tokens involved in the error; for example, for
         --  Match_Names_Error, the two name tokens.
      end case;

   end record;

   function Image (Item : in Check_Status) return String;

   type Semantic_Check is access function
     (Lexer   : in     WisiToken.Lexer.Handle;
      Nonterm : in out Recover_Token;
      Tokens  : in     Recover_Token_Array)
     return Check_Status;
   --  Called during parsing and error recovery to implement higher level
   --  checks, such as block name matching in Ada.

   Null_Check : constant Semantic_Check := null;

   function Match_Names
     (Lexer        : in WisiToken.Lexer.Handle;
      Tokens       : in Recover_Token_Array;
      Start_Index  : in Positive_Index_Type;
      End_Index    : in Positive_Index_Type;
      End_Optional : in Boolean)
     return Check_Status;
   --  Check that buffer text at Tokens (Start_Index).Name matches buffer
   --  text at Tokens (End_Index).Name.

   function Propagate_Name
     (Nonterm    : in out Recover_Token;
      Tokens     : in     Recover_Token_Array;
      Name_Index : in     Positive_Index_Type)
     return Check_Status;
   --  Set Nonterm.Name to Tokens (Name_Index).Name, or .Byte_Region, if
   --  .Name is Null_Buffer_Region. Return Ok.

   function Merge_Names
     (Nonterm     : in out Recover_Token;
      Tokens      : in     Recover_Token_Array;
      First_Index : in     Positive_Index_Type;
      Last_Index  : in     Positive_Index_Type)
     return Check_Status;
   --  Then set Nonterm.Name to the merger of Tokens (First_Index ..
   --  Last_Index).Name, return Ok.
   --
   --  If Tokens (Last_Index).Name is Null_Buffer_Region, use Tokens
   --  (Last_Index).Byte_Region instead.

end WisiToken.Semantic_Checks;
