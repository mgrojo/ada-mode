--  Abstract :
--
--  Grammar semantic check routines.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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

package WisiToken.LR.Semantic_Checks is

   function Match_Names
     (Lexer        : in WisiToken.Lexer.Handle;
      Tokens       : in Base_Token_Arrays.Vector;
      Start_Index  : in Positive_Index_Type;
      End_Index    : in Positive_Index_Type;
      End_Optional : in Boolean)
     return Semantic_Status;
   --  If buffer text at Tokens (Start_Index).Name equals buffer text at
   --  Tokens (End_Index).Name, or both are Null_Buffer_Retion, return
   --  Ok. Otherwise return Error.

   function Propagate_Name
     (Nonterm    : in out Base_Token;
      Tokens     : in     Base_Token_Arrays.Vector;
      Name_Index : in     Positive_Index_Type)
     return Semantic_Status;
   --  Set Nonterm.Name to Tokens (Name_Index).Name, return Ok.

   function Merge_Names
     (Nonterm     : in out Base_Token;
      Tokens      : in     Base_Token_Arrays.Vector;
      First_Index : in     Positive_Index_Type;
      Last_Index  : in     Positive_Index_Type)
     return Semantic_Status;
   --  Then set Nonterm.Name to the merger of Tokens (First_Index ..
   --  Last_Index).Name, return Ok.
   --
   --  If Tokens (Last_Index).Name is Null_Buffer_Region, use Tokens
   --  (Last_Index).Byte_Region instead.

end WisiToken.LR.Semantic_Checks;
