--  Abstract :
--
--  See spec.
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

package body WisiToken.LR.Semantic_Checks is

   function Match_Names
     (Lexer        : in WisiToken.Lexer.Handle;
      Tokens       : in Base_Token_Arrays.Vector;
      Start_Index  : in Positive_Index_Type;
      End_Index    : in Positive_Index_Type;
      End_Optional : in Boolean)
     return Semantic_Status
   is begin
      if End_Optional then
         if Tokens (End_Index).Name = Null_Buffer_Region then
            return Ok;
         elsif Tokens (Start_Index).Name = Null_Buffer_Region then
            return Error;
         else
            if Lexer.Buffer_Text (Tokens (Start_Index).Name) = Lexer.Buffer_Text (Tokens (End_Index).Name) then
               return Ok;
            else
               return Error;
            end if;
         end if;
      else
         if Tokens (Start_Index).Name = Null_Buffer_Region then
            if Tokens (End_Index).Name = Null_Buffer_Region then
               return Ok;
            else
               return Error;
            end if;

         elsif Tokens (End_Index).Name = Null_Buffer_Region then
            return Error;

         else
            if Lexer.Buffer_Text (Tokens (Start_Index).Name) = Lexer.Buffer_Text (Tokens (End_Index).Name) then
               return Ok;
            else
               return Error;
            end if;
         end if;
      end if;
   end Match_Names;

end WisiToken.LR.Semantic_Checks;
