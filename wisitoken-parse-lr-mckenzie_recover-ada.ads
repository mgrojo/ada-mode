--  Abstract :
--
--  Ada language specific algorithms for McKenzie_Recover
--
--  Copyright (C) 2018 - 2021 Free Software Foundation, Inc.
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

package WisiToken.Parse.LR.McKenzie_Recover.Ada is

   procedure Language_Fixes
     (Super             : not null access WisiToken.Parse.LR.McKenzie_Recover.Base.Supervisor;
      Parser_Index      : in              SAL.Peek_Type;
      Parse_Table       : in     WisiToken.Parse.LR.Parse_Table;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration);
   --  See wisitoken-parse-lr-parser.ads Language_Fixes_Access for description.

   procedure Matching_Begin_Tokens
     (Tree                    : in     Syntax_Trees.Tree;
      Tokens                  : in     Token_ID_Array_1_3;
      Config                  : in     Configuration;
      Matching_Tokens         :    out Token_ID_Arrays.Vector;
      Forbid_Minimal_Complete :    out Boolean);
   --  See wisitoken-parse-lr-parser.ads Language_Matching_Begin_Tokens_Access
   --  for description.

   function String_ID_Set
     (Descriptor        : in WisiToken.Descriptor;
      String_Literal_ID : in Token_ID)
     return Token_ID_Set;
   --  See wisitoken-parse-lr-parser.ads Language_String_ID_Set_Access for
   --  description.

end WisiToken.Parse.LR.McKenzie_Recover.Ada;
