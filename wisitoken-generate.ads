--  Abstract :
--
--  Types and operations for generating parsers, common to all parser
--  types.
--
--  The wisi* packages deal with reading *.wy files and generating
--  source code files. The wisitoken-generate* packages deal with
--  computing parser properties from the grammar. (For historical
--  reasons, not all packages follow this naming convention yet).
--
--  References :
--
--  See wisitoken.ads
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

with WisiToken.Productions;
package WisiToken.Generate is

   Error : Boolean := False;
   --  Set True by errors during grammar generation

   function Error_Message
     (File_Name : in String;
      File_Line : in WisiToken.Line_Number_Type;
      Message   : in String)
     return String;

   procedure Put_Error (Message : in String);
   --  Set Error True, output Message to Standard_Error

   procedure Check_Consistent
     (Grammar          : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor       : in WisiToken.Descriptor'Class;
      Source_File_Name : in String);

   function Has_Empty_Production (Grammar : in WisiToken.Productions.Prod_Arrays.Vector) return Token_ID_Set;
   --  Result (ID) is True if any production for ID can be an empty
   --  production, recursively.

   function First
     (Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in Token_ID_Set;
      First_Terminal       : in Token_ID)
     return Token_Array_Token_Set;
   --  For each nonterminal in Grammar, find the set of tokens
   --  (terminal or nonterminal) that any string derived from it can
   --  start with. Together with Has_Empty_Production, implements
   --  algorithm FIRST from [dragon], augmented with nonterminals.

end WisiToken.Generate;
