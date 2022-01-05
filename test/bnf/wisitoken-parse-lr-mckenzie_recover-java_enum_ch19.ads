pragma License (Modified_GPL);

package WisiToken.Parse.LR.McKenzie_Recover.Java_Enum_Ch19 is

   procedure Fixes
     (Super             : not null access WisiToken.Parse.LR.McKenzie_Recover.Base.Supervisor;
      Parser_Index      : in              SAL.Peek_Type;
      Parse_Table       : in     WisiToken.Parse.LR.Parse_Table;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration)
   is null;

   procedure Matching_Begin_Tokens
     (Super                   :         not null access WisiToken.Parse.LR.McKenzie_Recover.Base.Supervisor;
      Tokens                  :         in              Token_ID_Array_1_3;
      Config                  : aliased in              Configuration;
      Matching_Begin_Tokens   :            out          Token_ID_Arrays.Vector;
      Forbid_Minimal_Complete :            out          Boolean);
   --  See wisitoken-parse-lr-parser.ads Language_Matching_Begin_Tokens_Access
   --  for description.

   function String_ID_Set
     (Descriptor        : in WisiToken.Descriptor;
      String_Literal_ID : in Token_ID)
     return Token_ID_Set;

end WisiToken.Parse.LR.McKenzie_Recover.Java_Enum_Ch19;
