pragma License (Modified_GPL);

package WisiToken.Parse.LR.McKenzie_Recover.Java_Enum_Ch19 is

   procedure Fixes
     (Trace             : in out WisiToken.Trace'Class;
      Lexer             : access constant WisiToken.Lexer.Instance'Class;
      Parser_Label      : in     Natural;
      Parse_Table       : in     WisiToken.Parse.LR.Parse_Table;
      Terminals         : in     Base_Token_Arrays.Vector;
      Tree              : in     Syntax_Trees.Tree;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration)
   is null;

   procedure Use_Minimal_Complete_Actions
     (Tokens               : in     Token_ID_Array_1_3;
      Config               : in     Configuration;
      Use_Complete         :    out Boolean;
      Matching_Begin_Token :    out Token_ID_Arrays.Vector);
   --  See wisitoken-parse-lr-parser.ads Language_Use_Minimal_Complete_Actions_Access
   --  for description.

   function String_ID_Set
     (Descriptor        : in WisiToken.Descriptor;
      String_Literal_ID : in Token_ID)
     return Token_ID_Set;

end WisiToken.Parse.LR.McKenzie_Recover.Java_Enum_Ch19;
