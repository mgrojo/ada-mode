pragma License (Modified_GPL);

package WisiToken.Parse.LR.McKenzie_Recover.Java_Expressions_Antlr is

   procedure Fixes
     (Trace             : in out WisiToken.Trace'Class;
      Lexer             : access constant WisiToken.Lexer.Instance'Class;
      Parser_Label      : in     Syntax_Trees.Stream_ID;
      Parse_Table       : in     WisiToken.Parse.LR.Parse_Table;
      Tree              : in     Syntax_Trees.Tree;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration)
   is null;

   procedure Matching_Begin_Tokens
     (Tokens                   : in     Token_ID_Array_1_3;
      Config                   : in     Configuration;
      Matching_Begin_Tokens    :    out Token_ID_Arrays.Vector;
      Forbid_Matching_Complete :    out Boolean);
   --  See wisitoken-parse-lr-parser.ads Language_Matching_Begin_Tokens_Access
   --  for description.

   function String_ID_Set
     (Descriptor        : in WisiToken.Descriptor;
      String_Literal_ID : in Token_ID)
     return Token_ID_Set;

end WisiToken.Parse.LR.McKenzie_Recover.Java_Expressions_Antlr;
