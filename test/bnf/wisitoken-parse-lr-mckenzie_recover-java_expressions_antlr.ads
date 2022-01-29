pragma License (Modified_GPL);

package WisiToken.Parse.LR.McKenzie_Recover.Java_Expressions_Antlr is

   procedure Fixes
     (Super             : in out WisiToken.Parse.LR.McKenzie_Recover.Base.Supervisor;
      Shared_Parser     : in out Parser.Parser;
      Parser_Index      : in              SAL.Peek_Type;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration)
   is null;

   procedure Matching_Begin_Tokens
     (Tree                     : in     Syntax_Trees.Tree;
      Tokens                   : in     Token_ID_Array_1_3;
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
