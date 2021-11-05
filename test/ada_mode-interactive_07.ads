--  Editing a comment used to cause syntax errors in following declaration.

package Ada_Mode.Interactive_07 is

   --EMACSCMD:(progn (forward-line 1)(search-forward "--  1. ")(execute-kbd-macro "foo bar")(indent-new-comment-line))
   --EMACSCMD:(progn (undo 3)(indent-for-tab-command))
   --EMACSCMD:(length (wisi-parser-parse-errors wisi--parser))
   --EMACSRESULT:0
   function Find_New_Line
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type)
     return Node_Access
   with Pre => Tree.Editable;
   --  Return the terminal node containing a non_grammar that ends Line -
   --  1. Result is Invalid_Node_Access if Line is outside range spanned
   --  by Tree.

   procedure Find_New_Line;
   --  Same as Find_New_Line, also updates Line_Begin_Char_Pos to first
   --  char pos on Line.

end Ada_Mode.Interactive_07;
