--  Used to get "branched tree growing too large".
--EMACSCMD:(setq skip-recase-test t)
package WisiToken.Syntax_Trees is
   procedure Buffer (Foo : in Bar);

   procedure Update
     --  Missing ';' here. Error recovery finds two solutions; (insert ';'), (insert 'is')
     --  Both continue parsing with no errors until end of file.
     --  FIXME: Incremental parse error recover should keep only ';' solution
     --
     --  Bad indent caused by comment heuristic.

     function ID
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Token_ID
   with Pre => Tree.Contains (Stream, Element) or Tree.Contains (Tree.Terminal_Stream, Element);
   --  For example, Parser.Current_Token is either a Shared_Terminal from
   --  Terminal_Stream or a Virtual_Terminal on Stream from error
   --  recovery; in incremental parse, it could be a Shared_Terminal
   --  Stream from breakdown.

   function ID (Tree : in Syntax_Trees.Tree; Element : in Stream_Index) return Token_ID
   with Pre => Tree.Contains (Tree.Terminal_Stream, Element);

   function Label (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Label;
   function Label (Tree : in Syntax_Trees.Tree; Element : in Stream_Index) return Node_Label
   with Pre => Element /= Invalid_Stream_Index;

   function Label (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return Node_Label
   with Pre => Tree.Is_Valid (Stream);
   --  Label of Stream.Last; top of stack

   function Child_Count (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return SAL.Base_Peek_Type
   with Pre => Tree.Is_Nonterm (Node);

   function Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access_Array
   with Pre => Tree.Is_Nonterm (Node);
   --  Any children that were deleted by tree editing are returned as
   --  Invalid_Node_Access.

   function Child
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Valid_Node_Access;
      Child_Index : in Positive_Index_Type)
     return Node_Access
   with Pre => Tree.Is_Nonterm (Node);

   function Has_Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   with Pre => Tree.Is_Nonterm (Node);

   function Has_Child
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Child : in Valid_Node_Access)
     return Boolean
   with Pre => Tree.Is_Nonterm (Node);

   function Has_Parent (Tree : in Syntax_Trees.Tree; Child : in Valid_Node_Access) return Boolean;

   function Buffer_Region_Is_Empty (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   --  True if contained buffer region is empty; always the case for
   --  virtual tokens, and for most copied tokens. Use Has_Children or
   --  Child_Count to see if Node has children.

   function Is_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   function Is_Shared_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   function Is_Virtual_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;

   function Is_Virtual
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Boolean
   with Pre => Tree.Contains (Stream, Element) or Tree.Contains (Tree.Terminal_Stream, Element);
   function Is_Virtual (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   --  Virtual_Terminal, Virtual_Identifier, or Nonterm that contains some Virtual tokens.

   function Is_Virtual_Identifier (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   function Traversing (Tree : in Syntax_Trees.Tree) return Boolean;

   procedure Set_Name_Region
     (Tree   : in out Syntax_Trees.Tree;
      Node   : in     Valid_Node_Access;
      Region : in     Buffer_Region)
   with Pre => Tree.Is_Nonterm (Node);

   function ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return WisiToken.Token_ID;

   function Production_ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return WisiToken.Production_ID
   with Pre => Tree.Is_Nonterm (Node);

   function Byte_Region
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return WisiToken.Buffer_Region;

   function RHS_Index
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Natural
   with Pre => Tree.Is_Nonterm (Node);

   function Get_Recover_Token
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Recover_Token
   with Pre => Tree.Contains (Stream, Element);

   function Get_Recover_Token
     (Tree    : in Syntax_Trees.Tree;
      Element : in Stream_Index)
     return Recover_Token
     is (Tree.Get_Recover_Token (Tree.Terminal_Stream, Element))
   with Pre => Tree.Contains (Tree.Terminal_Stream, Element);

   function Get_Recover_Token
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Recover_Token;

end WisiToken.Syntax_Trees;
