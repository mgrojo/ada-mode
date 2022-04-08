-- Used to get assertion error in Edit_Tree Breakdown

--EMACS_SKIP_UNLESS: wisi-incremental-parse-enable
--EMACSCMD:(setq skip-reindent-test t)
--EMACSCMD:(wisi-process-parse-compare-tree-text wisi--parser :disable t) ;; only incremental creates optimized_list
package body WisiToken.Syntax_Trees is

   function Subtree_Image
     (Node_Numbers : in Boolean := True;
      --EMACSCMD:(progn (forward-line 1)(indent-rigidly (point)(line-beginning-position 2) -1)(wisi-parse-incremental-none))
      Non_Grammar  : in Boolean := False;
      Augmented    : in Boolean := False)
     return String;


   function Find_Ancestor
     return Node_Access
   is
   begin
      return N;
   end Find_Ancestor;

   function Find_Byte_Pos
     return Node_Access
   is begin
            return Node;
   end Find_Byte_Pos;

end WisiToken.Syntax_Trees;
