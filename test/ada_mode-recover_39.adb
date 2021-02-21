--  From a real editing session; LALR gets FAIL_ENQUEUE_LIMIT with limit at 58_000; LR1 does not
--EMACSCMD:(switch-to-lr1)
procedure Edit_Tree
  (Parser : in Base_Parser;
   Edits  : in Kmn_Lists.List)
is
   --  Similar to [Lahav 2004] Algorithms 3, 4.
   use Kmn_Lists;
   Kmn_Node     : Cursor          := Edits.First;
   Old_Byte_Pos : Buffer_Pos      := 1;
   Shift_Bytes  : Base_Buffer_Pos := 0;
   Shift_Chars  : Buffer_Pos      := 0;
   Scan_Pos     : Buffer_Pos      := 1;

   Terminal_Node : Syntax_Trees.Stream_Index := Parser.Tree.First_Shared_Terminal (Tree.Terminal_Stream);
begin
Kmn_Loop :
   loop
      declare
         Stable_Region : constant Buffer_Region :=
           (Old_Byte_Pos, Old_Byte_Pos + Constant_Ref (Kmn_Node).Stable_Bytes);
      begin
      Unchanged_Loop :
         loop
            declare
               Node_Byte_Region : Buffer_Region_Ref renames Tree.Byte_Region_Ref (Terminal_Node);
               Node_Char_Region : Buffer_Region_Ref renames Tree.Char_Region_Ref (Terminal_Node);
            begin
               exit when not Node_Byte_Region in Stable_Region;
               Node_Byte_Regin := @ + Shift_Bytes;
               , Char_Offset => Shift_Chars);

                     Terminal_Node := Next (Terminal_Node);
         end loop Unchanged_Loop;

         Kmn_Node := Next (Kmn_Node);
         exit Kmn_Loop when not Has_Element (Kmn_Node);
   end loop Kmn_Loop;
end Edit_Tree;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- End:
