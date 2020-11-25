--  Used to get Fatal_Error: wisi-name-action: name set twice. Now fixed.

function Child_Index
  (Tree      : in out Syntax_Trees.Tree;
   Parent    : in     Valid_Node_Index;
   Old_Child : in     Valid_Node_Index)
  return Sal.Peek_Type
is
   N : Node_Var_Ref renames Get_Node_Var_Ref (Tree, Parent);
   begin
      for I in N.Children.First_Index .. N.Children.Last_Index loop
         if N.Children (I) = Child then
            return I;
         end if;
      end loop;
      raise Sal.Programmer_Error; -- Should be prevented by precondition on caller.
   end Find_Child_Index;


end Child_Index;
-- Local Variables:
-- wisi-partial-parse-threshold: 0
-- End:
