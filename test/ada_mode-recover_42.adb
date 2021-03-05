--  Used to get error in resume, now fixed.

package body Wisitoken.Syntax_Trees is

   function Line_Begin_Token return Node_Access
   is
   begin
      Find_New_Line;
      end;
   end Line_Begin_Token;

end Wisitoken.Syntax_Trees;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- ada-end-name-optional: nil
-- End:
