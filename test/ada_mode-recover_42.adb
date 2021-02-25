--  Used to get error in resume, now fixed.

package body WisiToken.Syntax_Trees is

   function Line_Begin_Token return Node_Access
   is
   begin
            Find_New_Line;
      end;
   end Line_Begin_Token;

end WisiToken.Syntax_Trees;
