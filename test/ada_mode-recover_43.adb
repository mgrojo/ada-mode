--  Recover fails with ENQUEUE_LIMIT if mckenzie_task_count = 8.
pragma License (Modified_Gpl);

package body Wisitoken.Parse is

   procedure Edit_Tree
   is
   begin

      if Terminal_Byte_Region.First + Shift_Bytes <= Inserted_Region.First then
         null;
      else
         declare
            procedure Handle_Non_Grammar (Non_Grammar : in out Wisitoken.Lexer.Token_Arrays.Vector)
            is
            begin
               if Non_Grammar.Length = 0 then
                  null;
               else
                  if Delete > 0 and then Non_Grammar (Delete).Id = Tree.Lexer.Descriptor.Soi_Id then
                     if Delete = Non_Grammar.Last_Index then
                        Delete = 0; -- ERROR here; '=' should be ':='
                     else
                        Delete := Delete + 1;
                     end if;
                  end if;
               end if;
            end Handle_Non_Grammar;
         begin
            null;
         end;
      end if;
   end Edit_Tree;
end Wisitoken.Parse;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- End:
