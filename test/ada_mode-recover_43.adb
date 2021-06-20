--  Recover fails with ENQUEUE_LIMIT. However, on a small file
-- containing just the 'if' statement, it finds a solution.
pragma License (Modified_GPL);

package body WisiToken.Parse is

   procedure Edit_Tree
   is
   begin

                  if Terminal_Byte_Region.First + Shift_Bytes <= Inserted_Region.First then
                     null;
                  else
                     declare
                        procedure Handle_Non_Grammar (Non_Grammar : in out WisiToken.Lexer.Token_Arrays.Vector)
                        is
                        begin
                           if Non_Grammar.Length = 0 then
                              null;
                           else
                              if Delete > 0 and then Non_Grammar (Delete).ID = Tree.Lexer.Descriptor.SOI_ID then
                                 if Delete = Non_Grammar.Last_Index then
                                    Delete = 0; -- ERROR here
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
end WisiToken.Parse;
