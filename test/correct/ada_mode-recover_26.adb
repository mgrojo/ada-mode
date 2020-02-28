package body Ada_Mode.Recover_26 is
   procedure Create_Tree_Node
   is begin
      if W_Token_Id = No_Direct_Map then
         null;
      else
         declare
            procedure Find_Terminal
            is begin
               loop
                  Index := Index - 1;
                  if Id = Terminals (Index) then
                     return Index;
                  else
                  end if;
               end loop;
            end Find_Terminal;

         begin
            Find_Production;
         end;
      end if;
   end Create_Tree_Node;

end Ada_Mode.Recover_26;
